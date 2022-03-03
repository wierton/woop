#include "common.h"
#include "diff_top.h"

#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <unistd.h>

#include <thread>

/* clang-format off */
#define GPRS(X) \
  X(0)  X(1)  X(2)  X(3)  X(4)  X(5)  X(6)  X(7)  \
  X(8)  X(9)  X(10) X(11) X(12) X(13) X(14) X(15) \
  X(16) X(17) X(18) X(19) X(20) X(21) X(22) X(23) \
  X(24) X(25) X(26) X(27) X(28) X(29) X(30) X(31)
/* clang-format on */

class ProgIOs {
  std::istream *_ins = nullptr;
  std::ostream *_outs = nullptr;
  std::ostream *_errs = nullptr;

public:
  ProgIOs() = default;
  ProgIOs(std::istream &iss) : _ins(&iss) {}
  ProgIOs(std::ostream &oss) : _outs(&oss) {}
  ProgIOs(std::istream &iss, std::ostream &oss)
      : _ins(&iss), _outs(&oss) {}
  ProgIOs(std::istream &iss, std::ostream &oss,
      std::ostream &ess)
      : _ins(&iss), _outs(&oss), _errs(&ess) {}

  void setInput(std::istream &iss) { _ins = &iss; }
  void setOutput(std::ostream &oss) { _outs = &oss; }
  void setError(std::ostream &ess) { _errs = &ess; }

  std::istream &ins() { return *_ins; }
  std::ostream &outs() { return *_outs; }
  std::ostream &errs() { return *_errs; }

  bool hasInput() const { return _ins; }
  bool hasOutput() const { return _outs; }
  bool hasError() const { return _errs; }
};

uint64_t getMilliseconds() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (tv.tv_usec + tv.tv_sec * 1000000) / 1000;
}

int execute(ProgIOs io, const std::string &prog,
    const std::vector<std::string> &args,
    unsigned timeout) {
  std::string output;

  const char **uArgs = new const char *[args.size() + 1];
  for (auto i = 0u; i < args.size(); i++) {
    uArgs[i] = args[i].c_str();
  }
  uArgs[args.size()] = nullptr;

  int infds[2], outfds[2], errfds[2];
  if (io.hasInput()) pipe(infds);
  if (io.hasOutput()) pipe(outfds);
  if (io.hasError()) pipe(errfds);

  const int R = 0, W = 1;

  int pid = fork();
  if (pid == 0) {
    /* clang-format off */
    if (io.hasInput()) { dup2(infds[R], 0); close(infds[R]); close(infds[W]); }
    if (io.hasOutput()) { dup2(outfds[W], 1); close(outfds[R]); close(outfds[W]); }
    if (io.hasError()) { dup2(errfds[W], 2); close(errfds[R]); close(errfds[W]); }
    /* clang-format on */

    execve(prog.c_str(), (char *const *)uArgs, environ);
    exit(-1);
  } else {
    if (io.hasInput()) close(infds[R]);
    if (io.hasOutput()) close(outfds[W]);
    if (io.hasError()) close(errfds[W]);
    delete[] uArgs;
  }

  auto process_output_or_error = [](int fd,
                                     std::ostream &os) {
    int nbytes = 0;
    std::string buffer;
    ioctl(fd, FIONREAD, &nbytes);
    buffer.resize(nbytes);
    int len = read(fd, &buffer[0], nbytes);
    if (len > 0) os.write(&buffer[0], len);
    return len;
  };

  int status = -1;
  uint64_t st = getMilliseconds();

  std::thread inpoll;
  if (io.hasInput()) {
    inpoll = std::thread([&io, pid, infds]() {
      /* process input */
      std::string buffer;
      int nbytes = 1024;
      io.ins().peek();
      buffer.resize(nbytes);
      while (io.hasInput() && io.ins().good()) {
        io.ins().read(&buffer[0], nbytes);
        int len =
            write(infds[W], &buffer[0], io.ins().gcount());
        if (len < 0) break;
        io.ins().peek();

        int status = -1;
        waitpid(pid, &status, WNOHANG);
        if (WIFEXITED(status) || WIFSIGNALED(status)) break;
      }

      close(infds[W]);
    });
  }

  while (1) {
    if (io.hasOutput())
      process_output_or_error(outfds[R], io.outs());
    if (io.hasError())
      process_output_or_error(errfds[R], io.errs());

    waitpid(pid, &status, WNOHANG);

    if (WIFEXITED(status) || WIFSIGNALED(status)) { break; }
    if (getMilliseconds() > st + timeout) {
      kill(pid, SIGKILL);
    }
  }

  if (io.hasInput())
    inpoll.join();
  else
    close(infds[W]);

  if (io.hasOutput())
    process_output_or_error(outfds[R], io.outs());
  if (io.hasError())
    process_output_or_error(errfds[R], io.errs());

  if (io.hasOutput()) close(outfds[R]);
  if (io.hasError()) close(errfds[R]);

  if (WIFEXITED(status)) return WEXITSTATUS(status);
  return 139;
}

bool DiffTop::check_states() {
  mips_instr_t instr = napi_get_instr();
  if (instr.is_syscall() || instr.is_eret()) return true;

#define check_eq(a, b, ...)        \
  if ((a) != (b)) {                \
    printf(__VA_ARGS__);           \
    updateNoopState(NOOP_CHKFAIL); \
    return false;                  \
  }

  check_eq(napi_get_pc(), dut_ptr->io_commit_pc,
      "cycle %lu: pc: nemu:%08x <> dut:%08x\n", noop_cycles,
      napi_get_pc(), dut_ptr->io_commit_pc);
  check_eq(napi_get_instr(), dut_ptr->io_commit_instr,
      "cycle %lu: instr: nemu:%08x <> dut:%08x\n",
      noop_cycles, napi_get_instr(),
      dut_ptr->io_commit_instr);

#define GPR_TEST(i)                                     \
  check_eq(napi_get_gpr(i), dut_ptr->io_commit_gpr_##i, \
      "cycle %lu: gpr[%d]: nemu:%08x <> dut:%08x\n",    \
      noop_cycles, i, napi_get_gpr(i),                  \
      dut_ptr->io_commit_gpr_##i);
  GPRS(GPR_TEST);
#undef GPR_TEST
  return true;
}

uint32_t DiffTop::get_dut_gpr(uint32_t r) {
  switch (r) {
#define GET_GPR(i) \
  case i: return dut_ptr->io_commit_gpr_##i;
    GPRS(GET_GPR);
#undef GET_GPR
  }
  return 0;
}

void DiffTop::dump_regs_pre(std::ostream &os) {
  os << "[\n";
}
void DiffTop::dump_regs_post(std::ostream &os) {
  os << "\n]\n";
}
void DiffTop::dump_ulite_pre(std::ostream &os) {
  os << "[\n";
}
void DiffTop::dump_ulite_post(std::ostream &os) {
  os << "\n]\n";
}

void DiffTop::dump_nemu_regs_single(std::ostream &os) {
  os << "[";
  os << "0x" << std::hex << napi_get_pc() << ", ";
  os << "0x" << std::hex << napi_get_instr() << ", ";
  for (int i = 0; i < 31; i++)
    os << "0x" << std::hex << napi_get_gpr(i) << ", ";
  os << "0x" << std::hex << napi_get_gpr(31) << "]";
}

void DiffTop::dump_noop_regs_single(std::ostream &os) {
  static bool isFirstTime = true;
  if (noop_state == NOOP_CHKFAIL) {
    if (!isFirstTime) {
      os << "[]";
      return;
    }
    isFirstTime = false;
  }

  os << "[";
  os << "0x" << std::hex << dut_ptr->io_commit_pc << ", ";
  os << "0x" << std::hex << dut_ptr->io_commit_instr
     << ", ";
  for (int i = 0; i < 31; i++)
    os << "0x" << std::hex << get_dut_gpr(i) << ", ";
  os << "0x" << std::hex << get_dut_gpr(31) << "]";
}

void DiffTop::dump_regs_single(bool chkflag) {
  static bool isFirstTime = true;
  if (!isFirstTime) regs_fs << ",\n";
  isFirstTime = false;

  regs_fs << "  (";
  regs_fs << std::dec << noop_cycles << ", "
          << (chkflag ? "true" : "false") << ", ";
  dump_noop_regs_single(regs_fs);
  regs_fs << ", ";
  dump_nemu_regs_single(regs_fs);
  regs_fs << ")";
}

void DiffTop::dump_registers() {
  static bool isFirstChkfail = true;
  if (isFirstChkfail && noop_state == NOOP_CHKFAIL) {
    dump_regs_single(noop_state != NOOP_CHKFAIL);
    isFirstChkfail = false;
  } else {
    switch (elf_type) {
    case ELF_VMLINUX: {
      if (noop_ninstr % 3000 == 0 ||
          (noop_end_ninstr - 400 < noop_ninstr &&
              noop_ninstr < noop_end_ninstr + 20)) {
        dump_regs_single(noop_state != NOOP_CHKFAIL);
      }
    } break;
    default:
      if (noop_ninstr % ((noop_end_ninstr / 400) + 1) == 0 ||
          (noop_end_ninstr - 400 < noop_ninstr &&
              noop_ninstr < noop_end_ninstr + 20))
        dump_regs_single(noop_state != NOOP_CHKFAIL);
      break;
    }
  }
}

bool DiffTop::can_log_now() const {
  switch (elf_type) {
  case ELF_VMLINUX:
  case ELF_CACHE_FLUSH:
  case ELF_MICROBENCH:
    return (noop_end_ninstr - 200 < noop_ninstr &&
            noop_ninstr < noop_end_ninstr + 20);
  case ELF_OTHER: return true;
  }
  return true;
}

void DiffTop::dump_nemu_ulite_single(int data) {
  static bool isFirstTime = true;
  if (!isFirstTime) nemu_ulite_fs << ",\n";
  isFirstTime = false;
  nemu_ulite_fs << "  (" << (this->noop_cycles - 1) << ", '"
                << escape((char)data) << "')";
}

void DiffTop::dump_noop_ulite_single(int data) {
  static bool isFirstTime = true;
  if (!isFirstTime) noop_ulite_fs << ",\n";
  isFirstTime = false;
  noop_ulite_fs << "  (" << this->noop_cycles << ", '"
                << escape((char)data) << "')";
}

DiffTop::~DiffTop() {
  dump_regs_post(regs_fs);
  dump_ulite_post(nemu_ulite_fs);
  dump_ulite_post(noop_ulite_fs);
}

void DiffTop::init_elf_file_type(const char *elf) {
  if (string_contains(elf, "linux")) {
    elf_type = ELF_VMLINUX;
  } else if (string_contains(elf, "microbench")) {
    elf_type = ELF_MICROBENCH;
  } else if (string_contains(elf, "SimpleOS")) {
    elf_type = ELF_SIMPLEOS;
  } else if (string_contains(elf, "cache-flush")) {
    elf_type = ELF_CACHE_FLUSH;
  } else {
    elf_type = ELF_OTHER;
  }
}

void DiffTop::init_stop_condition() {
  switch (elf_type) {
  case ELF_SIMPLEOS:
    if (noop_enable_bug) {
      if (noop_enable_diff) {
      } else {
        stop_noop_when_ulite_send("0x01fe0000");
      }
    } else {
      stop_noop_when_ulite_send(
          "starting process 0 now...");
    }
    napi_stop_cpu_when_ulite_send(
        "starting process 0 now...");
    break;
  case ELF_VMLINUX:
    if (noop_enable_bug) {
      if (noop_enable_diff) {
        noop_end_ninstr = 29949901;
      } else {
        noop_end_ninstr = 34909125;
        stop_noop_when_ulite_send(
            "kill the idle task! ]---");
      }
    } else {
      noop_end_ninstr = 33258878;
      stop_noop_when_ulite_send("activate this console.");
    }
    napi_stop_cpu_when_ulite_send("activate this console.");
    break;
  default: {
    std::ostringstream oss;
    ProgIOs io(oss);
    std::vector<std::string> args{"nemu", "--print-ninstrs",
        "-b", "-e", elf_filename};
    ::execute(io, NEMU_BIN, args, 10000);

    std::istringstream iss(oss.str());
    while ((iss.peek(), iss.good())) {
      std::string line;
      std::getline(iss, line);
      if (string_contains(line, "NEMU: ")) {
        noop_end_ninstr =
            std::stoll(line.substr(6, line.size() - 6));
        break;
      }
    }
  } break;
  }
  printf("[noop, set noop_end_ninstr to %lu]\n",
      noop_end_ninstr);
}

void DiffTop::init_from_args(int argc, const char *argv[]) {
  const char *napi_args[4] = {"nemu", "-b", "-e", nullptr};

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-e") == 0 && i + 1 < argc)
      napi_args[3] = argv[(i++) + 1];
    else if (strcmp(argv[i], "--enable-bug") == 0)
      noop_enable_bug = true;
    else if (strcmp(argv[i], "-b") == 0)
      /* do nothing */;
    else if (strcmp(argv[i], "--enable-diff") == 0)
      noop_enable_diff = true;
    else {
      printf("unknown args '%s'\n", argv[i]);
      exit(0);
    }
  }

  if (!napi_args[3]) {
    printf("need '-e <elf>' as options\n");
    exit(0);
  }

  elf_filename = napi_args[3];
  init_elf_file_type(elf_filename);
  init_stop_condition();
  napi_init(4, napi_args);
}

// argv decay to the secondary pointer
DiffTop::DiffTop(int argc, const char *argv[])
    : regs_fs("registers.txt"),
      nemu_ulite_fs("nemu-serial.txt"),
      noop_ulite_fs("noop-serial.txt") {
  /* init dump stream */
  dump_regs_pre(regs_fs);
  dump_ulite_pre(nemu_ulite_fs);
  dump_ulite_pre(noop_ulite_fs);

  /* `soc_emu_top' must be created before srand */
  dut_ptr.reset(new verilator_top);

  /* srand */
  uint32_t seed = 0x12345678;
  srand(seed);
  srand48(seed);
  Verilated::randReset(seed);

  /* init nemu */
  init_from_args(argc, argv);

  /* init ddr */
  void *nemu_ddr_map = napi_map_dev("ddr", 0, ddr_size);
  memcpy(ddr, nemu_ddr_map, ddr_size);

  /* reset n noop_cycles */
  noop_reset_ncycles(10);

  /* print seed */
  printf(ESC_BLUE "seed %u" ESC_RST "\n", seed);
}

void DiffTop::noop_reset_ncycles(unsigned n) {
  for (int i = 0; i < n; i++) {
    dut_ptr->reset = 1;
    run_noop_one_cycle();
    dut_ptr->reset = 0;
  }
}

void DiffTop::run_nemu_one_instr() {
  /* launch timer interrupt */
  napi_set_irq(7, dut_ptr->io_commit_ip7);
  /* nemu executes one cycle */
  napi_ulite_set_data(-1);
  napi_exec(1);
  int ch = napi_ulite_get_data();
  if (ch != -1) dump_nemu_ulite_single(ch);
  if (napi_cpu_is_end()) nemu_state = NEMU_END;
}

void DiffTop::noop_tame_nemu() {
  /* keep consistency when execute mfc0 count */
  mips_instr_t instr = napi_get_instr();
  if (instr.is_mfc0_count()) {
    uint32_t r = instr.get_rt();
    uint32_t count0 = get_dut_gpr(r);
    napi_set_gpr(r, count0);
  }
}

bool DiffTop::run_noop_one_cycle() {
  dut_ptr->io_can_log_now = can_log_now();
  dut_ptr->io_enable_bug = noop_enable_bug;

  dut_ptr->clock = 0;
  dut_ptr->eval();

  dut_ptr->clock = 1;
  dut_ptr->eval();
  noop_cycles++;
  return dut_ptr->io_commit_valid;
}

void DiffTop::run_noop_one_instr() {
  while (!run_noop_one_cycle())
    ;
}

bool DiffTop::run_diff_one_instr() {
  bool chkflag = true;
  if (noop_state == NOOP_RUNNING)
    run_noop_one_instr();
  else
    noop_cycles++;

  if (nemu_state == NEMU_RUNNING) {
    run_nemu_one_instr();
    if (noop_enable_diff && noop_state == NOOP_RUNNING) {
      noop_tame_nemu();
      chkflag = check_states();
    }
  }

  if (!chkflag) {
    printf("[noop chkfail, cycles: %ld, ninstr: %ld]\n",
        noop_cycles, noop_ninstr);
  }

  noop_ninstr++;
  return chkflag;
}

int DiffTop::execute() {
  while (nemu_state == NEMU_RUNNING ||
         noop_state == NOOP_RUNNING) {
    run_diff_one_instr();
    dump_registers();
  }
  return 0;
}

void DiffTop::device_io(int addr, int len, int data,
    char func, char strb, int *resp) {
  assert(func == MX_RD || func == MX_WR);
  assert((addr & 3) == 0);

  /* mmio */
  if (!(0 <= addr && addr < 0x08000000)) {
    /* deal with dev_io */
    if (func == MX_RD) {
      if (napi_addr_is_valid(addr)) {
        *resp = napi_mmio_peek(addr, len + 1);
      } else {
        napi_dump_states();
        printf("bad addr 0x%08x received from SOC\n", addr);
        abort();
      }
    } else {
      if (addr == GPIO_TRAP) {
        if (data == 0)
          printf(
              ESC_GREEN "[NOOP] HIT GOOD TRAP\n" ESC_RST);
        else
          printf(ESC_RED
              "[NOOP] HIT BAD TRAP (%d)\n" ESC_RST,
              data);
        updateNoopState(NOOP_TRAP);
        noop_trap_code = data;
      } else if (addr == ULITE_BASE + ULITE_Tx) {
        char ch = (char)data;
        dump_noop_ulite_single(ch);
        if (ulite_stop_string_ptr) {
          if (*ulite_stop_string_ptr == ch) {
            ulite_stop_string_ptr++;
            if (*ulite_stop_string_ptr == 0) {
              updateNoopState(NOOP_ULITE_END);
              printf(
                  "[noop ulite-end, cycles: %ld, ninstr: "
                  "%ld]\n",
                  noop_cycles, noop_ninstr);
            }
          } else {
            ulite_stop_string_ptr = ulite_stop_string;
          }
        }
      }
    }
    return;
  }

  assert(0 <= addr && addr < 0x08000000);
  /* ddr io */
  if (func == MX_RD) {
    // MX_RD
    memcpy(resp, &ddr[addr], 4);
  } else {
    // MX_WR
    addr = addr & ~3;
    for (int i = 0; i < 4; i++) {
      if (strb & (1 << i))
        ddr[addr + i] = (data >> (i * 8)) & 0xFF;
    }
  }
}

void DiffTop::stopWhenUliteSend(const char *string) {
  ulite_stop_string = string;
  ulite_stop_string_ptr = ulite_stop_string;
}
