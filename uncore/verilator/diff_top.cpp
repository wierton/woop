#include "common.h"
#include "diff_top.h"

/* clang-format off */
#define GPRS(X) \
  X(0)  X(1)  X(2)  X(3)  X(4)  X(5)  X(6)  X(7)  \
  X(8)  X(9)  X(10) X(11) X(12) X(13) X(14) X(15) \
  X(16) X(17) X(18) X(19) X(20) X(21) X(22) X(23) \
  X(24) X(25) X(26) X(27) X(28) X(29) X(30) X(31)
/* clang-format on */

bool DiffTop::check_states() {
  mips_instr_t instr = napi_get_instr();
  if (instr.is_syscall() || instr.is_eret()) return true;

#define check_eq(a, b, ...)        \
  if ((a) != (b)) {                \
    eprintf(__VA_ARGS__);          \
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
      uint32_t bug_ninstr = 29949902;
      if (noop_ninstr % 3000 == 0 ||
          (bug_ninstr - 2000 < noop_ninstr &&
              noop_ninstr < bug_ninstr + 2000)) {
        dump_regs_single(noop_state != NOOP_CHKFAIL);
      }
    } break;
    default: break;
    }
  }
}

bool DiffTop::can_log_now() const { return false; }

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
      eprintf("unknown args '%s'\n", argv[i]);
      exit(0);
    }
  }

  if (!napi_args[3]) {
    eprintf("need '-e <elf>' as options\n");
    exit(0);
  }

  std::string elf = napi_args[3];
  if (string_contains(elf, "linux")) {
    elf_type = ELF_VMLINUX;
  } else if (string_contains(elf, "microbench")) {
    elf_type = ELF_MICROBENCH;
  } else if (string_contains(elf, "printf")) {
    elf_type = ELF_PRINTF;
  } else {
    elf_type = ELF_OTHER;
  }

  switch (elf_type) {
  case ELF_PRINTF:
    if (noop_enable_bug) {
      if (noop_enable_diff) {
      } else {
        stop_noop_when_ulite_send(
            "really trigger the bug!");
      }
    } else {
      stop_noop_when_ulite_send("it seems no bug.");
    }
    napi_stop_cpu_when_ulite_send("it seems no bug.");
    break;
  case ELF_VMLINUX:
    if (noop_enable_bug) {
      if (noop_enable_diff) {
      } else {
        stop_noop_when_ulite_send(
            "kill the idle task! ]---");
      }
    } else {
      stop_noop_when_ulite_send("activate this console.");
    }
    napi_stop_cpu_when_ulite_send("activate this console.");
    break;
  default: break;
  }

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

  noop_ninstr++;
  return chkflag;
}

int DiffTop::execute() {
  while (nemu_state == NEMU_RUNNING ||
         noop_state == NOOP_RUNNING) {
    eprintf("<$pc: %08x %08x %d %d\n",
        dut_ptr->io_commit_pc, napi_get_pc(), noop_state,
        nemu_state);
    dut_ptr->io_can_log_now = can_log_now();
    dut_ptr->io_enable_bug = noop_enable_bug;
    run_diff_one_instr();
    dump_registers();
    eprintf(">$pc: %08x %08x %d %d\n",
        dut_ptr->io_commit_pc, napi_get_pc(), noop_state,
        nemu_state);
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
        eprintf(
            "bad addr 0x%08x received from SOC\n", addr);
        abort();
      }
    } else {
      if (addr == GPIO_TRAP) {
        if (data == 0)
          eprintf(
              ESC_GREEN "[NOOP] HIT GOOD TRAP\n" ESC_RST);
        else
          eprintf(ESC_RED
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
              eprintf(
                  "noop ulite recv '%s', stop the cpu\n",
                  ulite_stop_string);
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
