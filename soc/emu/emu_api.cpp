#include "common.h"
#include "emu_api.h"

/* clang-format off */
#define GPRS(X) \
  X(0)  X(1)  X(2)  X(3)  X(4)  X(5)  X(6)  X(7)  \
  X(8)  X(9)  X(10) X(11) X(12) X(13) X(14) X(15) \
  X(16) X(17) X(18) X(19) X(20) X(21) X(22) X(23) \
  X(24) X(25) X(26) X(27) X(28) X(29) X(30) X(31)
/* clang-format on */

void DiffTop::abort_prologue() {
  if (finished) return;
  finished = true;
  single_cycle();
}

void DiffTop::check_states() {
#define check(cond, ...)  \
  if (!(cond)) {          \
    nemu_ptr->dump();     \
    eprintf(__VA_ARGS__); \
    abort_prologue();     \
    abort();              \
  }

  check(nemu_ptr->pc() == dut_ptr->io_commit_pc,
      "cycle %lu: pc: nemu:%08x <> dut:%08x\n", cycles,
      nemu_ptr->pc(), dut_ptr->io_commit_pc);
  check(nemu_ptr->get_instr() == dut_ptr->io_commit_instr,
      "cycle %lu: instr: nemu:%08x <> dut:%08x\n", cycles,
      nemu_ptr->get_instr(), dut_ptr->io_commit_instr);

  if (last_instr_is_store) {
    uint32_t nemu_mc = paddr_peek(ls_addr, 4);
    check(nemu_mc == ls_data,
        "cycle %lu: M[%08x]: nemu:%08x <> dut:%08x\n",
        cycles, ls_addr, nemu_mc, ls_data);
  }

#define GPR_TEST(i)                                     \
  check(nemu_ptr->gpr(i) == dut_ptr->io_commit_gpr_##i, \
      "cycle %lu: gpr[%d]: nemu:%08x <> dut:%08x\n",    \
      cycles, i, nemu_ptr->gpr(i),                      \
      dut_ptr->io_commit_gpr_##i);
  GPRS(GPR_TEST);
#undef GPR_TEST
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

device_t *DiffTop::find_device(const char *name) {
  for (device_t *head = get_device_list_head(); head;
       head = head->next) {
    if (strcmp(head->name, name) == 0) return head;
  }
  return nullptr;
}

// argv decay to the secondary pointer
DiffTop::DiffTop(int argc, const char *argv[]) {
  /* `emu' must be created before srand */
  dut_ptr.reset(new emu);

  /* srand */
  seed = (unsigned)time(NULL) ^ (unsigned)getpid();
  srand(seed);
  srand48(seed);
  Verilated::randReset(seed);

  /* init nemu */
  nemu_ptr.reset(new NEMU_MIPS32(argc, argv));

  /* init ddr */
  device_t *ddr_dev = find_device("ddr");
  void *nemu_ddr_map = ddr_dev->map(0, ddr_size);
  memcpy(ddr, nemu_ddr_map, ddr_size);

  /* reset n cycles */
  reset_ncycles(10);

  /* print seed */
  printf(ESC_BLUE "seed %u" ESC_RST "\n", seed);
}

void DiffTop::reset_ncycles(unsigned n) {
  for (int i = 0; i < n; i++) {
    dut_ptr->reset = 1;
    single_cycle();
    dut_ptr->reset = 0;
  }
}

void DiffTop::cycle_epilogue() {
  cycles++;
  silent_cycles++;

  if (silent_cycles >= 1000) {
    printf("cycle %lu: no commits in %ld cycles\n", cycles,
        silent_cycles);
    abort();
  }

  if (!dut_ptr->io_commit_valid) { return; }

  silent_cycles = 0;

  /* keep consistency when execute mfc0 */
  mips_instr_t instr = dut_ptr->io_commit_instr;
  if (instr.is_mfc0_count()) {
    uint32_t count0 = get_dut_gpr(instr.get_rt());
    nemu_ptr->set_c0_count(count0);
  }

  /* nemu executes one cycle */
  nemu_ptr->exec_one_instr();

  /* don't check eret and syscall instr */
  if (!instr.is_syscall() && !instr.is_eret())
    check_states();

  last_instr_is_store = false;
}

void DiffTop::single_cycle() {
  dut_ptr->clock = 0;
  dut_ptr->eval();

  dut_ptr->clock = 1;
  dut_ptr->eval();
}

int DiffTop::execute(uint64_t n) {
  while (!finished && n > 0) {
    single_cycle();
    cycle_epilogue();
    n--;
  }

  if (finished) return ret_code;
  return n == 0 ? -1 : 0;
}

void DiffTop::device_io(unsigned char is_aligned, int addr,
    int len, int data, char func, char strb, int *resp) {
  assert(func == MX_RD || func == MX_WR);

  /* mmio */
  if (!(0 <= addr && addr < 0x08000000)) {
    /* deal with dev_io */
    if (func == MX_RD) {
      device_t *dev = ::find_device(addr);
      if (!dev) {
        nemu_ptr->dump();
        eprintf(
            "bad addr 0x%08x received from SOC\n", addr);
        abort();
      } else {
        *resp = paddr_peek(addr, len + 1);
      }
    } else {
      if (addr == GPIO_TRAP) {
        finished = true;
        ret_code = data;
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
    if (is_aligned) {
      int l2b = addr & 3;
      assert(l2b + len < 4);
      memcpy(&ddr[addr], &data, len + 1);
    } else {
      addr = addr & ~3;
      for (int i = 0; i < 4; i++) {
        if (strb & (1 << i))
          ddr[addr + i] = (data >> (i * 8)) & 0xFF;
      }
    }

    last_instr_is_store = true;
    ls_addr = addr & ~3;
    memcpy(&ls_data, &ddr[ls_addr], 4);
  }
}
