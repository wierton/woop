#include "common.h"
#include "emu.h"

uint32_t Emulator::bram[4]
#if 0
 = {
   0x3c080000 | (Emulator::entry >> 16), // lui t0, %hi(entry)
   0x35080000 | (Emulator::entry & 0xFFFF), // ori t0, t0, %lo(entry)
   0x01000008,
   0x00000000,
 }
#endif
    ;

Emulator::Emulator(uint8_t *image, uint32_t image_size)
    : ddr(image), ddr_size(image_size) {
  reset_ncycles(1);
  klee_make_symbolic(bram, sizeof(bram), "bram");
}

void Emulator::reset_ncycles(unsigned n) {
  for (int i = 0; i < n; i++) {
    dut.reset = 1;
    single_cycle();
    dut.reset = 0;
  }
}

void Emulator::single_cycle() {
  dut.clock = 0;
  dut.eval();

  dut.clock = 1;
  dut.eval();
}

int Emulator::execute(uint64_t n) {
  while (!finished && n > 0) {
    single_cycle();
    n--;
  }

  if (finished) return ret_code;
  return n == 0 ? -1 : 0;
}

void Emulator::device_io(int addr, int len, int data,
    char func, char strb, int *resp) {
  assert(func == MX_RD || func == MX_WR);
  assert((addr & 3) == 0);

  /* mmio */
  if (!(0 <= addr && addr < 0x08000000)) {
    if (func == MX_RD) {
      /* block ram read*/
      if (BRAM_BASE <= addr && addr < BRAM_BASE + BRAM_SIZE)
        memcpy(
            resp, &((uint8_t *)bram)[addr - BRAM_BASE], 4);
    } else {
      if (addr == GPIO_TRAP) {
        finished = true;
        ret_code = data;
        dprintf(
            "cycles: %ld, ninstr: %ld\n", cycles, ninstr);
      } else if (addr == ULITE_Tx) {
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
