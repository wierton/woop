#ifndef EMU_API_H
#define EMU_API_H

#include <cassert>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <utility>
#include <vector>

#include "klee_top.h"
#include "klee_top__Dpi.h"

#define MX_RD 0
#define MX_WR 1
#define GPIO_TRAP 0x10000000
#define ULITE_BASE 0x1fe50000
#define ULITE_Rx (ULITE_BASE + 0x0)
#define ULITE_Tx (ULITE_BASE + 0x4)
#define ULITE_STAT (ULITE_BASE + 0x8)
#define ULITE_CTRL (ULITE_BASE + 0xC)
#define BRAM_BASE 0x1fc00000
#define BRAM_SIZE (4 * 4)

struct Emulator {
  klee_top dut;
  static constexpr uint32_t entry = 0x80000000;
  static uint32_t bram[4];

  uint32_t seed;
  uint64_t ninstr = 0;
  uint64_t cycles = 0, silent_cycles = 0;

  bool finished = false;
  int ret_code = -1;
  uint32_t ddr_size;
  uint8_t *ddr;

  void single_cycle();
  void reset_ncycles(unsigned n);

  Emulator(uint8_t *image, uint32_t ddr_size);
  int execute(uint64_t n = -1ull);
  void device_io(int addr, int len, int data, char func,
      char wstrb, int *resp);
};

#endif
