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

#include "emu.h"
#include "emu__Dpi.h"
#include "nemu_api.h"

class NEMU_MIPS32;
struct device_t;

// wrappers for nemu-mips32 library
class mips_instr_t {
  union {
    struct {
      uint32_t sel : 3;
      uint32_t __ : 8;
      uint32_t rd : 5;
      uint32_t rt : 5;
      uint32_t mf : 5;
      uint32_t cop0 : 6;
    };
    uint32_t val;
  };

public:
  mips_instr_t(uint32_t instr) : val(instr) {}

  bool is_mfc0_count() const {
    return cop0 == 0x10 && mf == 0x0 && rd == 0x9 &&
           sel == 0x0;
  }
  bool is_syscall() const { return val == 0x0000000c; }
  bool is_eret() const { return val == 0x42000018; }
  uint32_t get_rt() { return rt; }
};

class Emulator {
  std::unique_ptr<emu> dut_ptr;
  std::unique_ptr<NEMU_MIPS32> nemu_ptr;

  uint32_t seed;
  uint64_t cycles, silent_cycles;

  void epilogue();
  void check_registers();
  uint32_t get_dut_gpr(uint32_t r);
  static device_t *find_device(const char *name);
  void single_cycle();
  void cycle_epilogue();

public:
  bool finished;
  int ret_code;
  static constexpr uint32_t ddr_size = 128 * 1024 * 1024;
  uint8_t ddr[ddr_size];

public:
  // argv decay to the secondary pointer
  Emulator(int argc, const char *argv[]);
  void reset_ncycles(size_t cycles);
  int execute(uint64_t n = -1ull);

  uint64_t get_cycles() const { return cycles; }
};

#endif
