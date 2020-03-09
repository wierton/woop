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

#define MX_RD 0
#define MX_WR 1
#define GPIO_TRAP 0x10000000

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

class DiffTop {
  std::unique_ptr<emu> dut_ptr;
  std::unique_ptr<NEMU_MIPS32> nemu_ptr;

  uint32_t seed;
  uint64_t cycles = 0, silent_cycles = 0;

  bool finished = false;
  int ret_code = -1;
  static constexpr uint32_t ddr_size = 128 * 1024 * 1024;
  uint8_t ddr[ddr_size];

  /* record last memory store */
  bool last_instr_is_store = false;
  uint32_t ls_addr, ls_data;

  void check_states();
  uint32_t get_dut_gpr(uint32_t r);
  static device_t *find_device(const char *name);
  void single_cycle();
  void abort_prologue();
  void cycle_epilogue();
  void reset_ncycles(unsigned n);

public:
  // argv decay to the secondary pointer
  DiffTop(int argc, const char *argv[]);
  int execute(uint64_t n = -1ull);
  void device_io(unsigned char is_aligned, int addr,
      int len, int data, char func, char wstrb, int *resp);

  uint64_t get_cycles() const { return cycles; }
};

#endif
