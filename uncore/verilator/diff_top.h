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

#include "verilator_top.h"
#include "verilator_top__Dpi.h"
extern "C" {
#include "napis.h"
}

#define MX_RD 0
#define MX_WR 1
#define GPIO_TRAP 0x10000000
#define ULITE_BASE 0x1fe50000
#define ULITE_Rx 0x0
#define ULITE_Tx 0x4
#define ULITE_STAT 0x8
#define ULITE_CTRL 0xC

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

inline bool string_contains(
    const std::string &lhs, const std::string &rhs) {
  if (rhs.empty()) return true;
  for (int i = 0; i < lhs.size(); i++) {
    bool isSame = true;
    for (int j = 0; j < rhs.size(); j++) {
      if (i + j >= lhs.size() || lhs[i + j] != rhs[j]) {
        isSame = false;
        break;
      }
    }
    if (isSame) return true;
  }
  return false;
}

inline std::string escape(char ch) {
  std::string out;
  if (ch == '\n')
    out += "\\n";
  else if (ch == '\r')
    out += "\\r";
  else if (std::isprint(ch))
    out.push_back(ch);
  else {
    out += "\\x";
    out.push_back((ch >> 4) + 'a');
    out.push_back((ch & 0xf) + 'a');
  }
  return out;
}

class DiffTop {
private:
  std::unique_ptr<verilator_top> dut_ptr;

  std::ofstream regs_fs;
  std::ofstream nemu_ulite_fs;
  std::ofstream noop_ulite_fs;

  void dump_regs_pre(std::ostream &os);
  void dump_regs_post(std::ostream &os);
  void dump_regs_single(bool chkflag);
  void dump_nemu_regs_single(std::ostream &os);
  void dump_noop_regs_single(std::ostream &os);

  void dump_ulite_pre(std::ostream &os);
  void dump_ulite_post(std::ostream &os);
  void dump_nemu_ulite_single(int data);
  void dump_noop_ulite_single(int data);

  const char *ulite_stop_string = nullptr;
  const char *ulite_stop_string_ptr;
  void stop_noop_when_ulite_send(const char *data) {
    ulite_stop_string = ulite_stop_string_ptr = data;
  }

  enum ElfType {
    ELF_VMLINUX,
    ELF_MICROBENCH,
    ELF_PRINTF,
    ELF_OTHER,
  } elf_type;

  enum NemuState {
    NEMU_RUNNING,
    NEMU_END,
  } nemu_state = NEMU_RUNNING;

  enum NoopState {
    NOOP_RUNNING,
    NOOP_CHKFAIL,
    NOOP_TRAP,
    NOOP_ULITE_END,
  } noop_state = NOOP_RUNNING;

  void updateNoopState(NoopState newState) {
    assert(noop_state == NOOP_RUNNING);
    noop_state = newState;
  }

public:
  uint64_t noop_end_ninstr = 0;
  uint64_t noop_ninstr = 0;
  uint64_t noop_cycles = 0;
  int noop_trap_code = 0;
  bool noop_enable_bug = false;
  bool noop_enable_diff = false;

  void stopWhenUliteSend(const char *string);

  static constexpr uint32_t ddr_size = 128 * 1024 * 1024;
  uint8_t ddr[ddr_size];

  bool check_states();
  uint32_t get_dut_gpr(uint32_t r);

  void noop_reset_ncycles(unsigned n);
  void noop_tame_nemu();
  bool run_noop_one_cycle();
  void run_noop_one_instr();
  void run_nemu_one_instr();
  bool run_diff_one_instr();

  bool can_log_now() const;
  void dump_registers();

  void init_elf_file_type(const char *file);
  void init_stop_condition();
  void init_from_args(int argc, const char *argv[]);

public:
  // argv decay to the secondary pointer
  DiffTop(int argc, const char *argv[]);
  ~DiffTop();

  int execute();
  void device_io(int addr, int len, int data, char func,
      char wstrb, int *resp);
};

#endif
