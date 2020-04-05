#ifndef NEMU_API_H
#define NEMU_API_H

#include <stdint.h>

// wrappers for nemu-mips32 library
class NEMU_MIPS32 {
public:
  NEMU_MIPS32(int argc, const char *argv[]);

  static void dump();

  static void exec(unsigned n);
  static uint32_t pc();
  static uint32_t &gpr(uint32_t i);
  static uint32_t get_instr();
  static void set_irq(unsigned irqno, bool v);
  static uint32_t paddr_peek(uint32_t addr, int len);
  static void *map(
      const char *name, unsigned addr, unsigned size);
  static bool is_mapped(unsigned addr);
  static uint64_t get_ms();
  static void dump_tlb();
};

#endif
