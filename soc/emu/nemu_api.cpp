#include "nemu_api.h"

extern "C" {
#include "cpu.h"
#include "device.h"
#include "memory.h"

extern work_mode_t work_mode;

/* APIs exported by nemu-mips32 */
extern CPU_state cpu;
extern void cpu_exec(uint64_t);
extern void print_registers();
extern void print_instr_queue(void);
extern work_mode_t parse_args(int argc, const char *argv[]);
extern work_mode_t init_monitor(void);
extern void init_sdl(void);
extern void init_mmio(void);
extern void init_events(void);
extern uint32_t paddr_peek(paddr_t addr, int len);
extern uint32_t get_current_pc();
extern uint32_t get_current_instr();
extern device_t *get_device_list_head();
extern void nemu_set_irq(int irqno, bool val);
/* APIs exported by nemu-mips32 */
}

NEMU_MIPS32::NEMU_MIPS32(int argc, const char *argv[]) {
  parse_args(argc, argv);

  init_sdl();
  init_mmio();
  init_monitor();
  init_events();
}

void NEMU_MIPS32::dump() {
  eprintf("================nemu instrs=================\n");
  print_instr_queue();
  eprintf("==============nemu registers================\n");
  print_registers();
  eprintf("==============nemu status end===============\n");
}

void NEMU_MIPS32::exec(unsigned n) { cpu_exec(n); }
uint32_t NEMU_MIPS32::pc() { return get_current_pc(); }

uint32_t &NEMU_MIPS32::gpr(uint32_t i) {
  return cpu.gpr[i];
}

uint32_t NEMU_MIPS32::get_instr() {
  return get_current_instr();
}

void NEMU_MIPS32::set_irq(unsigned irqno, bool v) {
  nemu_set_irq(irqno, v);
}

bool NEMU_MIPS32::is_mapped(unsigned addr) {
  return find_device(addr);
}

void *NEMU_MIPS32::map(
    const char *name, unsigned addr, unsigned size) {
  device_t *dev = get_device_list_head();
  for (; dev; dev = dev->next) {
    if (strcmp(dev->name, name) == 0) break;
  }
  if (!dev) return nullptr;
  return dev->map(addr, size);
}

uint32_t NEMU_MIPS32::paddr_peek(uint32_t addr, int len) {
  return ::paddr_peek(addr, len);
}
