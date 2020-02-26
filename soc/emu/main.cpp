#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <getopt.h>
#include <iomanip>
#include <memory>
#include <string.h>

#include "common.h"
#include "emu_api.h"
#include "nemu_api.h"

#define MX_RD 0
#define MX_WR 0
#define GPIO_TRAP 0x10000000

uint8_t ddr_mem[128 * 1024 * 1024];

EmuGlobalState emu_gbl_state;

extern "C" {

void device_io(
    unsigned char valid, int addr, int data, char func, char wstrb, int *resp) {
  if (!valid) return;

  if (0 <= addr && addr < 0x08000000) {
    if (func == 0) {
      // MX_RD
      memcpy(resp, &ddr_mem[addr], 4);
    } else {
      // MX_WR
      for (int i = 0; i < 4; i++) {
        if (wstrb & (1 << i)) ddr_mem[addr + i] = (data >> (i * 8)) & 0xFF;
      }
    }
    return;
  }

  /* deal with read */
  if (func != 1) {
    /* all registers defined in IP manual have length 4 */
    *resp = paddr_peek(addr, 4);
    return;
  }

  /* deal with write */
  switch (addr) {
  case GPIO_TRAP:
    emu_gbl_state.finished = true;
    emu_gbl_state.ret_code = data;
    if (data == 0)
      printf(ANSI_COLOR_GREEN "EMU: HIT GOOD TRAP" ANSI_COLOR_RESET "\n");
    else
      printf(ANSI_COLOR_RED "EMU: HIT BAD TRAP" ANSI_COLOR_RESET "\n");
    break;
  }
}
}

double sc_time_stamp() { return 0; }

int main(int argc, const char **argv) {
  auto emu = Emulator(argc, argv);

  auto ret = emu.execute();

  if (ret == -1) {
    eprintf(ANSI_COLOR_RED "Timeout after %lld cycles\n" ANSI_COLOR_RESET,
        (long long)emu.get_max_cycles());
  } else if (ret == 0) {
    eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP\n" ANSI_COLOR_RESET);
  } else {
    eprintf(ANSI_COLOR_RED "HIT BAD TRAP code: %d\n" ANSI_COLOR_RESET, ret);
  }

  return ret;
}
