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
#define MX_WR 1
#define GPIO_TRAP 0x10000000

static std::unique_ptr<Emulator> soc_emu;

extern "C" {

void device_io(unsigned char valid, int addr, int data,
    char func, char wstrb, int *resp) {
  if (!valid) return;

  if (0 <= addr && addr < 0x08000000) {
    if (func == MX_RD) {
      // MX_RD
      memcpy(resp, &soc_emu->ddr[addr], 4);
    } else {
      // MX_WR
      for (int i = 0; i < 4; i++) {
        if (wstrb & (1 << i))
          soc_emu->ddr[addr + i] = (data >> (i * 8)) & 0xFF;
      }
    }
    return;
  }

  /* deal with read */
  if (func != MX_WR) {
    /* all registers defined in IP manual have length 4 */
    *resp = paddr_peek(addr, 4);
    return;
  }

  /* deal with write */
  switch (addr) {
  case GPIO_TRAP:
    soc_emu->finished = true;
    soc_emu->ret_code = data;
    break;
  default:
    /* do nothing */
    break;
  }
}
}

double sc_time_stamp() { return 0; }

int main(int argc, const char **argv) {
  soc_emu.reset(new Emulator(argc, argv));
  auto ret = soc_emu->execute();

  if (ret == -1) {
    eprintf(ESC_RED "Timeout\n" ESC_RST);
  } else if (ret == 0) {
    eprintf(ESC_GREEN "HIT GOOD TRAP\n" ESC_RST);
  } else {
    eprintf(ESC_RED "HIT BAD TRAP (%d)\n" ESC_RST, ret);
  }

  return ret;
}
