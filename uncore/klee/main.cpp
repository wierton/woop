#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <getopt.h>
#include <iomanip>
#include <memory>
#include <signal.h>
#include <string.h>
#include <sys/syscall.h>

#include "common.h"
#include "emu.h"

static Emulator *emu = nullptr;

uint8_t image[] = {
#include "image.txt"
};

extern "C" void device_io(unsigned char valid, int addr,
    int len, int data, char func, char wstrb, int *resp) {
  if (!valid) return;
  emu->device_io(addr, len, data, func, wstrb, resp);
}

double sc_time_stamp() { return 0; }

int main(int argc, const char **argv) {
  int ret = 0;
  emu = new Emulator(image, sizeof(image));
  emu->dut.io_can_log_now = false;
  while (true) {
    emu->single_cycle();
    if (emu->dut.io_commit_valid &&
        emu->dut.io_commit_instr == 0x12345678)
      break;
  }
  assert(0);

  if (ret == -1) {
    eprintf(ESC_RED "Timeout\n" ESC_RST);
  } else if (ret == 0) {
    eprintf(ESC_GREEN "HIT GOOD TRAP\n" ESC_RST);
  } else {
    eprintf(ESC_RED "HIT BAD TRAP (%d)\n" ESC_RST, ret);
  }

  delete emu;
  return ret;
}
