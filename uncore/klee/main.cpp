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

void emulator_epilogue(int sig) { syscall(__NR_exit, 0); }

int main(int argc, const char **argv) {
  signal(SIGINT, emulator_epilogue);

  emu = new Emulator(image, sizeof(image));
  emu->dut.io_can_log_now = false;
  auto ret = emu->execute();

  if (ret == -1) {
    eprintf(ESC_RED "Timeout\n" ESC_RST);
  } else if (ret == 0) {
    eprintf(ESC_GREEN "HIT GOOD TRAP\n" ESC_RST);
  } else {
    eprintf(ESC_RED "HIT BAD TRAP (%d)\n" ESC_RST, ret);
  }

  delete emu;
  syscall(__NR_exit, ret);
  return ret;
}
