#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <getopt.h>
#include <iomanip>
#include <memory>
#include <string.h>
#include <syscalls.h>

#include "common.h"
#include "diff_top.h"

static std::unique_ptr<DiffTop> diff_top;

extern "C" void device_io(unsigned char valid,
    unsigned char is_aligned, int addr, int len, int data,
    char func, char wstrb, int *resp) {
  if (!valid) return;

  diff_top->device_io(
      is_aligned, addr, len, data, func, wstrb, resp);
}

double sc_time_stamp() { return 0; }

int main(int argc, const char **argv) {
  diff_top.reset(new DiffTop(argc, argv));
  auto ret = diff_top->execute();

  if (ret == -1) {
    eprintf(ESC_RED "Timeout\n" ESC_RST);
  } else if (ret == 0) {
    eprintf(ESC_GREEN "HIT GOOD TRAP\n" ESC_RST);
  } else {
    eprintf(ESC_RED "HIT BAD TRAP (%d)\n" ESC_RST, ret);
  }

  // asm volatile("int $0x80": :"a"(__NR_exit), "b"(ret));
  asm volatile("int $0x80": :"a"(__NR_exit), "D"(ret));
  return ret;
}
