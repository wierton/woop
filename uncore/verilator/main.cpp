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
#include "diff_top.h"
#include "napis.h"

static std::unique_ptr<DiffTop> diff_top;

extern "C" void device_io(unsigned char valid, int addr,
    int len, int data, char func, char wstrb, int *resp) {
  if (!valid) return;

  diff_top->device_io(addr, len, data, func, wstrb, resp);
}

double sc_time_stamp() { return 0; }

void difftop_epilogue(int sig) {
  napi_dump_states();
  syscall(__NR_exit, 0);
}

int main(int argc, const char **argv) {
  signal(SIGINT, difftop_epilogue);

  diff_top.reset(new DiffTop(argc, argv));
  auto ret = diff_top->execute();
  printf("[noop-end, cycles: %ld, ninstr: %ld]\n",
      diff_top->noop_cycles, diff_top->noop_ninstr);
  diff_top.reset();

  syscall(__NR_exit, ret);
  return ret;
}
