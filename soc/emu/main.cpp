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

static bool finished = false;
static int ret_code = 0;

bool is_finished(void) { return finished; }
int get_exit_code(void) { return ret_code; }

extern "C" {

void ddr_io(unsigned char in_req_valid, int in_req_bits_addr,
    int in_req_bits_data, char in_req_bits_fcn, char in_req_bits_wstrb,
    int *in_resp_bits_data) {
  if (!in_req_valid) return;

  if (in_req_bits_fcn == 0) {
    // MX_RD
    memcpy(in_resp_bits_data, &ddr_mem[in_req_bits_addr], 4);
  } else {
    // MX_WR
    for (int i = 0; i < 4; i++) {
      if (in_req_bits_wstrb & (1 << i))
        ddr_mem[in_req_bits_addr + i] = (in_req_bits_data >> (i * 8)) & 0xFF;
    }
  }
}

void device_io(unsigned char in_req_valid, int in_req_bits_addr,
    int in_req_bits_data, char in_req_bits_fcn, char in_req_bits_wstrb,
    int *in_resp_bits_data) {
  if (!in_req_valid) return;

  /* deal with read */
  if (in_req_bits_fcn != 1) {
    /* all registers defined in IP manual have length 4 */
    *in_resp_bits_data = paddr_peek(in_req_bits_addr, 4);
    return;
  }

  /* deal with write */
  switch (in_req_bits_addr) {
  case GPIO_TRAP:
    finished = true;
    ret_code = in_req_bits_data;
    if (in_req_bits_data == 0)
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
