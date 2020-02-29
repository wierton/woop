.PHONY: all emu clean-emu clean-am clean-all update minicom

export ARCH=mips32-npc
export CROSS_COMPILE=mips-linux-gnu-
export AM_HOME          := $(PWD)/../nexus-am
export MIPS32_NEMU_HOME := $(PWD)/../nemu-mips32
export INSTTEST_HOME    := $(PWD)/../insttest
export U_BOOT_HOME      := $(PWD)/../u-boot
export LINUX_HOME       := $(PWD)/../linux
export NANOS_HOME       := $(PWD)/../nanos

.DEFAULT_GOAL=emu

VIVADO := vivado
VIVADO_18 := vivado
VIVADO_FLAG := -nolog -nojournal -notrace

OBJ_DIR := output

clean-am:
	make -s -C $(AM_HOME) clean

clean-all: clean-emu clean-am

minicom:
	cd $(OBJ_DIR) && sudo minicom -D /dev/ttyUSB1 -b 115200 -c on -C cpu.log -S ../minicom.script

include rules/emu.mk
include rules/test.mk
include rules/loongson.mk
include rules/nscscc.mk
include rules/linux.mk
