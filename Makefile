.PHONY: all emu clean-emu clean-am clean-all update minicom

export ARCH=mips32-npc
export CROSS_COMPILE=mips-linux-gnu-
export AM_HOME=$(abspath $(PWD)/../nexus-am)
export MIPS32_NEMU_HOME=$(abspath $(PWD)/../nemu-mips32)
export INSTTEST_HOME=$(abspath $(PWD)/../insttest)
export UBOOT_HOME=$(abspath $(PWD)/../u-boot)
export NANOS_HOME=$(abspath $(PWD)/../nanos)

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
include rules/test-all.mk
include rules/loongson.mk
include rules/nscscc.mk
