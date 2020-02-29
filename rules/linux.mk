.PHONY: compile-linux
.PHONY: run-nemu-linux run-linux run-cputests-linux

LINUX_OBJDIR := $(OBJ_DIR)/linux
LINUX_BIN    := $(LINUX_HOME)/vmlinux
U_BOOT_BIN   := $(U_BOOT_HOME)/u-boot

$(U_BOOT_BIN) $(LINUX_BIN):
	@ARCH=mips CROSS_COMPILE=mips-linux-gnu- make -s -C $(@D) -j32

compile-u-boot: $(U_BOOT_BIN)
compile-linux: $(LINUX_BIN)
compile-u-boot compile-linux:
	@mkdir -p $(LINUX_OBJDIR)
	@cd $(LINUX_OBJDIR) && ln -sf $^ .

run-u-boot: $(EMU_BIN) compile-u-boot
run-linux: $(EMU_BIN) compile-u-boot compile-linux
run-nemu-u-boot: $(MIPS32_NEMU) compile-u-boot
run-nemu-linux: $(MIPS32_NEMU) compile-u-boot compile-linux

run-u-boot run-linux:
	@mkdir -p $(LINUX_OBJDIR)
	@cd $(LINUX_OBJDIR) && \
	  ln -sf $(abspath $(EMU_BIN)) emulator && \
	  ./emulator -e u-boot \
	  --block-data ddr:0x4000000:vmlinux 2>npc.out

run-nemu-u-boot run-nemu-linux:
	@mkdir -p $(LINUX_OBJDIR)
	@cd $(LINUX_OBJDIR) && \
	  ln -sf $(MIPS32_NEMU) nemu && \
	  ./nemu -b -e u-boot \
	  --block-data ddr:0x4000000:vmlinux
