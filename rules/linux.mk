.PHONY: compile-linux
.PHONY: run-nemu-linux run-linux run-cputests-linux

LINUX_OBJDIR := $(OBJ_DIR)/linux
LINUX_BIN    := $(LINUX_HOME)/vmlinux
U_BOOT_BIN   := $(U_BOOT_HOME)/u-boot

$(U_BOOT_BIN):
	@ARCH=mips CROSS_COMPILE=mips-linux-gnu- make -s $(U_BOOT_HOME) -j32
$(LINUX_BIN):
	@ARCH=mips CROSS_COMPILE=mips-linux-gnu- make -s $(LINUX_HOME) -j32

compile-u-boot: $(U_BOOT_BIN)
	@mkdir -p $(LINUX_OBJDIR)
	@cd $(LINUX_OBJDIR) && ln -sf $^ .
compile-linux: $(LINUX_BIN)
	@mkdir -p $(LINUX_OBJDIR)
	@cd $(LINUX_OBJDIR) && ln -sf $^ .

run-u-boot run-linux: $(EMU_BIN) compile-u-boot compile-linux
	@mkdir -p $(LINUX_OBJDIR)
	@cd $(LINUX_OBJDIR) && \
	  ln -sf $(abspath $(EMU_BIN)) emulator && \
	  ./emulator -e u-boot --block-data ddr:0x4000000:vmlinux 2>npc.out

run-nemu-u-boot run-nemu-linux: $(EMU_BIN) compile-u-boot compile-linux
	mkdir -p $(LINUX_OBJDIR)
	cd $(LINUX_OBJDIR) && \
	  ln -sf $(MIPS32_NEMU) nemu && \
	  ./nemu -b -e u-boot --block-data ddr:0x4000000:vmlinux 2>npc.out
