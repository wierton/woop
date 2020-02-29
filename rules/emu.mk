SCALA_DIR := src
SCALA_FILES != find $(SCALA_DIR) -name "*.scala"

EMU_OBJ_DIR := $(OBJ_DIR)/emu
EMU_SRC_DIR := $(abspath ./soc/emu)
EMU_TOP_MODULE ?= SOC_EMU_TOP
EMU_TOP_V := $(EMU_OBJ_DIR)/emu_top.v
EMU_MK := $(EMU_OBJ_DIR)/emu.mk
EMU_BIN := $(EMU_OBJ_DIR)/emulator
EMU_LIB_V != find $(EMU_SRC_DIR) -name "*.v"
EMU_CXXFILES != find $(EMU_SRC_DIR) -name "*.cpp"

MIPS32_NEMU := $(MIPS32_NEMU_HOME)/build/nemu
MIPS32_NEMU_LIB := $(MIPS32_NEMU_HOME)/build/nemu.so

nemu: $(MIPS32_NEMU) $(MIPS32_NEMU_LIB)
emu: $(EMU_BIN)

$(EMU_TOP_V): $(SCALA_FILES)
	@mkdir -p $(@D)
	@sbt "run $(EMU_TOP_MODULE) -td $(@D) --output-file $(@F)"
	@sed -i '/ bram /a`undef RANDOMIZE_MEM_INIT' $@

$(EMU_MK): $(EMU_TOP_V) $(EMU_CXXFILES) $(EMU_LIB_V)
	@mkdir -p $(@D)
	@verilator --cc --exe --top-module $(EMU_TOP_MODULE) \
	  -o $(notdir $(EMU_BIN)) -Mdir $(@D) \
		-CFLAGS "-I $(MIPS32_NEMU_HOME)/include" \
		-LDFLAGS "$(MIPS32_NEMU_LIB) -lpthread -lreadline -lSDL" \
		--prefix $(basename $(notdir $(EMU_MK))) $^ 

update-emu: $(EMU_MK)
	@rm -rf $(EMU_BIN)
	@echo + $(EMU_BIN)
	@cd $(dir $(EMU_BIN)) && make -s -f $(notdir $<)
	@touch $<

$(EMU_BIN): $(EMU_MK) $(EMU_CXXFILES)
	@echo + $(EMU_BIN)
	@cd $(@D) && make -s -f $(notdir $<)
	@touch $<

$(MIPS32_NEMU) $(MIPS32_NEMU_LIB): $(shell find $(MIPS32_NEMU_HOME) -name "*.c" -or -name "*.h")
	@make -s -C $(MIPS32_NEMU_HOME) ARCH=mips32-npc

clean-emu:
	rm -rf $(EMU_OBJ_DIR)
