SCALA_DIR := src
SCALA_FILES != find $(SCALA_DIR) -name "*.scala"

EMU_OBJ_DIR := $(OBJ_DIR)/soc_emu
EMU_SRC_DIR := $(abspath ./soc/emu)
EMU_TOP_MODULE ?= SOC_EMU_TOP
EMU_PREFIX := SOC_EMU_TOP
EMU_TOP_V := $(EMU_OBJ_DIR)/$(EMU_PREFIX).v
EMU_MK := $(EMU_OBJ_DIR)/$(EMU_PREFIX).mk
EMU_BIN := $(EMU_OBJ_DIR)/emulator
EMU_LIB_V != find $(EMU_SRC_DIR) -name "*.v"
EMU_CXXFILES != find $(EMU_SRC_DIR) -name "*.cpp"

MIPS32_NEMU := $(MIPS32_NEMU_HOME)/build/nemu
MIPS32_NEMU_LIB := $(MIPS32_NEMU_HOME)/build/nemu.so

nemu: $(MIPS32_NEMU) $(MIPS32_NEMU_LIB)
emu: $(EMU_BIN)

ifeq ($(ASAN),1)
ASAN_CFLAGS := -fsanitize=address,undefined -Wformat -Werror=format-security -Werror=array-bounds
ASAN_LDFLAGS := -fsanitize=address,undefined
endif

EMU_CFLAGS := -I. -I $(MIPS32_NEMU_HOME)/include $(ASAN_CFLAGS)
EMU_LDFLAGS := $(MIPS32_NEMU_LIB) -lpthread -lreadline -lSDL $(ASAN_LDFLAGS)

$(EMU_TOP_V): $(SCALA_FILES)
	@mkdir -p $(@D)
	@sbt "run $(EMU_TOP_MODULE) -td $(@D) --output-file $(@F)"
	@sed -i '/ bram /a`undef RANDOMIZE_MEM_INIT' $@

$(EMU_MK): $(EMU_TOP_V) $(EMU_CXXFILES) $(EMU_LIB_V)
	@mkdir -p $(@D)
	@verilator -Wno-lint --cc --exe \
	  --top-module $(EMU_TOP_MODULE) \
	  -o $(notdir $(EMU_BIN)) -Mdir $(@D) \
	  -CFLAGS "$(EMU_CFLAGS)" -LDFLAGS "$(EMU_LDFLAGS)" \
	  --prefix $(EMU_PREFIX) $^ 

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
