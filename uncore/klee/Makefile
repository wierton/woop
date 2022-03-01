.PHONY: run-% project update clean

EMU_OBJ_DIR    := $(CORE_OBJ_DIR)
EMU_TOP_MODULE := $(CORE_TOP_MODULE)
EMU_TOP_V      := $(CORE_TOP_V)
EMU_SRC_DIR    := $(abspath uncore/$(UNCORE))
EMU_PREFIX     := $(UNCORE)_top
EMU_MK         := $(EMU_OBJ_DIR)/$(EMU_PREFIX).mk
EMU_BIN        := $(EMU_OBJ_DIR)/emulator
EMU_BC         := $(EMU_OBJ_DIR)/emulator.bc
EMU_LIB_V      != find $(EMU_SRC_DIR) -name "*.v"
EMU_CXXFILES   != find $(EMU_SRC_DIR) -name "*.cpp"
IMAGE_TXT      := $(EMU_OBJ_DIR)/image.txt
LLVM_LINK      := llvm-link-10

EMU_CFLAGS := -I. -I$(MIPS32_NEMU_HOME)/include -I$(EMU_OBJ_DIR)

$(IMAGE_TXT): $($(IMAGE)_APP)-$(ARCH).bin
	@hexdump -ve '8/1 "0x%02x, " "\n"' $^ > $@

$(EMU_BIN): $(EMU_MK) $(EMU_CXXFILES) $(IMAGE_TXT)
	@echo + $(EMU_BIN)
	@cd $(@D) && make -s -f $(notdir $<)
	@touch $<

$(EMU_BC): $(EMU_MK) $(EMU_CXXFILES) $(IMAGE_TXT)
	@echo + $(EMU_BC)
	@cd $(@D) && \
	  make -s -nB -f $(notdir $<) | \
	  sed 's/^g++/clang -g -emit-llvm/g' | \
	  sed 's/\(\w*\)\.[oa]/\1.bc/g' | \
	  sed 's/-Wno-unused-but-set-variable//g' | \
	  sed 's/ar -cr/$(LLVM_LINK) -o/g' | \
	  sed 's/clang.*-emit-llvm\(.*\)-o.*-lm -lstdc\+\+.*/$(LLVM_LINK) \1 -o emulator.bc/g' | \
	  sed 's/^ranlib/#ranlib/g' | sh -

$(EMU_MK): $(EMU_TOP_V) $(EMU_CXXFILES) $(EMU_LIB_V)
	@mkdir -p $(@D)
	@verilator -Wno-lint --cc --exe \
	  --top-module $(EMU_TOP_MODULE) \
	  -o $(notdir $(EMU_BIN)) -Mdir $(@D) \
	  -CFLAGS "$(EMU_CFLAGS)" --prefix $(EMU_PREFIX) $^ 

project: $(EMU_BIN) $(EMU_BC)

bitcode: $(EMU_BC)

run-%: project compile-%
	@cd $($*_OBJDIR) && \
	  ln -sf $(abspath $(EMU_BIN)) emulator && \
	  ./emulator -b -e ./$*.elf 2> npc.out

update: $(EMU_MK)
	@rm -rf $(EMU_BIN)
	@echo + $(EMU_BIN)
	@cd $(dir $(EMU_BIN)) && make -s -f $(notdir $<)
	@touch $<

clean:
	rm -Irf $(OBJ_DIR)/$(UNCORE)
