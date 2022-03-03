.PHONY: run-% project update clean

EMU_OBJ_DIR    := $(CORE_OBJ_DIR)
EMU_TOP_MODULE := $(CORE_TOP_MODULE)
EMU_TOP_V      := $(CORE_TOP_V)
EMU_SRC_DIR    := $(abspath uncore/$(UNCORE))
EMU_PREFIX     := $(UNCORE)_top
EMU_MK         := $(EMU_OBJ_DIR)/$(EMU_PREFIX).mk
EMU_BIN        := $(EMU_OBJ_DIR)/emulator
EMU_LIB_V      != find $(EMU_SRC_DIR) -name "*.v"
EMU_CXXFILES   != find $(EMU_SRC_DIR) -name "*.cpp"

ifeq ($(ASAN),1)
ASAN_CFLAGS := -fsanitize=address,undefined -Wformat -Werror=format-security -Werror=array-bounds
ASAN_LDFLAGS := -fsanitize=address,undefined
endif

EMU_CFLAGS := -ggdb3 -I. -I $(MIPS32_NEMU_HOME)/include $(ASAN_CFLAGS)
EMU_LDFLAGS := $(MIPS32_NEMU_LIB) -lpthread -lreadline -lSDL $(ASAN_LDFLAGS)

$(EMU_BIN): $(EMU_MK) $(EMU_CXXFILES)
	@echo + $(EMU_BIN)
	@cd $(@D) && make -s -f $(notdir $<)
	@touch $<

$(EMU_MK): $(EMU_TOP_V) $(EMU_CXXFILES) $(EMU_LIB_V)
	@mkdir -p $(@D)
	@verilator -Wno-lint --cc --exe \
	  --top-module $(EMU_TOP_MODULE) \
	  -o $(notdir $(EMU_BIN)) -Mdir $(@D) \
	  -CFLAGS "$(EMU_CFLAGS)" -LDFLAGS "$(EMU_LDFLAGS)" \
	  --prefix $(EMU_PREFIX) $^ 

project: $(EMU_BIN)

run-%: project compile-%
	@cd $($*_OBJDIR) && \
	  ln -sf $(abspath $(EMU_BIN)) emulator && \
	  ./emulator --enable-diff -e ./$*.elf 2> npc.out

define getdata
cd $($1_OBJDIR); mkdir -p $2; \
  cd $2; cp ../$1* .; \
  ln -sf $(abspath $(EMU_BIN)) emulator && \
  ./emulator $3 -e ./$1.elf 2>noop-signals.txt
endef

data-%: project compile-%
	@$(call getdata,$*,has_bug_no_diff,--enable-bug)
	@$(call getdata,$*,has_bug_has_diff,--enable-bug --enable-diff)
	@$(call getdata,$*,no_bug_has_diff,--enable-diff)

data-cputests: $(foreach c,$(CPUTESTS),data-$(c))

update: $(EMU_MK)
	@rm -rf $(EMU_BIN)
	@echo + $(EMU_BIN)
	@cd $(dir $(EMU_BIN)) && make -s -f $(notdir $<)
	@touch $<

pack-data:
	cd $(OBJ_DIR)/ && mkdir -p programs
	cd $(OBJ_DIR)/cputests; \
	  find . -name "*.txt" -exec \
	  cp --parents \{\} $(abspath $(OBJ_DIR))/programs \;
	cd $(OBJ_DIR)/; \
	  find microbench/ -name "*.txt" -exec \
	  cp --parents \{\} $(abspath $(OBJ_DIR))/programs \;
	cd $(OBJ_DIR)/; \
	  find linux/ -name "*.txt" -exec \
	  cp --parents \{\} $(abspath $(OBJ_DIR))/programs \;

clean:
	rm -Irf $(OBJ_DIR)/$(UNCORE)
