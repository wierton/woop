# note INPUT=REF is designed for microbench
# arg1 dir
# arg2 name eg. videotest microbench
# arg3 objdir default: obj/name
# arg4 env
define test_template
$(2)_APP := $(1)/build/$(2)
ifneq ($(3),)
$(2)_OBJDIR := $(3)
else
$(2)_OBJDIR := $(OBJ_DIR)/$(2)
endif
$(2)_DEPS != find $(1) -regex ".*.\(c\|h\|cc\|cpp\|S\)"

.PHONY: compile-$(2) run-$(2) run-nemu-$(2) clean-$(2)

$$($(2)_APP)-$(ARCH).%: $$($(2)_DEPS)
	@make -s -C $(1) ARCH=$(ARCH) $(4)

compile-$(2): $$($(2)_APP)-$(ARCH).elf \
			  $$($(2)_APP)-$(ARCH).bin \
			  $$($(2)_APP)-$(ARCH).txt
	@mkdir -p $$($(2)_OBJDIR)
	@cd $$($(2)_OBJDIR) && \
		ln -sf $$^ . && \
		rename -f 's/txt$$$$/S/g' *.txt && \
		rename -f 's/-$(ARCH)//g' * \

run-$(2): $(EMU_BIN) compile-$(2)
	@cd $$($(2)_OBJDIR) && \
	  ln -sf $(abspath $(EMU_BIN)) emulator && \
	  ./emulator -b -e ./$(2).elf 2> npc.out

run-nemu-$(2): $(MIPS32_NEMU) compile-$(2)
	@cd $$($(2)_OBJDIR) && \
	  ln -sf $(MIPS32_NEMU) nemu && \
	  ./nemu -b -e ./$(2).elf

clean-$(2):
	@make -s -C $(1) ARCH=$(ARCH) clean
endef
