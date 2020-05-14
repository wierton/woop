.PHONY: run-% prj

run-%: $(MIPS32_NEMU) compile-%
	@cd $($*_OBJDIR) && $< -b -e ./$*.elf

project: $(MIPS32_NEMU)
update: ;
clean: ;
