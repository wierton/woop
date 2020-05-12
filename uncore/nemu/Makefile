.PHONY: sim prj

sim-%: $(MIPS32_NEMU) compile-%
	@cd $($(subst sim-,,$@)_OBJDIR) && \
	  $< -b -e ./$(subst sim-,,$@).elf

prj: $(MIPS32_NEMU)
update: ;
clean: ;
