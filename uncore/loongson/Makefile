.PHONY: sim-% project update vivado bitstream clean

LS_SRC_DIR := $(UNCORE_DIR)
LS_OBJDIR  := $(abspath $(CORE_OBJ_DIR))
LS_PRJDIR  := $(LS_OBJDIR)/soc_axi_final
LS_SOFTDIR := $(LS_OBJDIR)/soft
U_BOOT_BIN := $(U_BOOT_HOME)/u-boot.bin
U_BOOT_COE := $(LS_OBJDIR)/soft/u-boot/u-boot.coe
BRAM_COE   := $(LS_PRJDIR)/testbench/bram.coe
DDR_COE    := $(LS_PRJDIR)/testbench/ddr.coe
INST_COE   := $(LS_PRJDIR)/testbench/inst_ram.coe
TRACE      := $(LS_PRJDIR)/testbench/trace.txt
LS_TOP_V   := $(LS_PRJDIR)/rtl/myCPU/loongson_top.v
TRAP_V     := $(LS_PRJDIR)/testbench/trap.v
SIM_TOP_V  := $(LS_PRJDIR)/testbench/sim_top.v
LS_XPR     := $(LS_PRJDIR)/run_vivado/mycpu_prj1/mycpu.xpr

$(U_BOOT_COE): $(U_BOOT_BIN)
	mkdir -p $(@D)
	$(call bin2coe,$<,$@)

$(DDR_COE) $(BRAM_COE) $(TRACE) $(INST_COE):
	@mkdir -p $(@D)
	@touch $@

$(TRAP_V): lib/sim/trap.v
$(SIM_TOP_V): lib/sim/sim_top.v
$(TRAP_V) $(SIM_TOP_V):
	@mkdir -p $(@D)
	@cp $< $@

$(LS_TOP_V): $(CORE_TOP_V)
	@mkdir -p $(@D)
	@cp $< $@
	@sed -i "s/_\(aw\|ar\|r\|w\|b\)_/_\1/g" $@

$(LS_XPR): 
	@rm -rf $(LS_PRJDIR)
	@mkdir -p $(LS_OBJDIR)
	@cp -r $(LS_SRC_DIR)/soc_axi_final $(LS_OBJDIR)

sim-%: $(ELF2COE) project compile-%
	@mkdir -p $(dir $(DDR_COE))
	@mkdir -p $(dir $(BRAM_COE))
	@cd $($(subst sim-,,$@)_OBJDIR); \
	  $(abspath $<) -e $(subst sim-,,$@).elf \
		-s $(DDR_COE):0x80000000:1048576 \
		-s $(BRAM_COE):0xbfc00000:1048576 \
	@$(MIPS32_NEMU) -b -e $(subst sim-,,$@).elf -c 2> $(TRACE)

project: $(LS_XPR) $(LS_TOP_V) $(U_BOOT_COE) $(INST_COE) \
  $(DDR_COE) $(BRAM_COE) $(TRACE) $(TRAP_V) $(SIM_TOP_V)
update: $(LS_TOP_V)
vivado: project
	@cd $(dir $(LS_XPR)) && nohup $(VIVADO) $(LS_XPR) &

bitstream: project
	@SOC_XPR=mycpu.xpr SOC_DIR=$(dir $(ZB_XPR)) \
	  $(VIVADO) -mode batch -source $(UNCORE_DIR)/mk.tcl

clean:
	rm -Irf $(LS_OBJDIR)
