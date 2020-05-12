.PHONY: sim-% project update vivado bitstream clean

LS_SRC_DIR := $(UNCORE_DIR)
LS_OBJDIR  := $(abspath $(CORE_OBJ_DIR))
LS_PRJDIR  := $(LS_OBJDIR)/soc_axi_final
LS_SOFTDIR := $(LS_OBJDIR)/soft
U_BOOT_BIN := $(U_BOOT_HOME)/u-boot.bin
U_BOOT_COE := $(LS_OBJDIR)/soft/u-boot/u-boot.coe
BRAM_COE   := $(LS_PRJDIR)/testbench/bram.coe
DDR_COE    := $(LS_PRJDIR)/testbench/ddr.coe
TRACE      := $(LS_PRJDIR)/testbench/trace.txt
LS_TOP_V   := $(LS_PRJDIR)/rtl/myCPU/loongson_top.v
LS_XPR     := $(LS_PRJDIR)/run_vivado/mycpu_prj1/mycpu.xpr

$(U_BOOT_COE): $(U_BOOT_BIN)
	mkdir -p $(@D)
	hexdump -ve 1/4\ \"%08x\\n\" $< | \
	  sed 1i'memory_initialization_radix = 16;\nmemory_initialization_vector =' > $@

$(DDR_COE) $(BRAM_COE) $(TRACE):
	@mkdir -p $(@D)
	@touch $@

$(LS_TOP_V): $(CORE_TOP_V)
	@mkdir -p $(@D)
	@cp $< $@
	@sed -i "s/_\(aw\|ar\|r\|w\|b\)_/_\1/g" $@

$(LS_XPR): $(U_BOOT_COE) $(DDR_COE) $(BRAM_COE) $(TRACE)
	@rm -rf $(LS_PRJDIR)
	@mkdir -p $(LS_OBJDIR)
	@cp -r $(LS_SRC_DIR)/soc_axi_final $(LS_OBJDIR)

sim-%: compile-%

project: $(LS_XPR) $(LS_TOP_V)
update: $(LS_TOP_V)
vivado: project
	@cd $(dir $(LS_XPR)) && nohup $(VIVADO) $(LS_XPR) &
bitstream: project
	@SOC_XPR=mycpu.xpr SOC_DIR=$(dir $(ZB_XPR)) \
	  $(VIVADO) -mode batch -source $(UNCORE_DIR)/mk.tcl
clean:
	rm -Irf $(LS_OBJDIR)
