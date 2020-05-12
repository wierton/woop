.PHONY: sim prj

LS_SRC_DIR   := $(UNCORE_DIR)
LS_OBJDIR    := $(abspath $(CORE_OBJ_DIR))
LS_SOFTDIR   := $(LS_OBJDIR)/soft
U_BOOT_BIN   := $(U_BOOT_HOME)/u-boot.bin
INST_COE     := $(LS_SOFTDIR)/func/obj/inst_ram.coe
GOLDEN_TRACE := $(LS_OBJDIR)/cpu132_gettrace/golden_trace.txt
LS_TOP_V   := $(LS_OBJDIR)/soc_axi_final/rtl/myCPU/LoongsonTop.v
LS_XPR     := $(LS_OBJDIR)/soc_axi_final/run_vivado/mycpu_prj1/mycpu.xpr

$(INST_COE) $(GOLDEN_TRACE):
	@mkdir -p $(@D)
	@touch $@

$(LS_TOP_V): $(CORE_TOP_V)
	@mkdir -p $(@D)
	@cp $< $@
	@sed -i "s/_\(aw\|ar\|r\|w\|b\)_/_\1/g" $@

$(LS_XPR): $(INST_COE) $(GOLDEN_TRACE)
	@rm -rf $(LS_OBJDIR)/soc_axi_final
	@mkdir -p $(LS_OBJDIR)
	@cp -r $(LS_SRC_DIR)/soc_axi_final $(LS_OBJDIR)

sim-%: compile-%

bit: $(LS_XPR)
prj: $(LS_XPR)
	cd $(<D) && $(VIVADO) $(<F)
update: $(LS_TOP_V)
clean:
	rm -Irf $(LS_OBJDIR)
