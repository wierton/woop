.PHONY: clean-nscscc nscscc-func nscscc-perf
.PHONY: nscscc-sync u-boot-coe

nscscc-sync: nscscc-sync-func nscscc-sync-perf nscscc-sync-final

NSCSCC_OBJDIR := $(abspath $(OBJ_DIR)/nscscc)
NSCSCC_SOFTDIR := $(NSCSCC_OBJDIR)/soft
LOONGSON_TOP_V := $(OBJ_DIR)/loongson/LoongsonTop.v

FUNC_COE  := $(NSCSCC_SOFTDIR)/func/obj/inst_ram.coe
GAME_COE  := $(NSCSCC_SOFTDIR)/memory_game/obj/axi_ram.coe
PERF_COE  := $(NSCSCC_SOFTDIR)/perf_func/obj/allbench/axi_ram.coe
U_BOOT_COE := $(NSCSCC_SOFTDIR)/u-boot/u-boot.coe
GOLDEN_TRACE := $(NSCSCC_OBJDIR)/cpu132_gettrace/golden_trace.txt

u-boot-coe: $(U_BOOT_COE)

$(U_BOOT_COE): $(U_BOOT_BIN)
	@mkdir -p $(@D)
	@hexdump -ve '1/4 "%08x\n"' $< > $@
	@sed -i 1i'memory_initialization_radix = 8;\nmemory_initialization_vector =' $@

$(LOONGSON_TOP_V): $(SCALA_FILES)
	@mkdir -p $(@D)
	@sbt "run LOONGSON_TOP -td $(@D) --output-file $(@F)"
	@sed -i "s/_\(aw\|ar\|r\|w\|b\)_/_\1/g" $@

clean-nscscc:
	rm -rf $(NSCSCC_OBJDIR)

# 1: name, 2: dependent .coe file
define nscscc_template =
.PHONY: nscscc-$(1) clean-nscscc-$(1) nscscc-sync-$(1)

$(1)_LS_TOP_V=$$(NSCSCC_OBJDIR)/soc_axi_$(1)/rtl/myCPU/LoongsonTop.v
$(1)_LS_XPR=$$(NSCSCC_OBJDIR)/soc_axi_$(1)/run_vivado/mycpu_prj1/mycpu.xpr

$$($(1)_LS_TOP_V): $$(LOONGSON_TOP_V)
	mkdir -p $$(@D)
	cp $$< $$@

$$($(1)_LS_XPR):
	rm -rf $$(NSCSCC_OBJDIR)/soc_axi_$(1)
	cp -r soc/loongson/soc_axi_$(1) $$(NSCSCC_OBJDIR)

nscscc-$(1)-prj: $$($(1)_LS_XPR) $$($(1)_LS_TOP_V) $(2)

nscscc-sync-$(1): $$($(1)_LS_TOP_V)

nscscc-$(1)-bit: $$($(1)_LS_XPR)
	SOC_XPR=mycpu.xpr SOC_DIR=$$(dir $$($(1)_LS_XPR)) \
	  $(VIVADO_18) $(VIVADO_FLAG) -mode batch -source soc/loongson/mk.tcl

nscscc-$(1)-vivado: $$($(1)_LS_XPR)
	cd $$(<D) && nohup $$(VIVADO_18) $$< &>/dev/null &

clean-nscscc-$(1):
	cd $$(NSCSCC_OBJDIR) && rm -rf soc_axi_$(1)
endef

$(eval $(call nscscc_template,func,$(FUNC_COE) $(GOLDEN_TRACE)))
$(eval $(call nscscc_template,perf,$(PERF_COE)))
$(eval $(call nscscc_template,final,$(U_BOOT_COE)))
