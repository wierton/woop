.PHONY: clean-loongson loongson-func loongson-perf
.PHONY: loongson-sync u-boot-coe

loongson-sync: loongson-sync-func loongson-sync-perf loongson-sync-final

LOONGSON_OBJDIR := $(abspath $(OBJ_DIR)/loongson)
LOONGSON_SOFTDIR := $(LOONGSON_OBJDIR)/soft
LOONGSON_TOP_V := $(OBJ_DIR)/loongson/LoongsonTop.v

U_BOOT_BIN:= $(U_BOOT_HOME)/u-boot.bin
FUNC_COE  := $(LOONGSON_SOFTDIR)/func/obj/inst_ram.coe
GAME_COE  := $(LOONGSON_SOFTDIR)/memory_game/obj/axi_ram.coe
PERF_COE  := $(LOONGSON_SOFTDIR)/perf_func/obj/allbench/axi_ram.coe
U_BOOT_COE := $(LOONGSON_SOFTDIR)/u-boot/u-boot.coe
GOLDEN_TRACE := $(LOONGSON_OBJDIR)/cpu132_gettrace/golden_trace.txt

u-boot-coe: $(U_BOOT_COE)

$(U_BOOT_COE): $(U_BOOT_BIN)
	@mkdir -p $(@D)
	@hexdump -ve '1/4 "%08x\n"' $< > $@
	@sed -i 1i'memory_initialization_radix = 16;\nmemory_initialization_vector =' $@

$(LOONGSON_TOP_V): $(SCALA_FILES)
	@mkdir -p $(@D)
	@sbt "run LOONGSON_TOP -td $(@D) --output-file $(@F)"
	@sed -i "s/_\(aw\|ar\|r\|w\|b\)_/_\1/g" $@

clean-loongson:
	rm -rf $(LOONGSON_OBJDIR)

# 1: name, 2: dependent .coe file
define loongson_template =
.PHONY: loongson-$(1) clean-loongson-$(1) loongson-sync-$(1)
.PHONY: loongson-$(1)-prj loongson-$(1)-vivado

$(1)_LS_TOP_V=$$(LOONGSON_OBJDIR)/soc_axi_$(1)/rtl/myCPU/LoongsonTop.v
$(1)_LS_XPR=$$(LOONGSON_OBJDIR)/soc_axi_$(1)/run_vivado/mycpu_prj1/mycpu.xpr

$$($(1)_LS_TOP_V): $$(LOONGSON_TOP_V) $$($(1)_LS_XPR)
	@mkdir -p $$(@D)
	@cp $$< $$@

$$($(1)_LS_XPR):
	@rm -rf $$(LOONGSON_OBJDIR)/soc_axi_$(1)
	@mkdir -p $$(LOONGSON_OBJDIR)
	@cp -r soc/loongson/soc_axi_$(1) $$(LOONGSON_OBJDIR)

loongson-$(1)-prj: $$($(1)_LS_XPR) $$($(1)_LS_TOP_V) $(2)

loongson-sync-$(1): $$($(1)_LS_TOP_V)

loongson-$(1)-bit: loongson-$(1)-prj
	@SOC_XPR=mycpu.xpr SOC_DIR=$$(dir $$($(1)_LS_XPR)) \
	  $(VIVADO_18) $(VIVADO_FLAG) -mode batch \
	  -source soc/loongson/mk.tcl

loongson-$(1)-vivado: loongson-$(1)-prj
	@cd $$(dir $$($(1)_LS_XPR)) && \
	  nohup $$(VIVADO_18) $$($(1)_LS_XPR) &

clean-loongson-$(1):
	@cd $$(LOONGSON_OBJDIR) && rm -rf soc_axi_$(1)
endef

$(eval $(call loongson_template,func,$(FUNC_COE) $(GOLDEN_TRACE)))
$(eval $(call loongson_template,perf,$(PERF_COE)))
$(eval $(call loongson_template,final,$(U_BOOT_COE)))
