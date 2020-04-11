.PHONY: clean-zedboard zedboard-sync

ZEDBOARD_OBJDIR  := $(abspath $(OBJ_DIR)/zedboard)
ZEDBOARD_TOP_V   := $(ZEDBOARD_OBJDIR)/src/ZedboardTop.v
ZEDBOARD_XPR     := $(ZEDBOARD_OBJDIR)/zedboard.xpr

$(ZEDBOARD_TOP_V): $(SCALA_FILES)
	@mkdir -p $(@D)
	@$(SBT) "run ZEDBOARD_TOP -td $(@D) --output-file $(@F)"
	@sed -i "s/_\(aw\|ar\|r\|w\|b\)_/_\1/g" $@

.PHONY: zedboard clean-zedboard zedboard-sync
.PHONY: zedboard-prj zedboard-vivado

$(ZEDBOARD_XPR):
	@rm -rf $(ZEDBOARD_OBJDIR)
	@cp -r soc/zedboard $(ZEDBOARD_OBJDIR)

zedboard-prj: $(ZEDBOARD_XPR) $(ZEDBOARD_TOP_V)

zedboard-sync: $(ZEDBOARD_TOP_V)

zedboard-bit: zedboard-prj
	@SOC_XPR=mycpu.xpr SOC_DIR=$(dir $(ZEDBOARD_XPR)) \
	  $(VIVADO) -mode batch -source soc/zedboard/mk.tcl

zedboard-vivado: zedboard-prj
	@cd $(dir $(ZEDBOARD_XPR)) && \
	  nohup $(VIVADO) $(ZEDBOARD_XPR) &

clean-zedboard:
	rm -rf $(ZEDBOARD_OBJDIR)
