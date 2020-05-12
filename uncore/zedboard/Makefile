.PHONY: clean

ZB_OBJDIR  := $(abspath $(OBJ_DIR)/zedboard)
ZB_TOP_V   := $(ZB_OBJDIR)/src/zedboard_top.v
ZB_XPR     := $(ZB_OBJDIR)/zedboard.xpr

$(ZB_TOP_V): $(CORE_TOP_V)
	@mkdir -p $(@D)
	@cp $< $@
	@sed -i "s/_\(aw\|ar\|r\|w\|b\)_/_\1/g" $@

$(ZB_XPR):
	@rm -rf $(ZB_OBJDIR)
	@cp -r soc/zedboard $(ZB_OBJDIR)

prj: $(ZB_XPR) $(ZB_TOP_V)
	@cd $(dir $(ZB_XPR)) && \
	  nohup $(VIVADO) $(ZB_XPR) &
update: $(ZB_TOP_V)
bit: prj
	@SOC_XPR=mycpu.xpr SOC_DIR=$(dir $(ZB_XPR)) \
	  $(VIVADO) -mode batch -source soc/zedboard/mk.tcl

clean:
	rm -rf $(ZB_OBJDIR)
