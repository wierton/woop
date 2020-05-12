include rules/test-template.mk

# for microbench
ifeq ($(UNCORE),nemu)
INPUT ?= REF
else
INPUT ?= TEST
endif

AM_TESTS   != ls $(AM_HOME)/tests
AM_TESTS   := $(filter-out cputests,$(AM_TESTS))
AM_APPS    != ls $(AM_HOME)/apps
MIPS_TESTS != ls $(MIPS_TEST_HOME)
CPUTESTS   != find $(AM_HOME)/tests/cputest -name "*.c"
CPUTESTS   := $(basename $(notdir $(CPUTESTS)))

# AM apps
$(foreach app,$(AM_APPS),$(eval $(call test_template,$(AM_HOME)/apps/$(app),$(app),)))

# AM tests
$(foreach app,$(AM_TESTS),$(eval $(call test_template,$(AM_HOME)/tests/$(app),$(app),)))

# mipstest
$(foreach app,$(MIPS_TESTS),$(eval $(call test_template,$(MIPS_TEST_HOME)/$(app),$(app),)))

# nanos
# $(eval $(call test_template,$(NANOS_HOME),nanos,))

# cputests
$(foreach c,$(CPUTESTS),$(eval $(call test_template,$(AM_HOME)/tests/cputest,$(c),$(OBJ_DIR)/cputests/$(c),ALL=$(c))))

.PHONY: clean-cputests %-cputests

clean-cputests:
	@make -s -C $(AM_HOME)/tests/cputest clean

sim-cputests: $(foreach c,$(CPUTESTS),sim-$(c))
