include rules/test-template.mk

INPUT ?= REF # for microbench, another mode is TEST

AM_TESTS := $(filter-out cputests,$(shell ls $(AM_HOME)/tests))
AM_APPS != ls $(AM_HOME)/apps
MIPS_TESTS != ls $(MIPS_TEST_HOME)

# AM apps
$(foreach app,$(AM_APPS),$(eval $(call test_template,$(AM_HOME)/apps/$(app),$(app),)))

# AM tests
$(foreach app,$(AM_TESTS),$(eval $(call test_template,$(AM_HOME)/tests/$(app),$(app),)))

# mipstest
$(foreach app,$(MIPS_TESTS),$(eval $(call test_template,$(MIPS_TEST_HOME)/$(app),$(app),)))

# nanos
# $(eval $(call test_template,$(NANOS_HOME),nanos,))

# cputests
.PHONY: clean-cputests compile-cputests run-cputests run-nemu-cputests

clean-cputests:
	@make -s -C $(AM_HOME)/tests/cputest clean

CPUTESTS := $(basename $(notdir $(shell find $(AM_HOME)/tests/cputest -name "*.c")))

compile-cputests: $(addprefix compile-,$(CPUTESTS))
run-cputests: $(addprefix run-,$(CPUTESTS))
run-nemu-cputests: $(addprefix run-nemu-,$(CPUTESTS))

$(foreach c,$(CPUTESTS),$(eval $(call test_template,$(AM_HOME)/tests/cputest,$(c),$(OBJ_DIR)/cputests/$(c),ALL=$(c))))

.PHONY: run-tests run-nemu-tests clean-tests

run-tests: run-cputests run-microbench run-insttest
run-nemu-tests: run-nemu-cputests run-nemu-microbench run-nemu-insttest
clean-tests: clean-cputests clean-microbench clean-insttest
