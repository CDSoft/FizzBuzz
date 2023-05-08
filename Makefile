###########################################################################
# Load makex rules (luax, upp, panda, ...)
###########################################################################

include makex.mk

###########################################################################
# Project directories
###########################################################################

BUILD = .build
DEPENDENCIES = $(BUILD)/dependencies

.DEFAULT_GOAL := all

ADD_MAKEFILE_TO_DEPENDENCIES = NO

###########################################################################
# Help
###########################################################################

welcome:
	@echo '${CYAN}Fizzbuzz${RESET} build system'

###########################################################################
# Targets
###########################################################################

ifeq ($(ADD_MAKEFILE_TO_DEPENDENCIES),YES)
MAKEFILEDEP = Makefile
endif

## Generate the PDF and HTML documentation, test report and slideshow
all: $(BUILD)/fizzbuzz.html
all: $(BUILD)/fizzbuzz.pdf
all: $(BUILD)/fizzbuzz_slideshow.pdf

## Clean the build directory
clean:
	rm -rf $(BUILD)

## Clean the build directory and remove all dependencies
mrproper: clean makex-clean

.SECONDARY:

###########################################################################
# Project directories
###########################################################################

$(BUILD) $(BUILD)/tests $(DEPENDENCIES):
	@mkdir -p $@

-include $(wildcard $(DEPENDENCIES)/*.d)

###########################################################################
# Tests
###########################################################################

TEST_RESULTS =  $(BUILD)/tests/result_lua.lua
TEST_RESULTS += $(BUILD)/tests/result_c.lua
TEST_RESULTS += $(BUILD)/tests/result_hs.lua

# Lua test

$(BUILD)/tests/fizzbuzz_lua: fizzbuzz.lua | $(LUAX) $(BUILD)/tests
	@echo '${MAKEX_COLOR}[LUAX]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@$(LUAX) -o $@ $<

$(BUILD)/tests/fizzbuzz_lua.txt: $(BUILD)/tests/fizzbuzz_lua test_config.lua | $(LUAX)
	@echo '${MAKEX_COLOR}[TEST]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@N=$(shell $(LUAX) -l test_config -e 'print(test_config.N)'); \
	$< $$N > $@ || ( rm -f $@ && false )

# C test

$(BUILD)/tests/fizzbuzz_c: fizzbuzz.c | $(BUILD) $(LUAX)
	@echo '${MAKEX_COLOR}[CC]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@gcc $< -o $@

$(BUILD)/tests/fizzbuzz_c.txt: $(BUILD)/tests/fizzbuzz_c test_config.lua | $(LUAX)
	@echo '${MAKEX_COLOR}[TEST]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@N=$(shell $(LUAX) -l test_config -e 'print(test_config.N)'); \
	$< $$N > $@ || ( rm -f $@ && false )

# Haskell test

$(BUILD)/tests/fizzbuzz_hs: fizzbuzz.hs | $(STACK) $(BUILD)
	@echo '${MAKEX_COLOR}[GHC]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@$(STACK_CMD) ghc -- -outputdir $(@)_tmp $< -o $@

$(BUILD)/tests/fizzbuzz_hs.txt: $(BUILD)/tests/fizzbuzz_hs test_config.lua | $(LUAX)
	@echo '${MAKEX_COLOR}[TEST]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@N=$(shell $(LUAX) -l test_config -e 'print(test_config.N)'); \
	$< $$N > $@ || ( rm -f $@ && false )

# Check test results

$(BUILD)/tests/result_%.lua: fizzbuzz_test.lua $(BUILD)/tests/fizzbuzz_%.txt test_config.lua | $(LUAX)
	@echo '${MAKEX_COLOR}[CHECK]${NORMAL} ${TARGET_COLOR}$(word 2,$^) -> $@${NORMAL}'
	@N=$(shell $(LUAX) -l test_config -e 'print(test_config.N)'); \
	$(LUAX) $(wordlist 1,2,$^) $$N > $@ || ( rm -f $@ && false )

###########################################################################
# Documentation
###########################################################################

UPP_FLAGS = -p . -l project_data

PANDOC_FLAGS += --table-of-content

HTML_FLAGS = $(PANDOC_FLAGS)
HTML_FLAGS += --mathml

PDF_FLAGS = $(PANDOC_FLAGS)
PDF_FLAGS += --number-sections
PDF_FLAGS += --highlight-style tango
PDF_FLAGS += --top-level-division=chapter

LOGO_PDF = $(BUILD)/logo.pdf
LOGO_HTML = $(BUILD)/logo.svg

$(LOGO_PDF): logo.lua | $(LSVG) $(BUILD)
	@echo '${MAKEX_COLOR}[LSVG]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@$(LSVG) $^ $@

$(LOGO_HTML): logo.lua | $(LSVG) $(BUILD)
	@echo '${MAKEX_COLOR}[LSVG]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@$(LSVG) $^ $@

export LUA_PATH = $(BUILD)/tests/?.lua;./?.lua
export REQDB = $(BUILD)/reqdb.lua
export REQTARGET = fizzbuzz.pdf

$(BUILD)/%.md: %.md $(TEST_RESULTS) $(MAKEFILEDEP) | $(UPP) $(BUILD) $(DEPENDENCIES)
	@echo '${MAKEX_COLOR}[UPP]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@$(UPP) $(UPP_FLAGS) -MT $@ -MF $(DEPENDENCIES)/$(notdir $@).upp.d $< -o $@

$(BUILD)/%.html: $(BUILD)/%.md $(LOGO_HTML) | $(PANDA) $(DEPENDENCIES)
	@echo '${MAKEX_COLOR}[PANDA]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@PANDA_TARGET=$@ \
	PANDA_DEP_FILE=$(DEPENDENCIES)/$(notdir $@).panda.d \
	LOGO=$(LOGO_HTML) \
	$(PANDA_HTML) $(HTML_FLAGS) $< -o $@

$(BUILD)/%.pdf: $(BUILD)/%.md $(LOGO_PDF) | $(PANDA) $(DEPENDENCIES)
	@echo '${MAKEX_COLOR}[PANDA]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@PANDA_TARGET=$@ \
	PANDA_DEP_FILE=$(DEPENDENCIES)/$(notdir $@).panda.d \
	LOGO=$(LOGO_PDF) \
	$(PANDA_PDF) $(PDF_FLAGS) $< -o $@

###########################################################################
# Slideshow
###########################################################################

$(BUILD)/%_slideshow.pdf: $(BUILD)/%_slideshow.md $(LOGO_PDF) | $(PANDA) $(DEPENDENCIES)
	@echo '${MAKEX_COLOR}[PANDA]${NORMAL} ${TARGET_COLOR}$< -> $@${NORMAL}'
	@PANDA_TARGET=$@ \
	PANDA_DEP_FILE=$(DEPENDENCIES)/$(notdir $@).panda.d \
	LOGO=$(LOGO_PDF) \
	$(PANDA_BEAMER) $< -o $@
