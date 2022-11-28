###########################################################################
# Project directories
###########################################################################

BUILD = .build
CACHE = /var/tmp/fizzbuzz
DEPENDENCIES = $(BUILD)/dependencies

.DEFAULT_GOAL := all

###########################################################################
# Help
###########################################################################

RED    := $(shell tput -Txterm setaf 1)
GREEN  := $(shell tput -Txterm setaf 2)
YELLOW := $(shell tput -Txterm setaf 3)
BLUE   := $(shell tput -Txterm setaf 4)
CYAN   := $(shell tput -Txterm setaf 6)
RESET  := $(shell tput -Txterm sgr0)

COMMAND := ${YELLOW}
TARGET  := ${GREEN}
TEXT    := ${YELLOW}

TARGET_MAX_CHAR_NUM = 20

## Show help
help:
	@echo '${CYAN}Fizzbuzz${RESET} build system'
	@echo ''
	@echo 'Usage:'
	@echo '  ${COMMAND}make${RESET} ${TARGET}<target>${RESET}'
	@echo ''
	@echo 'Targets:'
	@awk '/^[a-zA-Z\-_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "  ${TARGET}%-$(TARGET_MAX_CHAR_NUM)s${RESET} ${TEXT}%s${RESET}\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)

###########################################################################
# Targets
###########################################################################

## Generate the PDF and HTML documentation and test report
all: $(BUILD)/fizzbuzz.html
all: $(BUILD)/fizzbuzz.pdf

## Clean the build directory
clean:
	rm -rf $(BUILD)

## Clean the build directory and remove all dependencies
mrproper: clean
	rm -rf $(CACHE)

.SECONDARY:

###########################################################################
# Project directories
###########################################################################

$(BUILD) $(BUILD)/tests $(CACHE) $(CACHE)/bin $(CACHE)/lib $(CACHE)/tmp $(DEPENDENCIES):
	mkdir -p $@

export PATH := $(CACHE)/bin:$(PATH)

-include $(wildcard $(DEPENDENCIES)/*.d)

###########################################################################
# Tools used by the project
###########################################################################

# LuaX ####################################################################

LUAX_VERSION = master
LUAX_URL = https://github.com/CDSoft/luax
LUAX_TARGET = x86_64-linux-musl
LUAX = $(CACHE)/bin/luax

export PATH := $(dir $(LUAX)):$(PATH)
export LUA_PATH := $(BUILD)/?.lua

$(LUAX): | $(CACHE) $(CACHE)/bin $(CACHE)/tmp
	test -d $(CACHE)/tmp/luax \
	    && ( cd $(CACHE)/tmp/luax && git pull ) \
	    || git clone $(LUAX_URL) $(CACHE)/tmp/luax
	make -C $(CACHE)/tmp/luax install PREFIX=$(dir $@)

# UPP #####################################################################

UPP_VERSION = master
UPP_URL = https://github.com/CDSoft/upp
UPP = $(CACHE)/bin/upp

$(UPP): | $(LUAX) $(CACHE)/bin $(CACHE)/tmp
	test -d $(CACHE)/tmp/upp \
	    && ( cd $(CACHE)/tmp/upp && git pull ) \
	    || git clone $(UPP_URL) $(CACHE)/tmp/upp
	make -C $(CACHE)/tmp/upp install PREFIX=$(dir $@)

# Panda ###################################################################

PANDA_VERSION = master
PANDA_URL = https://github.com/CDSoft/panda
PANDA = $(CACHE)/bin/panda.lua

$(PANDA): | $(CACHE) $(CACHE)/bin $(CACHE)/tmp
	test -d $(CACHE)/tmp/panda \
	    && ( cd $(CACHE)/tmp/panda && git pull ) \
	    || git clone $(PANDA_URL) $(CACHE)/tmp/panda
	( cd $(CACHE)/tmp/panda && make install-all PREFIX=$(dir $@) )

export PANDA_CACHE := $(BUILD)/.panda
export CLASSPATH=$(CACHE)/bin/batik-all.jar

# Pandoc ##################################################################

PANDOC_VERSION = 2.19.2
PANDOC_ARCHIVE = pandoc-$(PANDOC_VERSION)-linux-amd64.tar.gz
PANDOC_URL = https://github.com/jgm/pandoc/releases/download/$(PANDOC_VERSION)/$(PANDOC_ARCHIVE)
PANDOC = $(CACHE)/bin/pandoc

$(PANDOC): | $(CACHE) $(CACHE)/bin $(CACHE)/tmp
	wget $(PANDOC_URL) -O $(CACHE)/tmp/$(notdir $(PANDOC_URL))
	tar -C $(CACHE)/tmp -xzf $(CACHE)/tmp/$(notdir $(PANDOC_URL))
	cp $(CACHE)/tmp/pandoc-$(PANDOC_VERSION)/bin/* $(dir $@)

# Pandoc LaTeX template ###################################################

PANDOC_LATEX_TEMPLATE_URL = https://github.com/Wandmalfarbe/pandoc-latex-template.git
PANDOC_LATEX_TEMPLATE = $(CACHE)/lib/pandoc-latex-template/eisvogel.tex

$(PANDOC_LATEX_TEMPLATE): | $(CACHE)/lib
	mkdir -p $(dir $@)
	test -d $(CACHE)/lib/pandoc-latex-template \
	    && ( cd $(CACHE)/lib/pandoc-latex-template && git pull ) \
	    || git clone $(PANDOC_LATEX_TEMPLATE_URL) $(CACHE)/lib/pandoc-latex-template

# Panam CSS ###############################################################

PANAM_CSS = $(CACHE)/lib/panam/styling.css
PANAM_URL = https://benjam.info/panam/styling.css

$(PANAM_CSS): | $(CACHE)/lib
	mkdir -p $(dir $@)
	wget $(PANAM_URL) -O $@

# Stack ###################################################################

STACK_LTS = lts-19.32
STACK_VERSION = 2.9.1
STACK_ARCHIVE = stack-$(STACK_VERSION)-linux-x86_64.tar.gz
STACK_URL = https://github.com/commercialhaskell/stack/releases/download/v$(STACK_VERSION)/$(STACK_ARCHIVE)
STACK = $(CACHE)/bin/stack

$(STACK): | $(CACHE)/bin $(CACHE)/tmp
	wget $(STACK_URL) -O $(CACHE)/tmp/$(notdir $(STACK_URL))
	tar -C $(CACHE)/tmp -xzf $(CACHE)/tmp/$(notdir $(STACK_URL))
	cp $(CACHE)/tmp/stack-$(STACK_VERSION)-linux-x86_64/stack $@

export PATH := $(dir $(STACK)):$(PATH)

###########################################################################
# Tests
###########################################################################

TEST_RESULTS =  $(BUILD)/tests/result_lua.lua
TEST_RESULTS += $(BUILD)/tests/result_c.lua
TEST_RESULTS += $(BUILD)/tests/result_hs.lua

# Lua test

$(BUILD)/tests/fizzbuzz_lua: fizzbuzz.lua | $(LUAX) $(BUILD)/tests
	$(LUAX) -o $@ $<

$(BUILD)/tests/fizzbuzz_lua.txt: $(BUILD)/tests/fizzbuzz_lua test_config.lua | $(LUAX)
	N=$(shell $(LUAX) -l test_config -e 'print(test_config.N)'); \
	$< $$N > $@ || ( rm -f $@ && false )

# C test

$(BUILD)/tests/fizzbuzz_c: fizzbuzz.c | $(BUILD) $(LUAX)
	gcc $< -o $@

$(BUILD)/tests/fizzbuzz_c.txt: $(BUILD)/tests/fizzbuzz_c test_config.lua | $(LUAX)
	N=$(shell $(LUAX) -l test_config -e 'print(test_config.N)'); \
	$< $$N > $@ || ( rm -f $@ && false )

# Haskell test

$(BUILD)/tests/fizzbuzz_hs: fizzbuzz.hs | $(STACK) $(BUILD)
	$(STACK) --resolver=$(STACK_LTS) ghc -- -outputdir $(@)_tmp $< -o $@

$(BUILD)/tests/fizzbuzz_hs.txt: $(BUILD)/tests/fizzbuzz_hs test_config.lua | $(LUAX)
	N=$(shell $(LUAX) -l test_config -e 'print(test_config.N)'); \
	$< $$N > $@ || ( rm -f $@ && false )

# Check test results

$(BUILD)/tests/result_%.lua: fizzbuzz_test.lua $(BUILD)/tests/fizzbuzz_%.txt test_config.lua | $(LUAX)
	N=$(shell $(LUAX) -l test_config -e 'print(test_config.N)'); \
	$(LUAX) $(wordlist 1,2,$^) $$N > $@ || ( rm -f $@ && false )

###########################################################################
# Documentation
###########################################################################

UPP_FLAGS = -p . -l project_data

PANDOC_FLAGS = -f markdown
PANDOC_FLAGS += --lua-filter $(PANDA)
PANDOC_FLAGS += --embed-resources --standalone
PANDOC_FLAGS += --table-of-content

HTML_FLAGS = $(PANDOC_FLAGS)
HTML_FLAGS += -t html5
HTML_FLAGS += --css $(PANAM_CSS)
HTML_FLAGS += --mathml

PDF_FLAGS = $(PANDOC_FLAGS)
PDF_FLAGS += -t latex
#PDF_FLAGS += --pdf-engine=pdflatex
PDF_FLAGS += --template=$(PANDOC_LATEX_TEMPLATE)
PDF_FLAGS += --number-sections
PDF_FLAGS += --highlight-style tango
PDF_FLAGS += --top-level-division=chapter
#PDF_FLAGS += --listings

export LUA_PATH = $(BUILD)/tests/?.lua;./?.lua
export REQDB = $(BUILD)/reqdb.lua
export REQTARGET = fizzbuzz.pdf

$(BUILD)/%.md: %.md $(TEST_RESULTS) Makefile | $(UPP) $(BUILD) $(DEPENDENCIES)
	$(UPP) $(UPP_FLAGS) -MT $@ -MF $(DEPENDENCIES)/$(notdir $@).upp.d $< -o $@

$(BUILD)/%.html: $(BUILD)/%.md | $(PANDA) $(PANDOC) $(PANAM_CSS) $(DEPENDENCIES)
	PANDA_TARGET=$@ \
	PANDA_DEP_FILE=$(DEPENDENCIES)/$(notdir $@).panda.d \
	$(PANDOC) $(HTML_FLAGS) $< -o $@

$(BUILD)/%.pdf: $(BUILD)/%.md | $(PANDA) $(PANDOC) $(PANDOC_LATEX_TEMPLATE) $(DEPENDENCIES)
	PANDA_TARGET=$@ \
	PANDA_DEP_FILE=$(DEPENDENCIES)/$(notdir $@).panda.d \
	$(PANDOC) $(PDF_FLAGS) $< -o $@
