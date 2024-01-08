include ../Makefile.base

TESTFILE ?= TestExtractAndPrependPipeOut.bsv
TOPMODULE ?= mkTestHeaderAndDataStreamConversion

ROOT_DIR = $(abspath ../)

SIMSCRIPT = $(BUILDDIR)/$(TOPMODULE).sh

CRC_SCRIPTS_DIR = $(ROOT_DIR)/libs/blue-ethernet/scripts
CRC_TAB_SCRIPT = $(ROOT_DIR)/libs/blue-crc/scripts/gen_crc_tab.py

test_host_memory.hex:
	python3 generate_test_host_memory.py

crc_table:
	python3 $(CRC_TAB_SCRIPT) $(CRC_SCRIPTS_DIR)/crc_ieee_32_256.json ./


compile:
	mkdir -p $(BUILDDIR)
	bsc -elab -sim -verbose $(BLUESIMFLAGS) $(DEBUGFLAGS) $(DIRFLAGS) $(MISCFLAGS) $(RECOMPILEFLAGS) $(RUNTIMEFLAGS) $(SCHEDFLAGS) $(TRANSFLAGS) -g $(TOPMODULE) $(TESTFILE)

link: compile
	bsc -sim $(BLUESIMFLAGS) $(DIRFLAGS) $(RECOMPILEFLAGS) $(SCHEDFLAGS) $(TRANSFLAGS) -e $(TOPMODULE) -o $(SIMSCRIPT)

simulate: crc_table link test_host_memory.hex 
	$(SIMSCRIPT)

clean:
	rm -rf $(BUILDDIR)

.PHONY: compile link simulate clean gen_mock_host_memory
.DEFAULT_GOAL := simulate
