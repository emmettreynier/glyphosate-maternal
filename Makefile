# -----------------------------------------------------------------------------
# Options 
R_OPTS=--vanilla
health-dir := data/health-restricted/
clean-dir = data/clean/

# Define variables
YEARS := $(shell seq 1990 2012)
NATALITY_RAW_FP := $(health-dir)raw/NatAC1990.txt #TODO: FILL IN REAL FILENAMES
NATALITY_CLEAN_FP := $(addprefix $(health-clean-dir)period-clean/natality-,$(addsuffix .fst, $(YEARS)))

# Just 
test-print:
	@echo $(NATALITY_CLEAN_FP)

# -----------------------------------------------------------------------------
# Setting up higher level targets
all: natality-clean 
# Natality data: unzip -> extract relevant columns by year -> aggregate
natality-clean: \
 $(NATALITY_CLEAN_FP) \
 $(NATALITY_RAW_FP) \
 $(clean-dir)natality-micro.fst

# Natality data clean ---------------------------------------------------------
# First need to unzip natality files 
$(NATALITY_RAW_FP) : R/01-unzip-data.R 
	Rscript $< 
	@echo "Unzipped natality data"
# Extract data from txt into annual fst files 
$(health-clean-dir)period-clean/natality-%.fst: \
 $(NATALITY_RAW_FP) \
 R/00-data-dictionary.R \
 R/02-period-micro-clean.R
	Rscript $<
	@echo "Cleaned raw natality data"
# Aggregate it into one file 
$(clean-dir)natality-micro.fst: \
 $(NATALITY_CLEAN_FP) \
 R/00-build-natality-twfe.R
	Rscript $<
	@echo "Built natality micro data"

# -----------------------------------------------------------------------------
# Helpers
.PHONY: all natality-clean test-print FORCE
FORCE: