# -----------------------------------------------------------------------------
# Options 
R_OPTS=--vanilla
health-dir := data/health-restricted/
dscr-dir := data/download-script/
clean-dir := data/clean/

# Define variables
YEARS := $(shell seq 1990 2012)
NATALITY_RAW_FP := $(health-dir)raw/NatAC1990.txt #TODO: FILL IN REAL FILENAMES
NATALITY_CLEAN_FP := $(addprefix $(health-clean-dir)period-clean/natality-,$(addsuffix .fst, $(YEARS)))

# Just testing things are set up correctly
test-print:
	@echo $(clean-dir)

# -----------------------------------------------------------------------------
# Setting up higher level targets
all: data-download natality-clean 

# Downloading from web or API
data-download: \
 $(dscr-dir)usgs-pesticides-raw.fst \
 $(dscr-dir)cnty-area-dt.fst \
 $(dscr-dir)cnty-pop-dt.fst \
 $(dscr-dir)farm-empl-dt.fst \
 download-crops

# Natality data: unzip -> extract relevant columns by year -> aggregate
natality-clean: \
 $(NATALITY_CLEAN_FP) \
 $(NATALITY_RAW_FP) \
 $(clean-dir)natality-micro.fst

# Natality data clean ---------------------------------------------------------
# First need to unzip natality files 
$(NATALITY_RAW_FP): R/00-data-prep/natality/01-unzip-data.R 
	Rscript $< 
	@echo "Unzipped natality data"
# Extract data from txt into annual fst files 
$(health-clean-dir)period-clean/natality-%.fst: \
 $(NATALITY_RAW_FP) \
 R/00-data-prep/natality/00-data-dictionary.R \
 R/00-data-prep/natality/02-period-micro-clean.R
	Rscript $<
	@echo "Cleaned raw natality data"
# Aggregate it into one file 
$(clean-dir)natality-micro.fst: \
 $(NATALITY_CLEAN_FP) \
 R/00-data-prep/natality/03-build-natality-twfe.R
	Rscript $<
	@echo "Built natality micro data"

# -----------------------------------------------------------------------------
# Targets for data-download

# Dowloading pesticide data from earlier years 
$(dscr-dir)usgs-pesticides-raw.fst: R/00-data-prep/farm/01a-usgs-chem-download.R
	Rscript $<
	@echo "Downloaded pesticide data"

# USDA NASS data
download-crops: R/00-data-prep/farm/02-usda-nass-data.R
	Rscript $<
	@echo "Downloaded USDA NASS"

# County population from census and county area
$(dscr-dir)cnty-area-dt.fst \
 $(dscr-dir)cnty-pop-dt.fst &: \
 R/00-data-prep/controls/00-cnty-pop-area.R
	Rscript $<
	@echo "Calculated county area and pop"

# BEA employment
$(dscr-dir)farm-empl-dt.fst: R/00-data-prep/controls/01-bea-empl.R
	Rscript $<
	@echo "Downloaded BEA data"

# -----------------------------------------------------------------------------
# Helpers
.PHONY: all natality-clean test-print FORCE
FORCE: