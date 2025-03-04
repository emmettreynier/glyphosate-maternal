# -----------------------------------------------------------------------------
# Options 
R_OPTS=--vanilla
health-dir := data/health-restricted/
dscr-dir := data/download-script/
raw-dir := data/raw/
clean-dir := data/clean/

# Define variables
YEARS := $(shell seq 1990 2012)
NATALITY_RAW_FP := $(health-dir)raw/NatAC1990.txt #TODO: FILL IN REAL FILENAMES
NATALITY_CLEAN_FP := $(addprefix $(health-clean-dir)period-clean/natality-,$(addsuffix .fst, $(YEARS)))
CROP_NAMES := acre yield irrigated
CROP_DT := $(CROP_NAMES:%=$(raw-dir)all-crop-%-dt.fst)

# Just testing things are set up correctly
test-print:
	@echo $(clean-dir)

# -----------------------------------------------------------------------------
# Setting up higher level targets
all: natality-clean data-download data-raw

# Natality data: unzip -> extract relevant columns by year -> aggregate
natality-clean: \
 $(NATALITY_CLEAN_FP) \
 $(NATALITY_RAW_FP) \
 $(clean-dir)natality-micro.fst
# Downloading from web or API
data-download: \
 $(dscr-dir)usgs-pesticides-raw.fst \
 $(dscr-dir)cnty-area-dt.fst \
 $(dscr-dir)cnty-pop-dt.fst \
 $(dscr-dir)farm-empl-dt.fst \
 download-crops
# Raw data we won't touch very often
data-raw: \
 $(raw-dir)est_pest_use.csv \
 $(raw-dir)est_pest_use.fst \
 $(raw-dir)edu-dt.fst \
 $(raw-dir)labor-dt.fst \
 $(raw-dir)census_crop_acre_county.fst \
 $(raw-dir)y_diff_dt.fst \
 $(CROP_DT) \
 $(raw-dir)fertilizer-dt-interpolated.fst


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
# Targets for data-raw
# Pesticide data
$(raw-dir)est_pest_use.csv \
 $(raw-dir)est_pest_use.fst &: \
 $(dscr-dir)usgs-pesticides-raw.fst \
 R/00-data-prep/farm/01-usgs-chem-data.R 
	Rscript $<
	@echo "Cleaned pesticide data"
# Other dependencies:
# $(dman-dir)est_pest_use_raw_2013_2017.txt

# GAEZ suitability data
$(raw-dir)y_diff_dt.fst &: R/00-data-prep/farm/04-gaez-yield.R 
	Rscript $<
	@echo "Made suitability data"
# Other dependencies: 
# $(wildcard $(dman-dir)/attainaible-yield/yl*r_*.tif)

# BLS labor data
$(raw-dir)labor-dt.fst: R/00-data-prep/controls/02-bls-labor.R
	Rscript $<
	@echo "Made bls labor data"
# Other dependencies $(wildcard $(dman-dir)bls-labforce-raw/*.xlsx)

# Crop data
$(CROP_DT) &: R/00-data-prep/farm/03-crop-county-cleaning.R
	Rscript $<
	@echo "Made crop data"
# Other dependencies: $(wildcard $(dscr-dir)nass-*/*/*.fst) # Recursive?

# Fertilizer data
$(raw-dir)fertilizer-dt-interpolated.fst &: R/00-data-prep/farm/05-fertilizer.R
	Rscript $<
	@echo "Made fertilizer data"
	

# -----------------------------------------------------------------------------
# Helpers
.PHONY: all natality-clean data-download data-raw test-print FORCE
FORCE: