# -----------------------------------------------------------------------------
# Options 
R_OPTS=--vanilla
health-dir := data/health-restricted/
dscr-dir := data/download-script/
raw-dir := data/raw/
clean-dir := data/clean/

# Define variables
YEARS := $(shell seq 1990 2012)
NATALITY_RAW_FP := $(addprefix $(health-dir)raw/NatAC,$(addsuffix .txt, $(YEARS)))
NATALITY_CLEAN_FP := $(addprefix $(health-dir)period-clean/natality-,$(addsuffix .fst, $(YEARS)))
CROP_NAMES := acre yield irrigated
CROP_DT := $(CROP_NAMES:%=$(raw-dir)all-crop-%-dt.fst)
# SEER files 
SEER_SEX := all male female
SEER_YR := 1990 1969
SEER_DT := $(foreach sex, $(SEER_SEX), $(foreach yr, $(SEER_YR), $(raw-dir)seer-shares-$(sex)pop-$(yr)-2022.fst))

# Just testing things are set up correctly
test-print:
	@echo $(NATALITY_CLEAN_FP)

# -----------------------------------------------------------------------------
# Setting up higher level targets
all: natality-clean data-download data-raw data-clean

# Natality data: unzip -> extract relevant columns by year -> aggregate
natality-clean: \
 $(NATALITY_CLEAN_FP) \
 $(NATALITY_RAW_FP) \
 $(clean-dir)natality-micro.fst
# Downloading from web or API
data-download: \
 $(dscr-dir)usgs-pesticides-raw.fst \
 $(dscr-dir)cnty-area-dt.fst \
 $(dscr-dir)cnty-pop-dt.fst
# Raw data we won't touch very often
data-raw: \
 $(raw-dir)est_pest_use.csv \
 $(raw-dir)est_pest_use.fst \
 $(raw-dir)edu-dt.fst \
 $(raw-dir)labor-dt.fst \
 $(raw-dir)census_crop_acre_county.fst \
 $(raw-dir)y_diff_dt.fst \
 $(raw-dir)fertilizer-dt-interpolated.fst \
 $(CROP_DT)
# Intermediate files that may get updated often
data-clean: \
 $(clean-dir)comb-cnty-dt.fst \
 $(clean-dir)crop-acre-percentile-90-95.fst \
 $(clean-dir)trt-dt.fst \
 $(clean-dir)glyph-nat-dt.fst \
 $(SEER_DT)

# Natality data clean ---------------------------------------------------------
# First need to unzip natality files 
$(NATALITY_RAW_FP) &: R/00-data-prep/natality/01-unzip-data.R 
	Rscript $< 
	@echo "Unzipped natality data"
# Extract data from txt into annual fst files 
$(NATALITY_CLEAN_FP) &: \
 R/00-data-prep/natality/02-period-micro-clean.R \
 R/00-data-prep/natality/00-data-dictionary.R \
 $(NATALITY_RAW_FP)
	Rscript $<
	@echo "Cleaned raw natality data"
# Aggregate it into one file 
$(clean-dir)natality-micro.fst: \
 R/00-data-prep/natality/03-build-natality-twfe.R \
 $(NATALITY_CLEAN_FP)
	Rscript $<
	@echo "Built natality micro data"

# -----------------------------------------------------------------------------
# Targets for data-download

# Dowloading pesticide data from earlier years 
$(dscr-dir)usgs-pesticides-raw.fst: R/00-data-prep/farm/01a-usgs-chem-download.R
	Rscript $<
	@echo "Downloaded pesticide data"

# USDA NASS crop data 
$(CROP_DT) &: \
 R/00-data-prep/farm/02-usda-nass-data.R \
 R/00-data-prep/farm/03-crop-county-cleaning.R \
 $(dscr-dir)cnty-area-dt.fst
	Rscript R/00-data-prep/farm/02-usda-nass-data.R
	@echo "Downloaded USDA NASS"
	Rscript R/00-data-prep/farm/03-crop-county-cleaning.R
	@echo "Made crop data"

# County population from census and county area
$(dscr-dir)cnty-area-dt.fst \
 $(dscr-dir)cnty-pop-dt.fst &: \
 R/00-data-prep/controls/00-cnty-pop-area.R
	Rscript $<
	@echo "Calculated county area and pop"

# -----------------------------------------------------------------------------
# Targets for data-raw
# Pesticide data
$(raw-dir)est_pest_use.csv \
 $(raw-dir)est_pest_use.fst &: \
 R/00-data-prep/farm/01-usgs-chem-data.R \
 $(dscr-dir)usgs-pesticides-raw.fst
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

# Fertilizer data
$(raw-dir)fertilizer-dt-interpolated.fst &: R/00-data-prep/farm/05-fertilizer.R
	Rscript $<
	@echo "Made fertilizer data"

# -----------------------------------------------------------------------------
# Targets for data-clean

# SEER data 
$(SEER_DT) &: R/00-data-prep/controls/06-seer-pop.R
	Rscript $<
	@echo "Made SEER data"

# Treatment definitions
$(clean-dir)trt-dt.fst: \
 R/01-data-clean/00-define-treatment.R \
 $(raw-dir)y_diff_dt.fst
	Rscript $<
	@echo "Made treatment definitions"	

# Pre period acreage percentiles
$(clean-dir)crop-acre-percentile-90-95.fst: \
 R/01-data-clean/01-crop-acre-percentiles.R \
 $(raw-dir)all-crop-acre-dt.fst \
 $(clean-dir)trt-dt.fst
	Rscript $<
	@echo "Made crop acreage percentiles"
# $(dscr-dir)cnty-area-dt.fst \

# Shift share instruments 
$(clean-dir)glyph-nat-dt.fst \
 $(water-dir)glyph-nat-watershed-dt.fst &: \
 R/01-data-clean/02-glyph-national.R \
 $(raw-dir)est_pest_use.fst
	Rscript $<
	@echo "Made shift share instruments"
# Other dependencies: 
# $(dman-dir)hydrobasin-area-weights.fst 
# $(dman-dir)upstream-dt-hydrobasin.fst 

# Combining all county data into one table
$(clean-dir)comb-cnty-dt.fst: \
 R/01-data-clean/03-combine-cnty.R \
 $(raw-dir)est_pest_use.fst \
 $(raw-dir)labor-dt.fst \
 $(raw-dir)fertilizer-dt-interpolated.fst \
 $(clean-dir)trt-dt.fst
	Rscript $<
	@echo "Made comb-cnty-dt"
# Other dependencies: 
# $(CROP_DT) \
# $(dman-dir)ruralurbancodes2003.xls
# $(dman-dir)farm-empl-dt.fst
# $(dscr-dir)cnty-area-dt.fst \
# $(dscr-dir)cnty-pop-dt.fst \

# -----------------------------------------------------------------------------
# Helpers
.PHONY: natality-clean data-download data-raw data-clean test-print FORCE
FORCE:
