# Maternal Health Effects of Glyphosate  
Effects of glyphosate on maternal health outcomes


`TODO`: 
- [ ] Make sure we are using consistent data source for employment and population 
  - [ ] Employment: Currently getting farm/nonfarm (and total) from the BEA, totals from BLS  
  - [ ] Population: Getting population data from SEER, Census, and BEA  


## Requirements 

We use `GNU Make 4.4.1` to run our analysis pipeline. Our analysis uses `R` version `4.4.1`. We use `renv` for package management. To get started, run `renv::restore()`, which will install the pacakge versions recorded in `renv.lock`. 

Some data were manually downloaded, they are in `data/download-manual`. The rest of the analysis can be run using the following `make` commands:  
- `make data-clean`: Gets data into final combined table to use in analysis   
- `make data-raw`: Does some of the basic reformatting of raw data    
- `make data-download`: Downloads data from USDA NASS, USGS, Census, and BEA  
- `make natality-clean`: Cleans the restricted access natality data  


## Notes

For the shift-share instrument, we exclude all counties upstream--instead of copying over all of the watershed work from the `glyphosate-birthweight` repo, we just pull two files from there: `data/watershed/weights/hydrobasin-area-weights.fst` and `data/watershed/upstream-dt-hydrobasin.fst`, and put them both in `data/download-manual`.