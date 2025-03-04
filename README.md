# Maternal Health Effects of Glyphosate  
Effects of glyphosate on maternal health outcomes


`TODO`: 
- [ ] Make sure we are using consistent data source for employment and population 
  - [ ] Employment: Currently getting farm/nonfarm (and total) from the BEA, totals from BLS  
  - [ ] Population: Getting population data from SEER, Census, and BEA  


## Requirements 

We use `GNU Make 4.4.1` to run our analysis pipeline. Our analysis uses `R` version `4.4.1`. We use [`renv`](https://rstudio.github.io/renv/index.html) for package management. To get started, run `renv::restore()`, which will install the pacakge versions recorded in `renv.lock`. 

Some data were manually downloaded, they are in `data/download-manual`. The rest of the analysis can be run using the following `make` commands:  
- `make data-clean`: Gets data into final combined table to use in analysis   
- `make data-raw`: Does some of the basic reformatting of raw data    
- `make data-download`: Downloads data from USDA NASS, USGS, and Census 
- `make natality-clean`: Cleans the restricted access natality data  

Downloading the USDA NASS and Census data require API keys. save them to .Renviron with with `usethis::edit_r_environ()`:

- [USDA QuickStats API](https://quickstats.nass.usda.gov/api/) saved in .Renviron as `NASS_KEY`
- [Census API](https://api.census.gov/data/key_signup.html) saved in .Renviron as `CENSUS_KEY`


## Notes

For the shift-share instrument, we exclude all counties upstream--instead of copying over all of the watershed work from the `glyphosate-birthweight` repo, we just pull two files from there: `data/watershed/weights/hydrobasin-area-weights.fst` and `data/watershed/upstream-dt-hydrobasin.fst`, and put them both in `data/download-manual`.

The BEA [removed the county-level farm/non-farm employment data from their API](https://www.bea.gov/itable/discontinued-data-tables), thus we copy `data/download-script/farm-empl-dt.fst` from the `glyphosate-birthweight` repo and put it into `data/download-manual/farm-empl-dt.fst`.