#-------------------------------------------------------
# Getting data from USDA NASS database
#-------------------------------------------------------
# Loading required packages
library(pacman)
p_load(
  tidyverse, usdarnass, rnassqs, data.table, 
  here, purrr, magrittr, janitor, fst
)

# NASS API key
nassqs_auth(Sys.getenv("NASS_KEY"))

#-------------------------------------------------------
# Survey data (annual)
#-------------------------------------------------------

# Looking for the parameters that we want to get
short_desc_acres_survey_raw = 
  nass_param(
    "short_desc",
    sector_desc = "CROPS",
    agg_level_desc = "COUNTY",
    source_desc = "SURVEY"
  ) 

# Filtering to acres planted, excluding rows that would 
# double count (irrigated + nonirrigated and total)
short_desc_acres_survey = 
  short_desc_acres_survey_raw[
    short_desc_acres_survey_raw |> str_detect("ACRES PLANTED$") &
    !(short_desc_acres_survey_raw |> str_detect("IRRIGATED")) &
    !(short_desc_acres_survey_raw |> str_detect("OIL")) &
    !(short_desc_acres_survey_raw |> str_detect("WHEAT - ")) &
    #!(short_desc_acres_survey_raw |> str_detect("SPRING")) &
      !(short_desc_acres_survey_raw |> str_detect("FOLLOWING ANOTHER CROP"))&
      !(short_desc_acres_survey_raw |> str_detect("CL CHICKPEAS"))
  ]
short_desc_yield_survey = 
  short_desc_acres_survey_raw[
    short_desc_acres_survey_raw |> str_detect("YIELD.*/ ACRE$") &
      !(short_desc_acres_survey_raw |> str_detect("IRRIGATED")) &
      !(short_desc_acres_survey_raw |> str_detect("OIL")) &
    !(short_desc_acres_survey_raw |> str_detect("WHEAT - ")) &
    #!(short_desc_acres_survey_raw |> str_detect("SPRING")) &
      !(short_desc_acres_survey_raw |> str_detect("CL CHICKPEAS")) &
      !(short_desc_acres_survey_raw |> str_detect("ALFALFA"))&
      !(short_desc_acres_survey_raw |> str_detect("FOLLOWING ANOTHER CROP"))
  ]
short_desc_acres_irrigated_survey = 
  short_desc_acres_survey_raw[
    short_desc_acres_survey_raw |> str_detect("ACRES PLANTED$") &
    short_desc_acres_survey_raw |> str_detect("IRRIGATED") &
    short_desc_acres_survey_raw |> str_detect("CORN|SOYBEANS|COTTON")
  ]


# Getting years available for each crop
crop_years =
  map_dfr(
    short_desc_acres_survey,
    \(sda){
        nass_param(
          "year",
          short_desc = sda,
          agg_level_desc = "COUNTY",
          sector_desc = "CROPS"
        ) |> tibble() |>
        mutate(
          short_desc = sda,
          year = `nass_param(...)`
        ) |>
        select(short_desc,year)
    }
  )

crop_years_yield =
  map_dfr(
    short_desc_yield_survey,
    \(sda){
      nass_param(
        "year",
        short_desc = sda,
        agg_level_desc = "COUNTY",
        sector_desc = "CROPS"
      ) |> tibble() |>
        mutate(
          short_desc = sda,
          year = `nass_param(...)`
        ) |>
        select(short_desc,year)
    }
  )

crop_years_irrigated =
  map_dfr(
    short_desc_acres_irrigated_survey,
    \(sda){
      nass_param(
        "year",
        short_desc = sda,
        agg_level_desc = "COUNTY",
        sector_desc = "CROPS"
      ) |> tibble() |>
        mutate(
          short_desc = sda,
          year = `nass_param(...)`
        ) |> 
        select(short_desc,year)
    }
  )

# Filtering to same time as health data and crops not already downloaded
path_survey = "data/download-script/nass-survey/acres-planted"
path_survey_yield = "data/download-script/nass-survey/yield"
path_survey_irrigated = "data/download-script/nass-survey/irrigated"
# Creating directories if they don't exist
map(
  c(path_survey, path_survey_yield, path_survey_irrigated), 
  \(x) if(!dir.exists(here(x))) dir.create(here(x), recursive = TRUE)
)
# Getting list to run--checking for things already run
crop_years_survey = 
  crop_years |> 
  filter(
    year %in% 1985:2017 & 
    !(paste0(short_desc,"-",year,".fst") %in% list.files(path = here(path_survey)))
  ) |>
  as.data.table()
crop_years_survey_yield = 
  crop_years_yield |> 
  filter(
    year %in% 1985:2017 & 
      !(paste0(str_replace_all(short_desc,"/","per"),"-",year,".fst") %in% list.files(path = here(path_survey_yield)))
  ) |>
  as.data.table()
crop_years_survey_irrigated = 
  crop_years_irrigated |> 
  filter(
    year %in% 1985:2017 & 
      !(paste0(str_replace_all(short_desc,"/","per"),"-",year,".fst") %in% list.files(path = here(path_survey_irrigated)))
  ) |>
  as.data.table()
# Combining into one table 
crop_yr_to_run = rbind(
  crop_years_survey[,.(
    sda = short_desc, 
    yr = year, 
    path =  paste0(path_survey,"/",short_desc,"-",year,".fst")
  )], 
  crop_years_survey_yield[,.(
    sda = short_desc, 
    yr = year, 
    path =  paste0(path_survey_yield,"/",str_replace_all(short_desc,"/","per"),"-",year,".fst")
  )], 
  crop_years_survey_irrigated[,.(
    sda = short_desc, 
    yr = year, 
    path =  paste0(path_survey_irrigated,"/",short_desc,"-",year,".fst")
  )]
)
# Function to call NASS API with rate limiting
get_crop_yr = function(sda, yr, path, max_attempts = 5){
  print(paste('Starting', sda, yr))
  # Loop in case there is an error
  for(i in 1:max_attempts){
    # Error handing via tryCatch
    result = tryCatch(
      { # Make API call
        yr_crop = nassqs(
          list(
            short_desc = sda,
            year = yr,
            agg_level_desc = "COUNTY",
            sector_desc = "CROPS"
          ),
          as = "data.frame"
        )
      }, 
      # Return message if there is an error
      error = function(e) return("error")
    )
    # Save results if successful
    if(is.data.frame(result)){
      write.fst(result, path = here(path))
      print('Success')
      break
    }
    # Throw error if we get to max_attempts
    if(i == max_attempts){
      stop(paste("Failed after", max_attempts, "tries."))
    }
    # Increase sleep amount for each failure
    slp = 5*i
    print(paste('Attempt', i, 'failed, sleeping for', slp, 'seconds'))
    Sys.sleep(slp)
  }
}

# Running for everything
pmap(
  .l = crop_yr_to_run, 
  .f = get_crop_yr
)