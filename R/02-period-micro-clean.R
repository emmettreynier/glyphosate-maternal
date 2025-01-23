# Cleaning raw data and saving it as an fst for quick access
# Loading packages
library(pacman)
p_load(
  data.table, readr, here, magrittr, stringr, dplyr,
  tidyr, glue, purrr, tictoc, furrr, fst, skimr, 
  lubridate, tigris
)

# Get state FIPS codes
state_fips = data.table(tigris::fips_codes)[,.(
  state_abb = state, 
  new_state_fips = state_code
)] |> unique()

# Getting the natality data dictionary 
source(here("R/00-data-dictionary.R"))
# Creating folder to put these in 
dest_dir = here('data/health-restricted/period-clean')
if (!dir.exists(dest_dir)) dir.create(dest_dir)

#-------------------------------------------------------------#
# Creating a function that cleans data for a single year
#-------------------------------------------------------------#
clean_natality = function(file_year, dt_out = FALSE){
  
  # Filtering to only dictionary for specific year
  natality_dct_yr = natality_dct[year == file_year]
  # Setting filepath for natality data
  natality_filepath = here(paste0(
    "data/health-restricted/raw/NatAC",file_year,".txt"
  ))
  # Reading Natality File
  natality_raw_dt = read_fwf(
    file = natality_filepath,
    col_positions = fwf_positions(
      start = natality_dct_yr$start_pos, 
      end = natality_dct_yr$end_pos, 
      col_names = natality_dct_yr$var_name
    )
  ) |> as.data.table() 
  
  if(file_year %in% 2003:2013){
    
    natality_raw_dt =
      merge(
        natality_raw_dt,
        state_fips,
        by.x = "state_postal", 
        by.y = "state_abb", 
        all.x = TRUE
      ) |>
      merge(
        state_fips,
        by.x = "state_postal_occ", 
        by.y = "state_abb", 
        all.x = TRUE
      ) %>% 
      .[,':='(
          year = file_year,
          month = dob_mm,
          GEOID = paste0(
            str_pad(new_state_fips.x,2,"left","0"), 
            str_pad(county_fips,3,"left","0")
          ),
          GEOID_occ = paste0(
            str_pad(new_state_fips.y,2,"left","0"), 
            str_pad(county_fips_occ,3,"left","0")
          ),
          meduc = fcase(
            !is.na(meduc), meduc,
            umeduc %in% paste0("0",0:8), 1,
            umeduc %in% c("09",10,11), 2,
            umeduc == "12", 3,
            umeduc %in% 13:15, 4,
            umeduc == 16, 6, 
            umeduc == 17, 7, 
            umeduc == 99, 9
          ),
          mage = fcase(
            file_year >= 2004 & as.numeric(mage) <= 15, 1,
            file_year >= 2004 & as.numeric(mage) > 15, as.numeric(mage) - 13,
            file_year < 2004 & !is.na(mage), as.numeric(mage) 
          ),
        dbwt = ifelse(dbwt == 9999, as.numeric(NA), as.numeric(dbwt)),
        no_city = place_fips == 99999,
        apgar5 = ifelse(apgar5 == 99, as.numeric(NA),as.numeric(apgar5)),
        c_section = fcase(
          delmeth %in% c(3,4,7), TRUE,
          delmeth %in% c(1,2,6), FALSE
        ),
        tobacco = fcase(
          tobacco == '1', TRUE, 
          tobacco_recode == 'Y', TRUE, 
          tobacco %in% c('2','8','9'), FALSE, 
          tobacco_recode %in% c('N','U'), FALSE,
          default = FALSE
        ),
        alcohol = alcohol == '1'
      )]
  }else if(file_year %in% 1990:1991){
    
    # Cleaning variables
    natality_raw_dt[,':='(
      year = file_year,
      month = dob_mm,
      GEOID = paste0(
        str_pad(state_fips,2,"left","0"), 
        str_pad(county_fips,3,"left","0")
      ),
      GEOID_occ = paste0(
        str_pad(state_fips_occ,2,"left","0"), 
        str_pad(county_fips_occ,3,"left","0")
      ),
      mrace = fcase(
        mrace %in% paste0("0",1:3), str_sub(mrace, 2,2),
        mrace %in% paste0("0",4:8), "4",
        mrace == "09", "1"
      ),
      frace = fcase(
        frace %in% paste0("0",1:3), str_sub(frace, 2,2),
        frace %in% paste0("0",4:8), "4",
        frace == "09", "1",
        default = "9"
      ),
      meduc = fcase(
        umeduc %in% paste0("0",0:8), 1,
        umeduc %in% c("09",10,11), 2,
        umeduc == "12", 3,
        umeduc %in% 13:15, 4,
        umeduc == 16, 6, 
        umeduc == 17, 7, 
        umeduc == 99, 9
      ),
      sex = fcase(
        sex == "1", "M",
        sex == "2", "F"
      ),
      dbwt = ifelse(dbwt == 9999, as.numeric(NA), as.numeric(dbwt)),
      no_city = city_res == 999,
      apgar5 = ifelse(apgar5 == 99, as.numeric(NA),as.numeric(apgar5)),
      c_section = fcase(
        delmeth %in% c(3,4), TRUE,
        delmeth %in% c(1,2), FALSE
      ),
      tobacco = tobacco == '1',
      alcohol = alcohol == '1'
    )] 
  }else if(file_year %in% 1992:2002){
    
    # Cleaning variables
    natality_raw_dt[,':='(
      year = file_year,
      month = dob_mm,
      GEOID = paste0(
        str_pad(state_fips,2,"left","0"), 
        str_pad(county_fips,3,"left","0")
      ),
      GEOID_occ = paste0(
        str_pad(state_fips_occ,2,"left","0"), 
        str_pad(county_fips_occ,3,"left","0")
      ),
      mrace = fcase(
        mrace %in% paste0("0",1:3), str_sub(mrace, 2,2),
        mrace %in% c(paste0("0",4:7), paste0(1:7,"8")), "4"
      ),
      frace = fcase(
        frace %in% paste0("0",1:3), str_sub(frace,2,2),
        frace %in% c(paste0("0",4:7), paste0(1:7,"8")), "4",
        default = "9"
      ),
      meduc = fcase(
        umeduc %in% paste0("0",0:8), 1,
        umeduc %in% c("09",10,11), 2,
        umeduc == "12", 3,
        umeduc %in% 13:15, 4,
        umeduc == 16, 6, 
        umeduc == 17, 7, 
        umeduc == 99, 9
      ),
      sex = fcase(
        sex == "1", "M",
        sex == "2", "F"
      ),
      dbwt = ifelse(dbwt == 9999, as.numeric(NA), as.numeric(dbwt)),
      no_city = city_res == 999,
      apgar5 = ifelse(apgar5 == 99, as.numeric(NA),as.numeric(apgar5)),
      c_section = fcase(
        delmeth %in% c(3,4), TRUE,
        delmeth %in% c(1,2), FALSE
      ),
      tobacco = tobacco == '1',
      alcohol = alcohol == '1'
    )] 
  }
  
  # Fixing broken GEOIDS 
  # skimr::skim(natality_raw_dt)
  # merge(
  #   county_year_dt[year == file_year,.(GEOID, year, in_panel = 1)], 
  #   natality_raw_dt[,.(num_births = .N),.(GEOID = GEOID, year)],
  #   by = c('GEOID','year'),
  #   all = TRUE
  # )[
  #   !(str_sub(GEOID, 1,2) %in% c('02','15','66','72','78','NA')) 
  #   & (is.na(in_panel) | is.na(num_births))]
  natality_raw_dt[,':='(
    GEOID = fcase(
      GEOID == '12025', '12086', 
      GEOID == '51780', '51083',
      GEOID == '51560', '51005',
      !is.na(GEOID), GEOID
    ),
    GEOID_occ = fcase(
      GEOID_occ == '12025', '12086', 
      GEOID_occ == '51780', '51083',
      GEOID_occ == '51560', '51005',
      !is.na(GEOID_occ), GEOID_occ
    )
  )]

  # # For 1990-1993 files, have to to special merge to get place_fips
  # if(file_year %in% 1990:1993){
  #   # Loading the list of cities 
  #   city_dt =
  #     read.fst(
  #       path = here('data/download-manual/city-water-dt.fst'),
  #       as.data.table = TRUE
  #     )
  #   # Loading the manual crosswalk
  #   manual_xwalk = 
  #     fread(
  #       here('data/health-restricted/city_res_xwalk.csv')
  #     )[,.(
  #       city_state,
  #       city_res = str_pad(city_res, 3, 'left','0')
  #     )] |>
  #     merge(
  #       city_dt,
  #       by = 'city_state',
  #       all= TRUE
  #     ) %>% .[,.(state_fips, city_res, place_fips)]
  #   natality_raw_dt[,state_fips := str_sub(GEOID, 1,2)]
  #   # Adding place fips to data 
  #   natality_raw_dt = 
  #     merge(
  #       natality_raw_dt[,-'place_fips'], 
  #       manual_xwalk,
  #       by = c('state_fips','city_res'),
  #       all.x = TRUE
  #     )
  # }
  
  # Only saving the columns we will use 
  natality_dt = natality_raw_dt[,.(
    GEOID, GEOID_occ, year, month, sex, dbwt, apgar5, gestation,
    mage, mrace, mhisp, meduc, mar,
    fage,  fhisp, frace, 
    birth_facility, restatus, no_city,
    live_birth_order, total_birth_order,
    c_section, 
    plurality,febrile,meconium,membrane_rupture,abruptio_placentae,placenta_previa,excessive_bleeding,mother_seizure,labor_under_3h,labor_over_20h,labor_dysfunc,breech,cephalopelvic_disproportion,cord_prolapse,anesthetic_comp,fetal_distress,labor_complication,baby_anemia,baby_injury,fetal_alcohol,baby_hyaline,meconoim_aspiration,vent_under_30m,vent_over_30m,baby_seizures,baby_other_abn,anencephaly,spina_bifida,hydrocephalus,microcephalus,baby_other_cent_nerv,heart_malform,baby_other_circ,rectal_atresia,tracheo,omphalocele,baby_other_gastro,malformed_genitals,renal_agenesis,baby_other_urogenital,cleft_lip,polydactyly,club_foot,baby_hernia,baby_other_muscl,downs_syndr,baby_other_chromo,baby_other_cong,tobacco, alcohol, 
    # Maternal Health
    mat_diabetes, 
    mat_chronic_hypertension, 
    mat_gest_hypertension, 
    mat_eclampsia
  )]
  # Saving the results 
  write.fst(
    natality_dt,
    path = here(dest_dir, paste0("natality-",file_year,".fst"))
  )
  # Also returning the results 
  if(dt_out == TRUE){return(natality_dt)}
  rm(natality_dt, natality_raw_dt)
  print(paste(file_year, "done"))
}


#plan(multisession(workers = 4))
map(1990:2013, clean_natality)