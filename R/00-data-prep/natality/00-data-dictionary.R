  
pacman::p_load(data.table, dplyr, here)
# Creating directory to save things in 
if(!dir.exists(here('data/health-restricted'))){
  dir.create(here('data/health-restricted'), recursive = TRUE)
}
# This script creates the data dictionary for natality data
natality_dct_1992 = 
  data.table(
    year = 1992, 
    var_name = c(
      "dob_yy", "dob_mm", 
      "state_fips", "county_fips",
      "state_fips_occ", "county_fips_occ",
      "birth_facility", "mage", 
      "city_res","place_fips", "restatus",
      "mrace", "mhisp",
      "mar", 
      "umeduc",
      "fage",
      "frace","fhisp",
      "live_birth_order", "total_birth_order",
      "apgar5", "sex",
      "gestation", "dbwt",
      "delmeth",
      # New indicator variables 
      "plurality","febrile","meconium","membrane_rupture","abruptio_placentae","placenta_previa","excessive_bleeding","mother_seizure","labor_under_3h","labor_over_20h","labor_dysfunc","breech","cephalopelvic_disproportion","cord_prolapse","anesthetic_comp","fetal_distress","labor_complication","baby_anemia","baby_injury","fetal_alcohol","baby_hyaline","meconoim_aspiration","vent_under_30m","vent_over_30m","baby_seizures","baby_other_abn","anencephaly","spina_bifida","hydrocephalus","microcephalus","baby_other_cent_nerv","heart_malform","baby_other_circ","rectal_atresia","tracheo","omphalocele","baby_other_gastro","malformed_genitals","renal_agenesis","baby_other_urogenital","cleft_lip","polydactyly","club_foot","baby_hernia","baby_other_muscl","downs_syndr","baby_other_chromo","baby_other_cong",
      # Tobacco and alcohol
      'tobacco', 'alcohol',
      # Maternal Health 
      'mat_diabetes','mat_chronic_hypertension', 'mat_gest_hypertension', 'mat_eclampsia'
    ),
    var_desc = c(
      "birth year", "birth month", 
      "residece state fips", "residence county fips",
      "occurence state fips", "occurence county fips",
      "birth place", "mother's age (recode 36)",
      "city of residence (mother)", 'place fips (mother residence)', "residence status",
      "mother's race", "mother hispanic origin",
      "mother's marital status", 
      "mother's education detail",
      "father's age (recode 11)",
      "father's race recode", "father hispanic origin",
      "Sum of all previous live births", "Sum of all previous pregnancies",
      "Five minute APGAR score", "sex of infant",
      "Computed gestation", "Birthweight",
      "Delivery method",
      "plurality","febrile","meconium","membrane_rupture","abruptio_placentae","placenta_previa","excessive_bleeding","mother_seizure","labor_under_3h","labor_over_20h","labor_dysfunc","breech","cephalopelvic_disproportion","cord_prolapse","anesthetic_comp","fetal_distress","labor_complication","baby_anemia","baby_injury","fetal_alcohol","baby_hyaline","meconoim_aspiration","vent_under_30m","vent_over_30m","baby_seizures","baby_other_abn","anencephaly","spina_bifida","hydrocephalus","microcephalus","baby_other_cent_nerv","heart_malform","baby_other_circ","rectal_atresia","tracheo","omphalocele","baby_other_gastro","malformed_genitals","renal_agenesis","baby_other_urogenital","cleft_lip","polydactyly","club_foot","baby_hernia","baby_other_muscl","downs_syndr","baby_other_chromo","baby_other_cong",
      'tobacco use during pregnancy', 
      'alcohol use during pregnancy', 
      'diabetes (general) for mother', 'chronic hypertension for mother','gestational hypertension for mother', 'eclampsia for mother'
    ),
    start_pos = c(
      176, 172,
      42, 44,
      21, 23,
      8, 72,
      37, 47, 6,
      80, 77,
      87, 
      83,
      156,
      160, 158,
      102, 105,
      205,189,
      183, 193,
      224,
      201,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,
      242, 246, 
      228, 232, 233, 234
    ),
    end_pos = c(
      179, 173,
      43, 46,
      22, 25,
      8, 73,
      39, 51, 6,
      81, 77,
      87,
      84,
      157,
      161, 158,
      102, 105,
      206, 189,
      184, 196,
      224,
      201,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,
      242, 246, 
      228, 232, 233, 234
    ),
    var_length = c(
      4, 2, 
      2, 3, 
      2, 3,
      1, 2,
      3,5, 1,
      2, 1,
      1,
      2,
      2,
      2, 1,
      2, 2,
      2, 1,
      2, 4,
      1,
      1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1, 
      1,1,1,1

    ),  
    var_codes = list(
      data.table(code = 1992, desc = "year"),
      data.table(code = "01-12", desc = "month"),
      data.table(code = "00", desc = "state fips"),
      data.table(code = "000", desc = "county fips"),
      data.table(code = "00", desc = "state fips"),
      data.table(code = "000", desc = "county fips"),
      data.table(code = c(1:5,9), desc = c(
        "Hospital", "Freestanding Birthing Center", "Clinic / Doctor’s Office",
        "Residence", "Other", "Unknown"
      )),
      data.table(code = c("01","02-36"), desc = c("Under 15","Code +13 years")),
      data.table(
        code = c("001", "999", "ZZZ"), 
        desc = c("Cities","Balance of county", "Foreign")
      ),
      data.table(
        code = c("nnnnn", "99999", "00000"), 
        desc = c("Cities","Balance of county", "Foreign")
      ),
      data.table(
        code = 1:4, 
        desc = c("resident","intrastate nonres","interstate nonres","foreign")
      ),
      data.table(code = c(paste0("0",1:7), 18,28,38,48,58,68,78), desc = c(
        "White",
        "Black",
        "American Indian",
        "Chinese",
        "Japanese",
        "Hawaiian",
        "Filipino",
        "Asian Indian",
        "Korean",
        "Samoan",
        "Vietnamese",
        "Guamanian",
        "Other Asian or Pacific Islander",
        "Combined other Asian or Pacific Islander"
      )),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = c(1,2), desc = c("Married","Unmarried")),
      data.table(code = c("00","01-08","09",10:17,99), desc = c(
        "No formal education"
        ,"Years of elementary school"
        ,"1 year of high school"
        ,"2 years of high school"
        ,"3 years of high school"
        ,"4 years of high school"
        ,"1 year of college"
        ,"2 years of college"
        ,"3 years of college"
        ,"4 years of college"
        ,"5 or more years of college"
        ,"Not stated"
      )),
      data.table(code = c(1:11), desc = c(
        "Under 15 years","15-19 years","20-24 years","25-29 years","30-34 years",
        "35-39 years","40-44 years","45-49 years","50-54 years","55-98 years", "Unknown"
      )),
      data.table(code = c(paste0("0",1:7), 18,28,38,48,58,68,78), desc = c(
        "White",
        "Black",
        "American Indian",
        "Chinese",
        "Japanese",
        "Hawaiian",
        "Filipino",
        "Asian Indian",
        "Korean",
        "Samoan",
        "Vietnamese",
        "Guamanian",
        "Other Asian or Pacific Islander",
        "Combined other Asian or Pacific Islander"
      )),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = c("1-7","8","9"), desc = c("Sum of all live births", "Eight or more","Unknown")),
      data.table(code = c("1-7","8","9"), desc = c("Sum of all pregnancies", "Eight or more","Unknown")),
      data.table(code = c("00-10", 99), desc = c("Score", "Unknown")),
      data.table(code = c("1","2"), desc = c("Male", "Female")),
      data.table(code = c("17-47", 99), desc = c("Weeks of gestation", "Unknown")),
      data.table(code = c("0000-9998", 9999), desc = c("Birth weight in grams", "Unknown")),
      data.table(code = c(1:5), desc = c("Vaginal", "Vaginal after previous c-section","Primary C-section","Repeat C-section","Unknown")),
      data.table(code = "1-5", desc = "number of babies"),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,9), desc = c('reported','not reported','Unknown/not stated')),
      data.table(code = c(1,2,9), desc = c('reported','not reported','Unknown/not stated')), 
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')), 
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')), 
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')), 
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable'))
    )
  )


natality_dct_2003 = 
  data.table(
    year = 2003, 
    var_name = c(
      "dob_yy", "dob_mm",
      "state_postal", "county_fips",
      "state_postal_occ", "county_fips_occ",
      "birth_facility", "mage", 
      "city_res", 'place_fips', "restatus",
      "mrace", "mhisp",
      "mar", 
      "meduc","umeduc",
      "fage",
      "frace","fhisp",
      "live_birth_order", "total_birth_order",
      "apgar5", "sex",
      "gestation", "dbwt",
      "delmeth",
      # New indicator variables 
      "plurality","febrile","meconium","membrane_rupture","abruptio_placentae","placenta_previa","excessive_bleeding","mother_seizure","labor_under_3h","labor_over_20h","labor_dysfunc","breech","cephalopelvic_disproportion","cord_prolapse","anesthetic_comp","fetal_distress","labor_complication","baby_anemia","baby_injury","fetal_alcohol","baby_hyaline","meconoim_aspiration","vent_under_30m","vent_over_30m","baby_seizures","baby_other_abn","anencephaly","spina_bifida","hydrocephalus","microcephalus","baby_other_cent_nerv","heart_malform","baby_other_circ","rectal_atresia","tracheo","omphalocele","baby_other_gastro","malformed_genitals","renal_agenesis","baby_other_urogenital","cleft_lip","polydactyly","club_foot","baby_hernia","baby_other_muscl","downs_syndr","baby_other_chromo","baby_other_cong",
      'tobacco','tobacco_recode',
      'alcohol',
      # Maternal Health 
      'mat_diabetes','mat_chronic_hypertension', 'mat_gest_hypertension', 'mat_eclampsia'
    ),
    var_desc = c(
      "birth year", "birth month",
      "residence state postal code", "residence county fips",
      "occurence state postal code", "occurence county fips",
      "birth place", "mother's age (recode 41)",
      "city of residence (mother)", 'place fips (mother residence)', "residence status",
      "mother's race recode", "mother hispanic origin",
      "mother's marital status", 
      "mother's education revised","mother's education unrevised",
      "father's age (recode 11)",
      "father's race recode", "father hispanic origin",
      "Sum of all previous live births", "Sum of all previous pregnancies",
      "Five minute APGAR score", "sex of infant",
      "Computed gestation", "Birthweight",
      "Delivery Method",
      # New indicator variables 
      "plurality","febrile","meconium","membrane_rupture","abruptio_placentae","placenta_previa","excessive_bleeding","mother_seizure","labor_under_3h","labor_over_20h","labor_dysfunc","breech","cephalopelvic_disproportion","cord_prolapse","anesthetic_comp","fetal_distress","labor_complication","baby_anemia","baby_injury","fetal_alcohol","baby_hyaline","meconoim_aspiration","vent_under_30m","vent_over_30m","baby_seizures","baby_other_abn","anencephaly","spina_bifida","hydrocephalus","microcephalus","baby_other_cent_nerv","heart_malform","baby_other_circ","rectal_atresia","tracheo","omphalocele","baby_other_gastro","malformed_genitals","renal_agenesis","baby_other_urogenital","cleft_lip","polydactyly","club_foot","baby_hernia","baby_other_muscl","downs_syndr","baby_other_chromo","baby_other_cong",
      'tobacco use', 'tobacco use (2003 birth certificate)',
      'alcohol use', 
      # Maternal health
      'diabetes (general) for mother', 'chronic hypertension for mother','gestational hypertension for mother', 'eclampsia for mother'
    ),
    start_pos = c(
      15, 19, 
      109, 114,
      30,37,
      42, 89,
      117, 120, 138,
      143, 148,
      153, 
      155, 156,
      186,
      191, 195,
      212, 217,
      415, 436,
      451, 463,
      401,
      423,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,483,484,485,486,487,488,489,490,491,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,
      290, 294,
      295, 
      331, 335, 336, 337
    ),
    end_pos = c(
      18, 20, 
      110, 116,
      31, 39,
      42, 90,
      119, 124, 138,
      143, 148,
      153,
      155, 157,
      187,
      191, 195,
      212, 217,
      416, 436,
      452, 466,
      401,
      423,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,483,484,485,486,487,488,489,490,491,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,
      290,294,
      295, 
      331, 335, 336, 337
    ),
    var_length = c(
      4, 2,
      2, 3,
      2, 3, 
      1, 2,
      3, 5, 1,
      1, 1,
      1,
      1, 2,
      2,
      1, 1,
      1, 1,
      2, 1,
      2, 4,
      1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,
      1, 
      1,1,1,1
    ),  
    var_codes = list(
      data.table(code = 2003, desc = "year"),
      data.table(code = "01-12", desc = "month"),
      data.table(code = "AK", desc = "postal code"),
      data.table(code = "000", desc = "county fips"),
      data.table(code = "AK", desc = "postal code"),
      data.table(code = "000", desc = "county fips"),
      data.table(code = c(1:5,9), desc = c(
        "Hospital", "Freestanding Birthing Center", "Clinic / Doctor’s Office",
        "Residence", "Other", "Unknown"
      )),
      data.table(code = c("01","02-41"), desc = c("Under 15","Code +13 years")),
      data.table(code = c("nnn", "999",'ZZZ'), desc = c("Cities","Balance of county", "Foreign")),
      data.table(
        code = c("nnnnn", "99999", "00000"), 
        desc = c("Cities","Balance of county", "Foreign")
      ),
      data.table(code = 1:4, desc = c("resident","intrastate nonres","interstate nonres","foreign")),
      data.table(code = c(1:4), desc = c("White","Black","American Indian","Asian or Pacific Islander")),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = c(1,2,9), desc = c("Married","Unmarried","Unknown")),
      data.table(code = c(1:9), desc = c(
        "8th grade or less"
        ,"9th through 12th grade with no diploma"
        ,"High school graduate or GED completed"
        ,"Some college credit, but not a degree"
        ,"Associate degree (AA,AS)"
        ,"Bachelor’s degree (BA, AB, BS)"
        ,"Master’s degree (MA, MS, MEng, MEd, MSW, MBA)"
        ,"Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, LLB, JD)"
        ,"Unknown"
      )),
      data.table(code = c("00","01-08","09",10:17,99), desc = c(
        "No formal education"
        ,"8	Years of elementary school"
        ,"1 year of high school"
        ,"2 years of high school"
        ,"3 years of high school"
        ,"4 years of high school"
        ,"1 year of college"
        ,"2 years of college"
        ,"3 years of college"
        ,"4 years of college"
        ,"5 or more years of college"
        ,"Not stated"
      )),
      data.table(code = c(1:11), desc = c(
        "Under 15 years","15-19 years","20-24 years","25-29 years","30-34 years",
        "35-39 years","40-44 years","45-49 years","50-54 years","55-98 years", "Unknown"
      )),
      data.table(code = c(1:4,9), desc = c(
        "White","Black","American Indian","Asian or Pacific Islander", "Unknown"
      )),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = c("1-7","8","9"), desc = c("Sum of all live births", "Eight or more","Unknown")),
      data.table(code = c("1-7","8","9"), desc = c("Sum of all pregnancies", "Eight or more","Unknown")),
      data.table(code = c("00-10", 99), desc = c("Score", "Unknown")),
      data.table(code = c("M","F"), desc = c("Male", "Female")),
      data.table(code = c("17-47", 99), desc = c("Weeks of gestation", "Unknown")),
      data.table(code = c("0000-9998", 9999), desc = c("Birth weight in grams", "Unknown")),
      data.table(code = c(1:7), desc = c(
        "Vaginal", "Vaginal after previous c-section","Primary C-section","Repeat C-section","Unknown",
        "Vaginal (2003 only)", "C-section (2003 only)"
      )),
      data.table(code = "1-5", desc = "number of babies"),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,9), desc = c('reported','not reported', 'unknown/not stated')),
      data.table(code = c('Y','N','U'), desc = c('reported','not reported', 'unknown/not stated')),
      data.table(code = c(1,2,9), desc = c('reported','not reported', 'unknown/not stated')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable'))
    )
  )

natality_dct = 
  rbind(
    mutate(natality_dct_1992, year = 1990),
    mutate(natality_dct_1992, year = 1991),
    natality_dct_1992,
    mutate(natality_dct_1992, year = 1993),
    mutate(natality_dct_1992, year = 1994),
    mutate(natality_dct_1992, year = 1995),
    mutate(natality_dct_1992, year = 1996),
    mutate(natality_dct_1992, year = 1997),
    mutate(natality_dct_1992, year = 1998),
    mutate(natality_dct_1992, year = 1999),
    mutate(natality_dct_1992, year = 2000),
    mutate(natality_dct_1992, year = 2001),
    mutate(natality_dct_1992, year = 2002),
    natality_dct_2003,
    mutate(natality_dct_2003, year = 2004),
    mutate(natality_dct_2003, year = 2005),
    mutate(natality_dct_2003, year = 2006),
    mutate(natality_dct_2003, year = 2007),
    mutate(natality_dct_2003, year = 2008),
    mutate(natality_dct_2003, year = 2009),
    mutate(natality_dct_2003, year = 2010),
    mutate(natality_dct_2003, year = 2011),
    mutate(natality_dct_2003, year = 2012),
    mutate(natality_dct_2003, year = 2013)
  )

save(natality_dct, file = here("data/health-restricted/natality-dct.RData"))
load(file = here("data/health-restricted/natality-dct.RData"))