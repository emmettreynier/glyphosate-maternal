# Notes ----------------------------------------------------------------------------------
#   Goal:   Run main TWFE analysis with various specifications.
#   Time:   ??? (Probably a long time)


# Todo list ------------------------------------------------------------------------------


# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    readr, readxl, stringr, fastverse, qs, patchwork,
    rlang, fixest, splines, parallel, magrittr, here,
    fst
  )
  fastverse_extend(topics = c('ST', 'DT', 'VI'))


# Load data ------------------------------------------------------------------------------
  # Yield-potential treatment definitions
  comb_cnty_dt = here(
    'data', 'clean', 'comb-cnty-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Pre-period county-level crop yield percentiles
  pctl_dt = here(
    'data', 'clean', 'crop-acre-percentile-90-95.fst'
  ) |> 
  read_fst(as.data.table = TRUE) |>
  get_vars(vars = c('GEOID','percentile'), regex = TRUE)
  # Shift-share data
  share_dt = here(
    'data', 'clean', 'glyph-nat-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Natality data: Raw
  natality_dt = here(
    'data', 'clean', 'natality-micro.fst'
  ) %>% read_fst(as.data.table = TRUE)


# Clean dataset and add variables --------------------------------------------------------
  # Convert ages to numeric (harmonizes; can still use as factors later)
  natality_dt[, `:=`(
    mage = mage %>% as.integer(),
    fage = fage %>% as.integer()
  )]
  # Drop individual birth anomalies
  anomalies = natality_dt |> fselect(febrile:baby_other_cong) |> names()
  natality_dt[, (anomalies) := NULL]
  # Add month of sample
  natality_dt[, year_month := paste0(year, '-', month)]
  # Add indicators for child's race; mother's race, ethnicity, marital status, education
  natality_dt[, `:=`(
    i_female = 1 * (sex == 'F'),
    i_m_black = 1 * (mrace == 2),
    i_m_nonwhite = 1 * (mrace != 1),
    i_m_hispanic = 1 * (mhisp > 0),
    i_m_married = 1 * (mar == 1),
    i_m_hs = 1 * (meduc >= 3),
    i_m_college = 1 * (meduc >= 5)
  )]


# Merge crop datasets --------------------------------------------------------------------
  # Add 90-95 percentiles to the county crop panel; then shift-share variables
  comb_cnty_dt %<>%
    merge(
      y = pctl_dt,
      by = 'GEOID',
      all = FALSE
    ) %>%
    merge(
      share_dt,
      by = c('GEOID', 'year'),
      all = TRUE
    )


# Define 'rural' counties ----------------------------------------------------------------
# NOTE: Within county, rural is constant across years (defined in a Census year)
# NOTE: Because births have two counties (occurence and residence), we define
#       two measures of 'rural births'
  # Change 'GEOID' to 'fips'
  setnames(comb_cnty_dt, old = 'GEOID', new = 'fips')
  # Key the county dataset
  setkey(comb_cnty_dt, fips, year)
  # Define 'rural' using residence
  setkey(natality_dt, fips_res, year, month)
  natality_dt %<>% merge(
    y = comb_cnty_dt[year == 1995, .(fips, rural_res = rural)],
    by.x = 'fips_res',
    by.y = 'fips',
    all.x = TRUE,
    all.y = FALSE
  )
  # Define 'rural' using birth occurence
  setkey(natality_dt, fips_occ, year, month)
  natality_dt %<>% merge(
    y = comb_cnty_dt[year == 1995, .(fips, rural_occ = rural)],
    by.x = 'fips_occ',
    by.y = 'fips',
    all.x = TRUE,
    all.y = FALSE
  )
  # Define 'groups' based upon the two rural statuses
  lvls = c('uu', 'rr', 'ru', 'ur')
  lbls = c(
    'urban res; urban occ', 'rural res; rural occ',
    'rural res; urban occ', 'urban res; rural occ'
  )
  natality_dt[, `:=`(
    rural_grp = fcase(
      rural_res == FALSE & rural_occ == FALSE, factor('uu', levels = lvls, labels = lbls),
      rural_res == TRUE & rural_occ == TRUE, factor('rr', levels = lvls, labels = lbls),
      rural_res == TRUE & rural_occ == FALSE, factor('ru', levels = lvls, labels = lbls),
      rural_res == FALSE & rural_occ == TRUE, factor('ur', levels = lvls, labels = lbls),
      default = NA
    )
  )]
  # Clean up
  rm(lbls, lvls)
  invisible(gc())


# If testing without real data -----------------------------------------------------------
  # natality_dt = read.fst(
  #   here('data/clean/mini-data.fst'),
  #   as.data.table = TRUE
  # )


# Additional county-year variables -------------------------------------------------------
  # Add income quintiles 
  inc_dt = comb_cnty_dt[
    year == 1995, .(
    fips, 
    tot_pop = ifelse(is.na(tot_pop), 0, tot_pop), 
    income_per_cap = inc_per_cap_farm + inc_per_cap_nonfarm,
    inc_per_cap_farm , 
    inc_per_cap_nonfarm
  )]
  setorder(inc_dt, income_per_cap)
  # Calculate cumulative population share
  inc_dt[, cum_pop := cumsum(tot_pop) / fsum(tot_pop)]
  # Assign quintiles based on the cumulative population share
  inc_dt[, 
    income_quintile := cut(
      cum_pop, 
      breaks = seq(0, 1, 0.2), 
      include.lowest = TRUE, labels = FALSE
    )
  ]
  # Adding back to main table 
  comb_cnty_dt = 
    merge(
      comb_cnty_dt[,-"income_quintile"], 
      inc_dt[, .(fips, income_quintile)],
      by = 'fips', 
      all.x = TRUE
    )
  # Calculate additional variables
  comb_cnty_dt[, `:=`(
    empl_rate = tot_empl / tot_pop,
    farm_empl_per_cap = farm_empl / tot_pop, 
    tot_acres_km2  = tot_acres/area_km2,
    corn_acres_km2  = corn_acres/area_km2,
    soy_acres_km2  = soy_acres/area_km2,
    cotton_acres_km2  = cotton_acres/area_km2,
    other_acres_km2  = other_acres/area_km2
  )]
  # Change name of unemployment rate
  setnames(comb_cnty_dt, old = 'unemployment_rate', new = 'unempl_rate')
  # Load ag district-code crosswalk (variable: `asd_code`)
  asd_xwalk =
    data.table(read_fwf(
      here('data/download-manual/county_list.txt'),
      col_positions = fwf_positions(
        start = c(1, 6, 11, 17),
        end = c(2, 7, 13, 100)
      ),
      show_col_type = FALSE,
      skip = 12
    ))[!is.na(X1), .(
      state_fips = X1,
      asd_code = X2,
      county_fips = X3,
      GEOID = paste0(X1, X3),
      name = str_split_i(X4, '\\\t{3,6}', i = 1),
      historical = str_split_i(X4, '\\\t{3,6}', i = 2)
    )]
  asd_xwalk[,
    GEOID := fcase(
      GEOID == '46102', '46113',
      GEOID != '46102', GEOID
    )
  ]
  # Drop historical codes
  asd_xwalk %<>% .[historical != 2]
  # Drop the full states (only want counties)
  asd_xwalk %<>% .[county_fips != '000']
  # Drop "combined" counties
  asd_xwalk %<>% .[!(county_fips %in% c(888, 999))]
  # Combine ASD code with state to make unique
  asd_xwalk[, asd_code := paste0(state_fips, '-', asd_code)]
  # Drop unwanted columns
  asd_xwalk %<>% .[, .(GEOID, asd_code)]
  # Merge onto the estimation dataset
  comb_cnty_dt %<>% merge(
    y = asd_xwalk,
    by.x = 'fips',
    by.y = 'GEOID',
    all.x = TRUE,
    all.y = FALSE
  )
  # Fix DC's missing ASD code (assign one code for DC)
  comb_cnty_dt[fips == '11001' & is.na(asd_code), asd_code := '11-01']
  # Clean up
  rm(asd_xwalk)
  # Load ARMS farm resource region cross walk (variable: `farm_region`)
  frr_xwalk =
    here('data', 'download-manual', 'farm-resource-regions.xls') |>
    read_xls(skip = 2)
  # To data table
  setDT(frr_xwalk)
  # Grab region names
  frr_names = frr_xwalk[1:9, 7]
  setnames(frr_names, 'var')
  frr_names %<>% .[, tstrsplit(var, '=')]
  setnames(frr_names, c('num', 'label'))
  # Grab desired columns
  frr_xwalk %<>% .[, 1:2]
  # Rename columns
  setnames(frr_xwalk, c('fips', 'farm_region'))
  # Pad FIPS codes
  frr_xwalk[, fips := str_pad(fips, 5, 'left', '0')]
  # Recode regions
  frr_xwalk[, `:=`(
    farm_region = factor(
      farm_region,
      levels = frr_names$num,
      labels = frr_names$label
    )
  )]
  comb_cnty_dt %<>% merge(
    y = frr_xwalk,
    by = 'fips',
    all.x = TRUE,
    all.y = FALSE
  )
  # Copy Boulder County's region to Broomfield County
  set(
    x = comb_cnty_dt,
    i = comb_cnty_dt[fips == '08014', which = TRUE],
    j = 'farm_region',
    value = comb_cnty_dt[fips == "08013", ffirst(farm_region)]
  )
  # Clean up
  rm(frr_xwalk, frr_names)
  invisible(gc())


# Function: Run TFWE analysis ------------------------------------------------------------
  est_twfe = function(
    outcomes = c(
      'mat_diabetes',
      'mat_chronic_hypertension',
      'mat_gest_hypertension',
      'mat_eclampsia'
    ),
    iv = 'all_yield_diff_percentile_gmo_max',
    iv_shift = NULL,
    spatial_subset = 'rural',
    random_subset = NULL,
    county_subset = NULL,
    county_subset_name = NULL,
    het_split = NULL,
    base_fe = c('year_month', 'fips_res', 'fips_occ'),
    dem_fe = TRUE,
    dad_fe = TRUE,
    control_sets = list2(
      'none',
      'pest',
      'unempl_rate',
      'empl_rate',
      c('unempl_rate', 'empl_rate'),
      c('unempl_rate', 'empl_rate', 'pct_farm_empl'),
      c('unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap'),
      c('age_share'),
      c('race_share'),
      c('pest', 'unempl_rate', 'empl_rate', 'age_share', 'race_share'),
      'fert',
      NULL
    ),
    name_suffix = NULL,
    clustering = c('year', 'state_fips'),
    include_ols = FALSE,
    include_did = FALSE,
    skip_iv = FALSE,
    ...
  ) {

    # Define outcome variables
    outcome_vars = outcomes

    # Drop NULL elements from the control sets
    control_sets = Filter(Negate(is.null), control_sets)

    # Define sets of control variables (helps when loading data)
    # Child and mother demographic controls (fixed effects)
    dem_fes = c(
      'sex', 'mage', 'mrace', 'mhisp', 'meduc', 'mar',
      'birth_facility', 'restatus', 'total_birth_order'
    )
    # Father controls (fixed effects)
    dad_fes = c('fage', 'fhisp', 'frace')
    # Define pesticide controls
    pest_controls = c(
      'alachlor_km2', 'atrazine_km2', 'cyanazine_km2', 'fluazifop_km2',
      'metolachlor_km2', 'metribuzin_km2', 'nicosulfuron_km2'
    )
    pest_fml = paste(pest_controls, collapse = ' + ')
    # Economic controls
    econ_controls = c(
      'unempl_rate',
      'pct_farm_empl',
      'farm_empl_per_cap',
      'tot_pop',
      'inc_per_cap_farm',
      'inc_per_cap_nonfarm',
      'empl_rate',
      'farm_empl_per_cap',
      NULL
    )
    # Fertilizer controls
    fert_controls = c(
      'p_commercial_km2', 'n_commercial_km2',
      'p_farm_km2', 'n_farm_km2',
      'p_nonfarm_km2', 'n_nonfarm_km2',
      'p_manure_km2', 'n_manure_km2',
      NULL
    )
    fert_fml = paste(fert_controls, collapse = ' + ')
    # Acre controls
    acre_controls = c(
      'corn_acres_km2',
      'soy_acres_km2',
      'cotton_acres_km2',
      'other_acres_km2',
      NULL
    )
    acre_fml = paste(acre_controls, collapse = ' + ')
    # Age-share controls
# NOTE Omitting the 70+ share (colinear)
    age_controls = paste0('shr_age_', seq(0, 60, 10), '_all')
    age_fml = paste(age_controls, collapse = ' + ')
    # Race-share controls
    race_controls = c(
      'shr_raceblack_all',
      'shr_racewhite_all',
      'shr_hispanic_all',
      NULL
    )
    race_fml = paste(race_controls, collapse = ' + ')

    # Collecting glyphosate variables
    glyph_vars = 'glyph_km2'
    # iv_vars = comb_cnty_dt %>% names() %>% str_subset(iv)
    iv_vars =
      c(iv, iv_shift) |>
      na.omit() |>
      as.vector()
    # Heterogeneity variables
    het_vars = c(
      switch(
        !is.null(het_split),
        switch(het_split %in% names(natality_dt), het_split, NULL),
        NULL
      )
    )
    # Define variables that might be used for fixed effects (in comb_cnty_dt)
    fe_vars = c(
      'state_fips', 'census_region', 'census_division',
      'asd_code', 'farm_region'
    )

    # Enforce spatial subsets (essentially rural, urban, or all)
    if (is.null(spatial_subset)) {
      est_dt = natality_dt
    } else {
      est_dt = natality_dt[str_detect(rural_grp, spatial_subset)]
    }

    # Merge the datasets with the requested variables
    est_dt = merge(
      x = est_dt[, unique(c(
        'fips_occ', 'fips_res', 'year', 'month', 'year_month',
        'rural_grp', 'rural_occ', 'rural_res',
        outcome_vars, het_vars, dem_fes, dad_fes
      )), with = FALSE],
      y = comb_cnty_dt[, unique(c(
        'fips', 'year',
        switch(
          !is.null(het_split),
          switch(het_split %in% names(comb_cnty_dt), het_split, NULL),
          NULL
        ),
        glyph_vars,
        iv_vars,
        fe_vars,
        pest_controls,
        econ_controls,
        fert_controls,
        acre_controls,
        clustering
      )), with = FALSE],
      by.x = c('fips_res', 'year'),
      by.y = c('fips', 'year'),
      all = FALSE
    )

    # Load and add additional datasets (if controls require them)
    if (any(c('age_share', 'race_share', 'pop_all') %in% unlist(control_sets))) {
      # Load the age-share dataset
# NOTE The 1969-2022 version has a longer time series but lacks "origin" data
      seer_dt =
        here('data/raw/seer', 'seer-shares-allpop-1990-2022.fst') |>
        read.fst(as.data.table = TRUE)
      # Merge
      est_dt %<>% merge(
        y = seer_dt,
        by.x = c('fips_res', 'year'),
        by.y = c('fips', 'yr'),
        all.x = TRUE,
        all.y = FALSE
      )
    }

    # Enforce regional subsets (e.g., Census regions)
    if (!is.null(county_subset)) {
      # Take the implied subset
      est_dt %<>% .[fips_res %in% county_subset]
    }

    # Enforce random subset (if requested)
    if (!is.null(random_subset)) {
      # Set seed
      set.seed(random_subset[2])
      # Sample desired rows (rounding the percent up to nearest integer)
      est_dt %<>% .[sample(.N, ceiling(random_subset[1] * .N))]
    }

    # Build the requested pieces of the formula...
    # Glyph variable
    fml_gly = 'glyph_km2'

    # Outcome(s)
    fml_y = paste0(
      'c(',
      paste(outcome_vars, collapse = ', '),
      ')'
    )

    # Instruments: Without shift-share (event study)
    fml_iv = paste0('1 + i(year, ', iv, ', ref = 1995)')

    # Instruments: Shift-share approach (if iv_shift is defined)
    if (!is.null(iv_shift)) {
      fml_iv_ss = paste0(iv_shift, ' + ', iv_shift, ':', iv)
    }

    # Fixed effects
    fml_fes =
      c(
        if (dem_fe == TRUE) dem_fes else NULL,
        if (dad_fe == TRUE) dad_fes else NULL,
        base_fe,
        NULL
      ) |>
      paste(collapse = ' + ')
    if (fml_fes == '') fml_fes = '1'

    # Controls
    # Start by recording the length and class (for subcases)
    cs_len = control_sets |> length()
    cs_class = control_sets |> class()
    # Check cases (based on length and class)
    if (
      # Case 1: No controls (length 0 or 'none')
      (cs_len == 0) || all(control_sets == 'none')
    ) {
      # No controls: Set to NULL
      fml_controls = NULL
    } else if (
      # Case 2: One set of controls
      ((cs_len == 1) && (cs_class == 'list')) ||
      ((cs_class == 'character') && (control_sets != 'none'))
    ) {
      # One set of controls: Collapse
      fml_controls =
        control_sets |>
        unlist() |>
        paste(collapse = ' + ') |>
        str_replace(pattern = 'fert', replacement = fert_fml) |>
        str_replace(pattern = 'acres', replacement = acre_fml) |>
        str_replace(pattern = 'age_share', replacement = age_fml) |>
        str_replace(pattern = 'pest', replacement = pest_fml) |>
        str_replace(pattern = 'race_share', replacement = race_fml)
    } else if (
      # Case 3: Multiple sets of controls (iterate over list elements)
      (cs_len > 1) && (cs_class == 'list')
    ) {
      # Multiple sets of controls: Iterate over list elements
      fml_controls =
        lapply(
          X = seq_along(control_sets),
          FUN = function(i) {
            # Grab the set of controls
            cs_i = control_sets[[i]]
            # If NULL or 'none', then return '1'; else collapse
            if (is.null(cs_i) || all(cs_i == 'none')) {
              '1'
            } else {
              cs_i |>
                unlist() |>
                paste(collapse = ' + ') |>
                str_replace(pattern = 'fert', replacement = fert_fml) |>
                str_replace(pattern = 'acres', replacement = acre_fml) |>
                str_replace(pattern = 'age_share', replacement = age_fml) |>
                str_replace(pattern = 'pest', replacement = pest_fml) |>
                str_replace(pattern = 'race_share', replacement = race_fml)
            }
          }
        ) |>
        paste(collapse = ', ')
      # Wrap with fixest `sw` function
      fml_controls = paste0('sw(', fml_controls, ')')
    }

    # Clusters
    fml_inf =
      ifelse(
        length(clustering) == 1,
        clustering %>% paste0('~ ', .),
        clustering %>% paste(collapse = ' + ') %>% paste0('~ ', .)
      ) %>%
      as.formula()

    # Formula: Reduced form
    fml_rf =
      paste(
        fml_y,
        '~',
        fml_iv,
        ifelse(
          !is.null(fml_controls),
          paste0(' + ', fml_controls),
          ''
        ),
        ' | ',
        fml_fes
      ) |>
      as.formula()

    # Formula: 2SLS via event study
    fml_2sls =
      paste(
        fml_y,
        '~ 1',
        ifelse(
          !is.null(fml_controls),
          paste0(' + ', fml_controls),
          ''
        ),
        ' | ',
        fml_fes,
        ' | ',
        fml_gly,
        ' ~ ',
        fml_iv
      ) |>
      as.formula()

    # Formula: 2SLS via shift-share (if iv_shift is defind)
    if (!is.null(iv_shift)) {
      fml_2sls_ss =
        paste(
          fml_y,
          '~ 1',
          ifelse(
            !is.null(fml_controls),
            paste0(' + ', fml_controls),
            ''
          ),
          ' | ',
          fml_fes,
          ' | ',
          'glyph_km2 ~ ',
          fml_iv_ss
        ) |>
        as.formula()
    }

    # Formula: OLS
    if (include_ols == TRUE) {
      fml_ols =
        paste(
          fml_y,
          ' ~ ',
          fml_gly,
          ifelse(
            !is.null(fml_controls),
            paste0(' + ', fml_controls),
            ''
          ),
          ' | ',
          fml_fes
        ) |>
        as.formula()
    }

    # Formula: Difference-in-differences
    if (include_did == TRUE) {
      # Add variable for post-treatment (1996+) interacted with treatment
      set(
        x = est_dt,
        j = 'trt_post',
        value = est_dt[[iv]] * (est_dt[['year']] >= 1996)
      )
      # Add gly_km2 to the outcome variables
      fml_y_did = paste0(
        'c(',
        paste(c(outcome_vars, 'glyph_km2'), collapse = ', '),
        ')'
      )
      fml_did =
        paste(
          fml_y_did,
          ' ~ ',
          iv, ' + trt_post',
          ifelse(
            !is.null(fml_controls),
            paste0(' + ', fml_controls),
            ''
          ),
          ' | ',
          fml_fes
        ) |>
        as.formula()
    }

    # Make folder for the results
    dir_today = here('data', 'results', 'micro-new')
    dir_today |> dir.create(showWarnings = FALSE, recursive = TRUE)
    # Calculate time suffix
    time_suffix = now() |> as.integer() |> as.character()

    # Base filename with all options
    base_name = paste0(
      # Outcomes
      '_outcome-',
      paste(str_remove_all(outcomes, '[^a-z]'), collapse = '-'),
      # Fixed effects (including demographic FEs)
      '_fe-',
      ifelse(
        is.null(base_fe) && dem_fe == FALSE && dad_fe == FALSE,
        'none',
        paste0(
          c(base_fe, ifelse(dem_fe, 'dem', ''), ifelse(dad_fe, 'dad', '')),
          collapse = '-'
        ) |>
          str_replace_all('\\^', 'X') |>
          str_remove_all('_')
      ),
      # Demographic (mother and child demographics) FEs
      ifelse(dem_fe, '_dem-fe', ''),
      # Father demographic FEs
      ifelse(dad_fe, '_dad-fe', ''),
      # Spatial subset
      ifelse(is.null(spatial_subset), '', paste0('_spatial-', spatial_subset)),
      # Random subset
      ifelse(
        is.null(random_subset),
        '',
        paste0('_random-', round(100 * random_subset[1], 0), '-', random_subset[2])
      ),
      # County subset
      ifelse(
        is.null(county_subset),
        '',
        ifelse(
          is.null(county_subset_name),
          '_county-subset-',
          paste0('_county-', county_subset_name)
        )
      ),
      # Heterogeneity split
      ifelse(
        is.null(het_split),
        '',
        paste0('_het-', het_split %>% str_remove_all('[^0-9a-z]'))
      ),
      # Instrument
      '_iv-',
      iv %>% str_remove_all('[^0-9a-z]'),
      # Clustering
      '_cl-',
      clustering %>% str_remove_all('[^a-z]') %>% paste0(collapse = '-'),
      # Add name suffix (if defined)
      ifelse(
        is.null(name_suffix),
        '',
        paste0('_', name_suffix)
      ),
      # Add time suffix
      paste0('_', time_suffix),
      # File suffix
      '.qs'
    )

    # Save information about the model
    qsave(
      list2(
        outcomes = outcomes,
        iv = iv,
        iv_shift = iv_shift,
        spatial_subset = spatial_subset,
        random_subset = random_subset,
        county_subset = county_subset,
        county_subset_name = county_subset_name,
        het_split = het_split,
        base_fe = base_fe,
        dem_fe = dem_fe,
        dad_fe = dad_fe,
        control_sets = control_sets,
        name_suffix = name_suffix,
        clustering = clustering,
        include_ols = include_ols,
        include_did = include_did,
        skip_iv = skip_iv
      ),
      file.path(dir_today, paste0('info_', time_suffix, '.qs')),
      preset = 'fast'
    )

    # If requested: Estimate difference-in-differences
    if (include_did == TRUE) {
      # Estimate
      est_did = feols(
        fml = fml_did,
        cluster = fml_inf,
        data = est_dt[year %in% c(1990:1995, 2000:2005)],
        lean = TRUE
      )
      # Save
      qsave(
        est_did,
        file.path(dir_today, paste0('est_did', base_name)),
        preset = 'fast'
      )
      rm(est_did)
      invisible(gc())
    }

    # Estimate reduced form and 2SLS
    if (skip_iv == FALSE) {
      # Estimate with or without heterogeneity splits
      if (!is.null(het_split)) {
        est_rf = feols(
          fml = fml_rf,
          cluster = fml_inf,
          data = est_dt,
          fsplit = het_split,
          lean = TRUE
        )
      } else {
        est_rf = feols(
          fml = fml_rf,
          cluster = fml_inf,
          data = est_dt,
          lean = TRUE
        )
      }
      # Save
      qsave(
        est_rf,
        file.path(dir_today, paste0('est_rf', base_name)),
        preset = 'fast'
      )
      rm(est_rf)
      invisible(gc())

      # Estimate: 2SLS with event study
      if (!is.null(het_split)) {
        est_2sls = feols(
          fml = fml_2sls,
          cluster = fml_inf,
          data = est_dt[!(year %in% 1990:1991)],
          fsplit = het_split,
          lean = TRUE
        )
      } else {
        est_2sls = feols(
          fml = fml_2sls,
          cluster = fml_inf,
          data = est_dt[!(year %in% 1990:1991)],
          lean = TRUE
        )
      }
      # Save
      qsave(
        est_2sls,
        file.path(dir_today, paste0('est_2sls', base_name)),
        preset = 'fast'
      )
      rm(est_2sls)
      invisible(gc())

      # Estimate: 2SLS with shift share (if iv_shift is defined)
      if (!is.null(iv_shift)) {
        if (!is.null(het_split)) {
          est_2sls_ss = feols(
            fml = fml_2sls_ss,
            cluster = fml_inf,
            data = est_dt[!(year %in% 1990:1991)],
            fsplit = het_split,
            lean = TRUE
          )
        } else {
          est_2sls_ss = feols(
            fml = fml_2sls_ss,
            cluster = fml_inf,
            data = est_dt[!(year %in% 1990:1991)],
            lean = TRUE
          )
        }
        # Save
        qsave(
          est_2sls_ss,
          file.path(dir_today, paste0('est_2sls_ss', base_name)),
          preset = 'fast'
        )
        rm(est_2sls)
        invisible(gc())
      }
    }

    # Estimate OLS
    if (include_ols == TRUE) {
      if (!is.null(het_split)) {
        est_ols = feols(
          fml = fml_ols,
          cluster = fml_inf,
          data = est_dt[!(year %in% 1990:1991)],
          fsplit = het_split,
          lean = TRUE
        )
      } else {
        est_ols = feols(
          fml = fml_ols,
          cluster = fml_inf,
          data = est_dt[!(year %in% 1990:1991)],
          lean = TRUE
        )
      }
      # Save
      qsave(
        est_ols,
        file.path(dir_today, paste0('est_ols', base_name)),
        preset = 'fast'
      )
      rm(est_ols)
      invisible(gc())
    }

    # Return something
    return('done')
  }


# Estimates: Main, pooled results --------------------------------------------------------
  # Instrument: Yield diff percentile GMO max
  est_twfe(
    iv = 'all_yield_diff_percentile_gmo_max',
    iv_shift = 'glyphosate_nat_100km',
    spatial_subset = 'rural',
    county_subset = NULL,
    county_subset_name = NULL,
    het_split = NULL,
    base_fe = c('year_month', 'fips_res', 'fips_occ'),
    dem_fe = TRUE,
    dad_fe = TRUE,
    control_sets = list2(
      'none',
      c(
        'pest',
        'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
        'inc_per_cap_farm', 'inc_per_cap_nonfarm',
        'pop_all',
        'age_share', 'race_share',
        'fert'
       )
    ),
    name_suffix = NULL,
    clustering = c('year', 'state_fips'),
    include_ols = TRUE,
    include_did = TRUE,
    skip_iv = FALSE
  )

  # Instrument: Yield diff percentile GMO max, east of 100th meridian
  est_twfe(
    iv = 'e100m_yield_diff_percentile_gmo_max',
    iv_shift = NULL,
    spatial_subset = 'rural',
    county_subset = NULL,
    county_subset_name = NULL,
    het_split = NULL,
    base_fe = c('year_month', 'fips_res', 'fips_occ'),
    dem_fe = TRUE,
    dad_fe = TRUE,
    control_sets = list2(
      'none',
      c(
        'pest',
        'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
        'inc_per_cap_farm', 'inc_per_cap_nonfarm',
        'pop_all',
        'age_share', 'race_share',
        'fert'
       )
    ),
    name_suffix = NULL,
    clustering = c('year', 'state_fips'),
    include_ols = TRUE,
    include_did = TRUE,
    skip_iv = FALSE
  )
