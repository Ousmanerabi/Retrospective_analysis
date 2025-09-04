# ============================================================
# Burkina Faso malaria – data prep, reporting, outliers (clean)
# Author: Ousmane Diallo
# Requires: R >= 4.1, tidyverse
# ============================================================

# -------------------------
# 0) Libraries (deduplicated)
# -------------------------
suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, readr, ggplot2, etc.
  library(readxl)
  library(sf)
  library(lubridate)
  library(janitor)
  library(stringdist)
  library(zoo)
})

# -------------------------
# 1) Parameters / paths
# -------------------------
# Use parameters instead of setwd() and absolute Windows paths
# Adjust these to your environment.
path_shp   <- "~/Shapefiles/Districts/District sanitaires.shp"
path_xls   <- "C:/.../retrospective analysis 2023/data/Raw"       
path_2016_2022_xlsx <- "C:/.../BFA_monthly_routine_data_2016_2022.xlsx" 
path_hf_cases_active_csv <- "C:/.../HF_cases_active.csv"              

# -------------------------
# 2) Read spatial data (sf)
# -------------------------
# Using sf instead of sp::readOGR (deprecated)
HD_sf <- sf::st_read(path_shp, options = "ENCODING=UTF-8", quiet = TRUE)

# -------------------------
# 3) Read raw Excel files (if needed)
# -------------------------
xls_files <- base::list.files(path_xls, pattern = "\\.xls$", full.names = TRUE)
myfiles   <- base::lapply(xls_files, read_xls)

# -------------------------
# 4) Ingest HMIS table and rename core fields
# -------------------------
# NOTE: The object `Données_Palu_2024` comes from your environment.
# Replace with an explicit read if needed (e.g., read_xlsx).
# TODO: Replace this with the appropriate object or read call.
# Donnees_Palu_2024 <- read_xlsx("...")  # example

raw_data2 <- Données_Palu_2024 %>%     # TODO: ensure this object exists
  # standardize admin names
  dplyr::mutate(
    adm1  = orgunitlevel2,
    adm2  = orgunitlevel4
  ) %>%
  # pick only needed columns and rename in English
  dplyr::select(
    adm1, adm2,
    hf     = organisationunitname,
    period = periodname,
    susp_u5   = `Cas suspects de Paludisme enfant <5 ans`,
    susp_514  = `Cas suspects de Paludisme 5_14 ans`,
    susp_ov15 = `Cas suspects de Paludisme 15 ans et plus`,
    testrdt_u5   = `TDR realises enfant <5 ans`,
    testrdt_514  = `TDR realises enfant 5-14 ans`,
    testrdt_ov15 = `TDR realises 15 ans et +`,
    testmic_u5   = `GE realises enfant <5 ans`,
    testmic_514  = `GE realises 5-14 ans`,
    testmic_ov15 = `GE realises 15 ans et +`,
    confrdt_u5   = `TDR positif enfant <5 ans`,
    confrdt_514  = `TDR positif 5-14 ans`,
    confrdt_ov15 = `TDR positif 15 ans et +`,
    confmic_u5   = `GE positif enfant <5 ans`,
    confmic_514  = `GE positif 5-14 ans`,
    confmic_ov15 = `GE positif 15 ans et +`,
    maltreat_u5   = `Cas de paludisme simple confirmes traites aux ACT enfant <5 ans`,
    maltreat_514  = `Cas de paludisme simple confirmes traites aux ACT 5-14 ans`,
    maltreat_ov15 = `Cas de paludisme simple confirmes traites aux ACT 15 ans et +`,
    pres_u5   = `Cas presume de paludisme simple enfant <5 ans`,
    pres_514  = `Cas presume de paludisme simple  5_14 ans`,
    pres_ov15 = `Cas presume de paludisme simple  15 ans et plus`,
    conf_u5   = `Cas confirmes de paludisme simple enfant <5 ans`,
    conf_514  = `Cas confirmes de paludisme simple 5-14ans`,
    conf_ov15 = `Cas confirmes de paludisme simple 15 ans et +`,
    pressev_u5   = `Cas presume de Paludisme grave enfant <5 ans`,
    pressev_514  = `Cas presume de Paludisme grave  5_14 ans`,
    pressev_ov15 = `Cas presume de Paludisme grave  15 ans et plus`,
    confsev_u5   = `Cas confirmes de paludisme grave enfant <5 ans`,
    confsev_514  = `Cas confirmes de paludisme grave 5-14ans`,
    confsev_ov15 = `Cas confirmes de paludisme grave 15 ans et +`,
    maldth_u5  = `Deces dus au paludisme grave enfants de <5 ans`,
    maldth_514 = `Deces dus au paludisme grave 5-14 ans`,
    maldth_ov15= `Deces dus au paludisme grave 15 ans et +`
  ) %>%
  # build roll-ups with rowwise sums
  dplyr::rowwise() %>%
  dplyr::mutate(
    susp_ov5    = sum(c_across(c(susp_514, testrdt_ov15 = susp_ov15)), na.rm = TRUE),
    susp        = sum(c_across(c(susp_u5, susp_ov5)), na.rm = TRUE),
    
    testrdt_ov5 = sum(c_across(c(testrdt_514, testrdt_ov15)), na.rm = TRUE),
    testrdt     = sum(c_across(c(testrdt_u5, testrdt_ov5)), na.rm = TRUE),
    
    testmic_ov5 = sum(c_across(c(testmic_514, testmic_ov15)), na.rm = TRUE),
    testmic     = sum(c_across(c(testmic_u5, testmic_ov5)), na.rm = TRUE),
    
    confrdt_ov5 = sum(c_across(c(confrdt_514, confrdt_ov15)), na.rm = TRUE),
    confrdt     = sum(c_across(c(confrdt_u5, confrdt_ov5)), na.rm = TRUE),
    
    confmic_ov5 = sum(c_across(c(confmic_514, confmic_ov15)), na.rm = TRUE),
    confmic     = sum(c_across(c(confmic_u5, confmic_ov5)), na.rm = TRUE),
    
    conf        = sum(c_across(c(confrdt, confmic)), na.rm = TRUE),
    test        = sum(c_across(c(testrdt, testmic)), na.rm = TRUE),
    test_u5     = sum(c_across(c(testrdt_u5, testmic_u5)), na.rm = TRUE),
    test_ov5    = sum(c_across(c(testrdt_ov5, testmic_ov5)), na.rm = TRUE),
    
    maltreat_ov5 = sum(c_across(c(maltreat_514, maltreat_ov15)), na.rm = TRUE),
    maltreat     = sum(c_across(c(maltreat_u5, maltreat_ov5)), na.rm = TRUE),
    
    pres_ov5     = sum(c_across(c(pres_514, pres_ov15)), na.rm = TRUE),
    pres         = sum(c_across(c(pres_u5, pres_ov5)), na.rm = TRUE),
    
    conf_ov5     = sum(c_across(c(conf_514, conf_ov15)), na.rm = TRUE),
    
    pressev_ov5  = sum(c_across(c(pressev_514, pressev_ov15)), na.rm = TRUE),
    pressev      = sum(c_across(c(pressev_u5, pressev_ov5)), na.rm = TRUE),
    
    confsev_ov5  = sum(c_across(c(confsev_514, confsev_ov15)), na.rm = TRUE),
    confsev      = sum(c_across(c(confsev_u5, confsev_ov5)), na.rm = TRUE),
    
    maldth_ov5   = sum(c_across(c(maldth_514, maldth_ov15)), na.rm = TRUE),
    maldth       = sum(c_across(c(maldth_u5, maldth_ov5)), na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

# -------------------------
# 5) Period -> month/year (French to numeric)
# -------------------------
fr_months <- c(
  "Janvier"=1,"Février"=2,"Fevrier"=2,"Mars"=3,"Avril"=4,"Mai"=5,"Juin"=6,
  "Juillet"=7,"Août"=8,"Aout"=8,"Septembre"=9,"Octobre"=10,"Novembre"=11,"Décembre"=12,"Decembre"=12
)

raw_data2 <- raw_data2 %>%
  tidyr::separate(period, into = c("month_fr","year"), sep = " ", fill = "right") %>%
  dplyr::mutate(
    month = as.integer(recode(month_fr, !!!fr_months)),
    year  = as.integer(year)
  ) %>%
  dplyr::select(-month_fr)

# -------------------------
# 6) Normalize accents & clean district names
# -------------------------
raw_data1 <- raw_data2 %>%
  dplyr::mutate(
    across(everything(), ~ stringi::stri_trans_general(., id = "Latin-ASCII"))
  ) %>%
  # Remove "DS " prefix and fix a few known forms
  dplyr::mutate(
    adm2 = adm2 %>%
      str_replace("^DS\\s+", "") %>%
      str_replace("^Gorom-Gorom$", "Gorom") %>%
      str_replace("^Sig-Noghin$", "Sig-noghin") %>%
      str_trim()
  ) %>%
  dplyr::mutate(UID = paste(adm1, adm2, hf, sep = " | "))

# -------------------------
# 7) Build verification table (zero -> NA for conf)
# -------------------------
ver <- raw_data1 %>%
  dplyr::select(adm1, adm2, hf, year, month, conf) %>%
  dplyr::mutate(
    UID  = paste(adm1, adm2, hf, sep = " | "),
    Date = make_date(year = year, month = month, day = 1),
    conf = na_if(conf, 0)  # treat zero as missing for reporting
  )

# -------------------------
# 8) Historical data (2016–2022) & active HF list
# -------------------------
BFA_routine_data_2016_2022 <- read_excel(path_2016_2022_xlsx)
HF_cases_active            <- read.csv(path_hf_cases_active_csv)

liste_hf_2023 <- raw_data1 %>%
  dplyr::distinct(adm1, adm2, hf) %>%
  dplyr::mutate(UID = paste(adm1, adm2, hf, sep = " | "))

liste_hf_2016_2022 <- HF_cases_active %>%
  dplyr::distinct(adm1, adm2, hf) %>%
  dplyr::mutate(UID = paste(adm1, adm2, hf, sep = " | "))

# -------------------------
# 9) Fuzzy matching (2016–2022 UID vs 2023 UID)
# -------------------------
matching <- function(column1, column2) {
  # Build all pairs and compute Jaro-Winkler similarity
  results <- expand.grid(Col1 = column1, Col2 = column2, stringsAsFactors = FALSE) %>%
    mutate(Match_Score = (1 - stringdist(Col1, Col2, method = "jw")) * 100)
  
  exact_matches <- results %>%
    filter(Col1 == Col2) %>%
    mutate(Match_Status = "Match", Match_Score = 100)
  
  inexact_matches <- results %>%
    anti_join(exact_matches, by = c("Col1","Col2")) %>%
    group_by(Col1) %>%
    slice_max(order_by = Match_Score, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Match_Status = "Unmatch")
  
  used_col2 <- c(exact_matches$Col2, inexact_matches$Col2)
  inexact_matches <- inexact_matches %>%
    filter(!Col2 %in% used_col2[duplicated(used_col2)])
  
  unmatched_col2 <- setdiff(column2, c(exact_matches$Col2, inexact_matches$Col2))
  unmatched_df <- tibble(
    Col1 = ifelse(unmatched_col2 %in% column1, unmatched_col2, NA_character_),
    Col2 = unmatched_col2,
    Match_Score = 0,
    Match_Status = "Unmatch"
  )
  
  bind_rows(exact_matches, inexact_matches, unmatched_df) %>%
    distinct(Col1, Col2, .keep_all = TRUE)
}

data_match <- matching(liste_hf_2016_2022$UID, liste_hf_2023$UID)

liste_fusion <- data_match %>%
  dplyr::select(UID = Col1, new_name = Col2) %>%
  dplyr::left_join(liste_hf_2023, by = "UID") %>%
  dplyr::select(adm1, adm2, UID, new_name)

raw_data_finale <- raw_data1 %>%
  dplyr::left_join(liste_fusion, by = c("adm1","adm2","UID")) %>%
  dplyr::select(-UID) %>%
  dplyr::rename(UID = new_name)

# -------------------------
# 10) Combine 2016–2022 & 2023, cast numeric columns
# -------------------------
# Keep a controlled set of columns
cols <- c(
  "adm1","adm2","hf","month","year",
  "susp_u5","susp_514","susp_ov15","testrdt_u5","testrdt_514","testrdt_ov15",
  "testmic_u5","testmic_514","testmic_ov15","confrdt_u5","confrdt_514","confrdt_ov15",
  "confmic_u5","confmic_514","confmic_ov15","maltreat_u5","maltreat_514","maltreat_ov15",
  "pres_u5","pres_514","pres_ov15","conf_u5","conf_514","conf_ov15",
  "pressev_u5","pressev_514","pressev_ov15",
  "confsev_u5","confsev_514","confsev_ov15",
  "maldth_u5","maldth_514","maldth_ov15",
  "susp_ov5","susp","testrdt_ov5","testrdt","testmic_ov5","testmic","confrdt_ov5","confrdt",
  "confmic_ov5","confmic","conf","test","test_u5","test_ov5","maltreat_ov5","maltreat",
  "pres_ov5","pres","conf_ov5","pressev_ov5","pressev","confsev_ov5","confsev",
  "maldth_ov5","maldth","UID"
)

bfa_data_monthly_hf_2016_2022 <- HF_cases_active %>% dplyr::select(any_of(cols))

cols_num <- lubridate::intersect(cols, c(
  "month","year","susp_u5","susp_514","susp_ov15","testrdt_u5","testrdt_514","testrdt_ov15",
  "testmic_u5","testmic_514","testmic_ov15","confrdt_u5","confrdt_514","confrdt_ov15",
  "confmic_u5","confmic_514","confmic_ov15","maltreat_u5","maltreat_514","maltreat_ov15",
  "pres_u5","pres_514","pres_ov15","conf_u5","conf_514","conf_ov15","pressev_u5","pressev_514",
  "pressev_ov15","confsev_u5","confsev_514","confsev_ov15","maldth_u5","maldth_514","maldth_ov15",
  "susp_ov5","susp","testrdt_ov5","testrdt","testmic_ov5","testmic","confrdt_ov5","confrdt",
  "confmic_ov5","confmic","conf","test","test_u5","test_ov5","maltreat_ov5","maltreat",
  "pres_ov5","pres","conf_ov5","pressev_ov5","pressev","confsev_ov5","confsev","maldth_ov5","maldth"
))

raw_data_finale <- raw_data_finale %>%
  dplyr::mutate(across(all_of(cols_num), ~ suppressWarnings(as.numeric(.))))

bfa_data_monthly_hf_2016_2023 <- dplyr::bind_rows(
  bfa_data_monthly_hf_2016_2022 %>% dplyr::mutate(source = "2016_2022"),
  raw_data_finale %>% dplyr::mutate(source = "2023")
)

# -------------------------
# 11) Classify HF activity (single implementation)
# -------------------------
inactive_threshold <- 6

classify_activity_status <- function(df, conf_col = "conf") {
  df %>%
    dplyr::group_by(UID) %>%
    complete(
      Date = seq(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE), by = "month"),
      fill = setNames(list(NA), conf_col)
    ) %>%
    dplyr::filter(year(Date) != 2019) %>%       
    dplyr::arrange(Date, .by_group = TRUE) %>%
    dplyr::mutate(
      temp_status   = if_else(is.na(.data[[conf_col]]) | .data[[conf_col]] == 0, "inactive", "active"),
      active_status = zoo::na.locf(ifelse(temp_status == "active", temp_status, NA),
                                   na.rm = FALSE, fromLast = FALSE, default = "inactive"),
      rle_vals      = rle(temp_status),
      run_len       = rep(rle_vals$lengths, rle_vals$lengths),
      run_val       = rep(rle_vals$values,   rle_vals$lengths),
      HF_status     = case_when(
        all(temp_status == "inactive") ~ "inactive",
        run_val == "inactive" & run_len >= inactive_threshold ~ "inactive",
        TRUE ~ active_status
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-temp_status, -active_status, -rle_vals, -run_len, -run_val)
}

# Build input for classification
HF_activity <- HF_cases_active %>%
  dplyr::select(adm1, adm2, hf, month, year, Date, conf, HF_status) %>%
  dplyr::mutate(UID = paste(adm1, adm2, hf, sep = " | ")) %>%
  dplyr::arrange(UID, Date)

HF_cases <- HF_activity %>%
  dplyr::mutate(Date = make_date(year = year, month = month, day = 1)) %>%
  classify_activity_status("conf")

HF_cases_2023 <- ver %>%
  classify_activity_status("conf")

HF_cases_combined <- dplyr::bind_rows(HF_cases, HF_cases_2023)

# -------------------------
# 12) Reporting rate (weighted & unweighted)
# -------------------------
hf_avg_monthly <- HF_cases_combined %>%
  dplyr::group_by(adm2, UID, month) %>%
  dplyr::summarise(avg_cases = mean(conf, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(avg_cases = replace_na(avg_cases, 0))

district_avg_monthly <- hf_avg_monthly %>%
  dplyr::group_by(adm2, month) %>%
  dplyr::summarise(district_total_avg = sum(avg_cases, na.rm = TRUE), .groups = "drop")

hf_weights <- hf_avg_monthly %>%
  dplyr::left_join(district_avg_monthly, by = c("adm2","month")) %>%
  dplyr::mutate(
    weight = if_else(district_total_avg > 0, avg_cases / district_total_avg, 0),
    weight = replace(weight, !is.finite(weight), 0)
  )

hf_data_weighted <- HF_cases_combined %>%
  dplyr::distinct(adm2, Date, UID, .keep_all = TRUE) %>%
  dplyr::left_join(hf_weights %>% select(UID, month, weight, avg_cases), by = c("UID","month")) %>%
  dplyr::mutate(avg_cases_adjusted = if_else(HF_status == "inactive", 0, avg_cases))

district_reporting_rates <- hf_data_weighted %>%
  dplyr::group_by(adm2, Date) %>%
  dplyr::summarise(
    n_hfs         = n(),
    n_active      = sum(HF_status == "active"),
    total_weight  = sum(weight, na.rm = TRUE),
    weighted_rate = sum(weight[HF_status == "active"], na.rm = TRUE),
    unweighted_rate = mean(!is.na(conf) & conf > 0 & HF_status == "active", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(unweighted_rate = replace_na(unweighted_rate, 0))

# -------------------------
# 13) Outlier detection utilities (3σ, MAD, IQR)
# -------------------------
detect_outliers <- function(df, value_col) {
  stopifnot(value_col %in% names(df))
  stopifnot(is.numeric(df[[value_col]]) || is.integer(df[[value_col]]))
  
  stats <- df %>%
    dplyr::group_by(adm1, adm2, hf, UID, year) %>%
    dplyr::summarise(
      mean_v  = mean(.data[[value_col]], na.rm = TRUE),
      sd_v    = sd(.data[[value_col]], na.rm = TRUE),
      med_v   = median(.data[[value_col]], na.rm = TRUE),
      mad_v   = mad(.data[[value_col]], constant = 1, na.rm = TRUE),
      q1      = quantile(.data[[value_col]], 0.25, na.rm = TRUE),
      q3      = quantile(.data[[value_col]], 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      lower_3sd = mean_v - 3*sd_v,
      upper_3sd = mean_v + 3*sd_v,
      lower_mad = med_v - 15*mad_v,
      upper_mad = med_v + 15*mad_v,
      iqr       = q3 - q1,
      lower_iqr = q1 - 1.5*iqr,
      upper_iqr = q3 + 1.5*iqr
    )
  
  df %>%
    dplyr::left_join(stats, by = c("adm1","adm2","hf","UID","year")) %>%
    dplyr::mutate(
      out_3sd = if_else(.data[[value_col]] < lower_3sd | .data[[value_col]] > upper_3sd, "outlier","ok"),
      out_mad = if_else(.data[[value_col]] < lower_mad | .data[[value_col]] > upper_mad, "outlier","ok"),
      out_iqr = if_else(.data[[value_col]] < lower_iqr | .data[[value_col]] > upper_iqr, "outlier","ok")
    )
}

# -------------------------
# 14) Outlier imputation: nearest non-zero neighbors
# -------------------------
impute_neighbors_mean <- function(vec) {
  # vec is a numeric vector sorted in time
  n <- length(vec)
  out <- vec
  for (i in seq_len(n)) {
    if (is.na(out[i]) || out[i] == 0) {
      # search backward
      last_valid <- NA_real_
      if (i > 1) {
        for (j in (i-1):1) {
          if (!is.na(out[j]) && out[j] != 0) { last_valid <- out[j]; break }
        }
      }
      # search forward
      next_valid <- NA_real_
      if (i < n) {
        for (j in (i+1):n) {
          if (!is.na(out[j]) && out[j] != 0) { next_valid <- out[j]; break }
        }
      }
      if (!is.na(last_valid) && !is.na(next_valid)) {
        out[i] <- mean(c(last_valid, next_valid))
      } else {
        out[i] <- out[i]  # leave as is (NA or 0)
      }
    }
  }
  out
}

# Apply imputation using the valid neighbors

data_long <- HF_cases_combined %>% dplyr::pivot_longer(cols = susp_u5:maldth, names_to = "var", values_to = "val")
long_df <- data_long %>% dplyr::group_by(UID, var) %>% 
            dplyr::arrange(Date) %>% 
            dplyr::mutate(val_imp = impute_neighbors_mean(val)) %>% dplyr::ungroup()

# -------------------------
# 15) District-month aggregation (example)
# -------------------------
# NOTE: raw_data_finale holds 2023; bfa_data_monthly_hf_2016_2022 holds 2016–22
raw_data_all <- dplyr::bind_rows(
  bfa_data_monthly_hf_2016_2022 %>% dplyr::mutate(source = "2016_2022"),
  raw_data_finale %>% dplyr::mutate(source = "2023")
)

monthly_data_DS <- raw_data_all %>%
  dplyr::group_by(adm1, adm2, year, month) %>%
  dplyr::summarise(across(c(susp_u5:maldth), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# 16) Save data

# ============================
write_csv(district_reporting_rates, "outputs/district_reporting_rates.csv")
write_csv(out_conf_imputed, "outputs/hf_conf_with_outlier_imputation.csv")
write_csv(monthly_DS, "outputs/monthly_DS_aggregates.csv")
