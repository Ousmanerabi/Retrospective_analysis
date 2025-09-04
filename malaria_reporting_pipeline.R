# ============================
# 0) Libraries
# ============================
suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, ggplot2, readr, etc.
  library(sf)          # modern spatial I/O
  library(lubridate)   # dates
  library(readxl)      # read_xls/xlsx
  library(janitor)     # clean_names, etc.
  library(naniar)      # NA helpers
  library(stringi)     # accent handling
  library(stringdist)  # fuzzy matching
  library(zoo)         # na.locf
  library(viridis)     # palettes
})

# ============================
# 1) Helpers
# ============================

# Normalize administrative names:
# - trim, collapse spaces, remove DS/CHR/CHU prefixes,
# - special replacements, remove accents, lowercase.
normalize_adm <- function(x) {
  x |>
    str_trim() |>
    str_replace_all("\\s+", " ") |>
    str_remove_all("^(DS|CHR|CHU)\\s+") |>
    str_replace_all("(?i)Hôpital\\s+Paul\\s+VI", "Sig-Noghin") |>
    str_replace_all("(?i)Hôpital\\s+Saint\\s+Camille\\s+de\\s+Ouagadougou", "Bogodogo") |>
    str_replace_all("(?i)Gynéco\\s+Obstétricale\\s+Hôpital\\s+SCHIPHRA", "Nongr-Massom") |>
    str_replace_all("’", "'") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower()
}

# Map French month names → month number
month_fr_to_num <- function(m) {
  key <- c("janvier"=1,"fevrier"=2,"février"=2,"mars"=3,"avril"=4,"mai"=5,"juin"=6,
           "juillet"=7,"aout"=8,"août"=8,"septembre"=9,"octobre"=10,"novembre"=11,"decembre"=12,"décembre"=12)
  unname(key[str_to_lower(stringi::stri_trans_general(m, "Latin-ASCII"))])
}

# Classify facility activity with 6+ months inactive rule (excludes 2019 globally)
classify_activity_status <- function(df, conf_col = "conf", inactive_threshold = 6) {
  df %>%
    filter(year(Date) != 2019) %>%
    group_by(UID) %>%
    complete(
      Date = {
        rng <- range(Date, na.rm = TRUE)
        dates <- seq(rng[1], rng[2], by = "month")
        dates[!year(dates) %in% 2019]
      },
      fill = setNames(list(NA), conf_col)
    ) %>%
    filter(year(Date) != 2019) %>%
    arrange(Date, .by_group = TRUE) %>%
    mutate(
      # raw status from conf counts
      temp_status = if_else(is.na(.data[[conf_col]]) | .data[[conf_col]] == 0, "inactive", "active"),
      # carry the first "active" forward; default is "inactive"
      active_status = zoo::na.locf(ifelse(temp_status == "active", temp_status, NA),
                                   na.rm = FALSE, fromLast = FALSE, default = "inactive"),
      # detect long inactive runs
      rl  = rle(temp_status),
      run_lengths = rep(rl$lengths, rl$lengths),
      run_values  = rep(rl$values,   rl$lengths),
      HF_status = case_when(
        all(temp_status == "inactive") ~ "inactive",
        run_values == "inactive" & run_lengths >= inactive_threshold ~ "inactive",
        TRUE ~ active_status
      )
    ) %>%
    ungroup() %>%
    select(-temp_status, -active_status, -rl, -run_lengths, -run_values)
}

# Simple outlier detector (3σ, MAD(15), IQR) for one numeric column
detect_outliers <- function(df, num_col) {
  stopifnot(num_col %in% names(df), is.numeric(df[[num_col]]))
  stats <- df %>%
    group_by(adm1, adm2, hf, UID, year) %>%
    summarise(
      mean_v = mean(.data[[num_col]], na.rm = TRUE),
      sd_v   = sd(.data[[num_col]],   na.rm = TRUE),
      med_v  = median(.data[[num_col]], na.rm = TRUE),
      mad_v  = mad(.data[[num_col]], constant = 1, na.rm = TRUE),
      q1 = quantile(.data[[num_col]], .25, na.rm = TRUE),
      q3 = quantile(.data[[num_col]], .75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      lower_3sd = mean_v - 3*sd_v,
      upper_3sd = mean_v + 3*sd_v,
      lower_mad = med_v - 15*mad_v,
      upper_mad = med_v + 15*mad_v,
      iqr = q3 - q1,
      lower_iqr = q1 - 1.5*iqr,
      upper_iqr = q3 + 1.5*iqr
    )
  
  df %>%
    left_join(stats, by = c("adm1","adm2","hf","UID","year")) %>%
    mutate(
      outliers_3sd = if_else(.data[[num_col]] < lower_3sd | .data[[num_col]] > upper_3sd, "outlier", "ok"),
      outliers_mad = if_else(.data[[num_col]] < lower_mad | .data[[num_col]] > upper_mad, "outlier", "ok"),
      outliers_iqr = if_else(.data[[num_col]] < lower_iqr | .data[[num_col]] > upper_iqr, "outlier", "ok")
    )
}

# Impute outliers by averaging nearest non-zero neighbors (within a vector)
impute_neighbors <- function(v, idx) {
  out <- rep(NA_real_, length(v))
  for (i in idx) {
    # backward
    lb <- NA
    if (i > 1) {
      for (j in (i-1):1) if (!is.na(v[j]) && v[j] != 0) { lb <- v[j]; break }
    }
    # forward
    lf <- NA
    if (i < length(v)) {
      for (j in (i+1):length(v)) if (!is.na(v[j]) && v[j] != 0) { lf <- v[j]; break }
    }
    out[i] <- if (is.na(lb) || is.na(lf)) NA_real_ else mean(c(lb, lf))
  }
  out
}

# ============================
# 2) Inputs
# ============================

# -- Shapefile (prefer sf::st_read)

# HD_sf <- st_read("~/NU-malaria-team Dropbox/data/togo/Shapefiles/Districts/District sanitaires.shp", options = "ENCODING=UTF-8")

# -- Bulk xls read (if needed)
# TODO: update working dir or use absolute path
# setwd("C:/path/to/your/xls/folder")
# xls_files <- list.files(pattern = "\\.xls$", full.names = TRUE)
# myfiles <- lapply(xls_files, read_xls)

# -- 2024 data frame already in memory:
# TODO: replace Données_Palu_2024 by your actual object name
raw_2024 <- Données_Palu_2024 %>%
  transmute(
    adm1 = orgunitlevel2,
    adm2 = orgunitlevel4,
    hf   = organisationunitname,
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
    maldth_u5   = `Deces dus au paludisme grave enfants de <5 ans`,
    maldth_514  = `Deces dus au paludisme grave 5-14 ans`,
    maldth_ov15 = `Deces dus au paludisme grave 15 ans et +`
  ) %>%
  # collapsed totals by age groups
  rowwise() %>%
  mutate(
    susp_ov5   = sum(susp_514, susp_ov15, na.rm = TRUE),
    susp       = sum(susp_u5, susp_ov5, na.rm = TRUE),
    testrdt_ov5 = sum(testrdt_514, testrdt_ov15, na.rm = TRUE),
    testrdt     = sum(testrdt_u5, testrdt_ov5, na.rm = TRUE),
    testmic_ov5 = sum(testmic_514, testmic_ov15, na.rm = TRUE),
    testmic     = sum(testmic_u5, testmic_ov5, na.rm = TRUE),
    confrdt_ov5 = sum(confrdt_514, confrdt_ov15, na.rm = TRUE),
    confrdt     = sum(confrdt_u5, confrdt_ov5, na.rm = TRUE),
    confmic_ov5 = sum(confmic_514, confmic_ov15, na.rm = TRUE),
    confmic     = sum(confmic_u5, confmic_ov5, na.rm = TRUE),
    conf        = sum(confrdt, confmic, na.rm = TRUE),
    test        = sum(testrdt, testmic, na.rm = TRUE),
    test_u5     = sum(testrdt_u5, testmic_u5, na.rm = TRUE),
    test_ov5    = sum(testrdt_ov5, testmic_ov5, na.rm = TRUE),
    maltreat_ov5 = sum(maltreat_514, maltreat_ov15, na.rm = TRUE),
    maltreat     = sum(maltreat_u5, maltreat_ov5, na.rm = TRUE),
    pres_ov5     = sum(pres_514, pres_ov15, na.rm = TRUE),
    pres         = sum(pres_u5, pres_ov5, na.rm = TRUE),
    conf_ov5     = sum(conf_514, conf_ov15, na.rm = TRUE),
    pressev_ov5  = sum(pressev_514, pressev_ov15, na.rm = TRUE),
    pressev      = sum(pressev_u5, pressev_ov5, na.rm = TRUE),
    confsev_ov5  = sum(confsev_514, confsev_ov15, na.rm = TRUE),
    confsev      = sum(confsev_u5, confsev_ov5, na.rm = TRUE),
    maldth_ov5   = sum(maldth_514, maldth_ov15, na.rm = TRUE),
    maldth       = sum(maldth_u5, maldth_ov5, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # split "Janvier 2024" → month/year numeric
  separate_wider_delim(period, delim = " ", names = c("month_fr","year")) %>%
  mutate(
    month = month_fr_to_num(month_fr),
    year  = as.integer(year)
  ) %>%
  select(-month_fr) %>%
  # standardize accents everywhere (avoid per-column conversions)
  mutate(across(everything(), ~ if (is.character(.x)) stringi::stri_trans_general(.x, "Latin-ASCII") else .x)) %>%
  # normalize adm2 labels + UID
  mutate(
    adm2 = normalize_adm(adm2),
    UID  = paste(adm1, adm2, hf, sep = " | ")
  )

# Convenience: ver = 2024 confirmations by HF/month
ver <- raw_2024 %>%
  transmute(adm1, adm2, hf, year, month, conf, UID,
            Date = make_date(year, month, 1)) %>%
  mutate(conf = na_if(conf, 0))  # treat zero as NA for activity logic

# ============================
# 3) Historic data & matching (2016–2022 vs 2024)
# ============================

# TODO: replace with your actual file paths/objects
BFA_routine_data_2016_2022 <- read_excel("C:/Users/.../BFA_monthly_routine_data_2016_2022.xlsx")
HF_cases_active            <- read.csv("C:/Users/.../HF_cases_active.csv")

# HF lists & UIDs
liste_hf_2024 <- raw_2024 %>% select(adm1, adm2, hf) %>% distinct() %>%
  mutate(UID = paste(adm1, adm2, hf, sep = " | "))
liste_hf_2016_2022 <- HF_cases_active %>%
  select(adm1, adm2, hf) %>% distinct() %>%
  mutate(UID = paste(adm1, normalize_adm(adm2), hf, sep = " | "))

# Fuzzy matching UID (Jaro-Winkler), returns best pair per UID
matching_uid <- function(u_old, u_new) {
  grid <- expand.grid(Col1 = u_old, Col2 = u_new, stringsAsFactors = FALSE) %>%
    mutate(score = (1 - stringdist(Col1, Col2, method = "jw")) * 100)
  # exacts
  exact <- grid %>% filter(Col1 == Col2) %>% mutate(score = 100)
  # best non-exact per Col1
  best  <- grid %>%
    filter(!(Col1 %in% exact$Col1)) %>%
    group_by(Col1) %>% slice_max(order_by = score, n = 1, with_ties = FALSE) %>% ungroup()
  bind_rows(exact, best) %>% distinct(Col1, .keep_all = TRUE)
}

data_match <- matching_uid(liste_hf_2016_2022$UID, liste_hf_2024$UID)

# Map old UID → new UID, then join to raw_2024
liste_fusion <- data_match %>%
  select(UID_old = Col1, UID_new = Col2) %>%
  left_join(liste_hf_2024 %>% select(UID_new = UID, adm1, adm2), by = "UID_new")

raw_2024_matched <- raw_2024 %>%
  # if you need to replace existing UID with matched one:
  left_join(liste_fusion, by = c("adm1","adm2", "UID" = "UID_new")) %>%
  mutate(UID = coalesce(UID, UID_old)) %>%
  select(-UID_old)

# ============================
# 4) Activity classification (2016–2022 + 2024)
# ============================

HF_activity <- HF_cases_active %>%
  mutate(
    adm2 = normalize_adm(adm2),
    UID  = paste(adm1, adm2, hf, sep = " | "),
    Date = make_date(year, month, 1)
  ) %>%
  arrange(UID, Date)

HF_cases_2016_2022 <- HF_activity %>%
  classify_activity_status(conf_col = "conf")

HF_cases_2024 <- ver %>%
  classify_activity_status(conf_col = "conf")

HF_cases_all <- bind_rows(HF_cases_2016_2022, HF_cases_2024)

# ============================
# 5) Reporting rates (weighted & unweighted)
# ============================

# Historical average cases per HF by month → weights
hf_avg_monthly <- HF_cases_all %>%
  group_by(adm2, UID, month = month(Date)) %>%
  summarise(avg_cases = mean(conf, na.rm = TRUE), .groups = "drop") %>%
  mutate(avg_cases = replace_na(avg_cases, 0))

district_avg_monthly <- hf_avg_monthly %>%
  group_by(adm2, month) %>%
  summarise(district_total_avg = sum(avg_cases, na.rm = TRUE), .groups = "drop")

hf_weights <- hf_avg_monthly %>%
  left_join(district_avg_monthly, by = c("adm2","month")) %>%
  mutate(weight = if_else(district_total_avg > 0, avg_cases / district_total_avg, 0))

# Merge weights to daily data and zero out averages for inactive HFs
hf_weighted_data <- HF_cases_all %>%
  distinct(adm2, Date, UID, .keep_all = TRUE) %>%
  mutate(month = month(Date)) %>%
  left_join(hf_weights, by = c("adm2","UID","month")) %>%
  mutate(avg_cases_adj = if_else(HF_status == "inactive", 0, replace_na(avg_cases, 0)),
         weight = replace_na(weight, 0))

district_reporting_rates <- hf_weighted_data %>%
  group_by(adm2, Date) %>%
  summarise(
    n_hfs       = n_distinct(UID),
    n_active    = sum(HF_status == "active", na.rm = TRUE),
    weighted_rate   = sum(weight[HF_status == "active"], na.rm = TRUE),
    unweighted_rate = mean(!is.na(conf) & conf > 0 & HF_status == "active", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(unweighted_rate = replace_na(unweighted_rate, 0))

# Quick diagnostic
district_reporting_rates %>%
  filter(n_active < n_hfs & weighted_rate >= 1) %>%
  arrange(desc(weighted_rate)) %>%
  head()

# ============================
# 6) Outlier detection (example on "conf")
# ============================

# Build a harmonized monthly HF dataset (2016–2024)
bfa_data_monthly_hf <- bind_rows(
  HF_cases_active %>%
    mutate(
      adm2 = normalize_adm(adm2),
      UID  = paste(adm1, adm2, hf, sep = " | ")
    ) %>%
    select(adm1, adm2, hf, month, year, conf, UID),
  raw_2024 %>% select(adm1, adm2, hf, month, year, conf, UID)
) %>%
  mutate(across(c(month, year, conf), as.numeric)) %>%
  replace_na(list(conf = 0))

# Detect outliers in "conf"
out_conf <- detect_outliers(bfa_data_monthly_hf, "conf")

# OPTIONAL IMPUTATION:
# For each (adm1,adm2,hf,UID,year), impute HALPER outliers by neighbor average
out_conf_imputed <- out_conf %>%
  arrange(UID, year, month) %>%
  group_by(adm1, adm2, hf, UID, year) %>%
  mutate(
    imputed = {
      v  <- conf
      ix <- which(outliers_mad == "outlier")
      imp <- impute_neighbors(v, ix)
      # replace only the indices flagged as outliers_mad
      v[ix] <- ifelse(is.na(imp[ix]), v[ix], ceiling(imp[ix]))
      v
    }
  ) %>%
  ungroup()

# ============================
# 7) Aggregations (district-month)
# ============================

monthly_DS <- raw_2024 %>%
  group_by(adm1, adm2, year, month) %>%
  summarise(across(susp_u5:maldth, ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# ============================
# 8) Plot examples
# ============================

# Heatmap of weighted reporting
district_report <- district_reporting_rates %>%
  mutate(Month = as.yearmon(Date))

ggplot(district_report, aes(x = Month, y = fct_reorder(adm2, as.numeric(adm2)), fill = weighted_rate)) +
  geom_tile() +
  scale_fill_viridis(name = "Weighted\nreporting") +
  labs(x = "Date", y = "Health districts", title = "Weighted reporting rate by district & month") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Facilities by reporting status over time (example)
classify_reporting <- function(n_zeros, max_months) {
  case_when(
    n_zeros >= max_months ~ "0_report",
    n_zeros > 0 & n_zeros < max_months ~ "at_least_one",
    n_zeros == 0 ~ "full_100",
    TRUE ~ NA_character_
  )
}

bfa_hf_reporting_summary <- HF_cases_all %>%
  group_by(UID, year = year(Date)) %>%
  summarise(n_zero = sum(is.na(conf), na.rm = TRUE), .groups = "drop") %>%
  mutate(
    max_months = if_else(year == 2023, 12L, 12L),   # adjust per year range if needed
    reporting  = classify_reporting(n_zero, max_months)
  ) %>%
  filter(!is.na(reporting)) %>%
  count(year, reporting, name = "total")

ggplot(bfa_hf_reporting_summary %>% filter(reporting != "0_report"),
       aes(x = year, y = total, color = reporting)) +
  geom_line() + geom_point(size = 2) +
  scale_color_manual(values = c("full_100" = "#1b9e77", "at_least_one" = "#d95f02"),
                     labels = c("Full Reporting (100%)", "At Least One Report")) +
  labs(title = "Health Facilities Reporting Trends",
       x = "Year", y = "Number of Health Facilities", color = "Status") +
  theme_bw()

# ============================
# 9) Save outputs (optional)
# ============================
# write_csv(district_reporting_rates, "outputs/district_reporting_rates.csv")
# write_rds(out_conf_imputed, "outputs/hf_conf_with_outlier_imputation.rds")
# write_csv(monthly_DS, "outputs/monthly_DS_aggregates.csv")
