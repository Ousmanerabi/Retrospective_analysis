# Packages
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(lubridate)
library(purrr)
library(forcats)

# ------------------------------------------------------------
# EXPECTED INPUT OBJECTS (rename here if your objects differ)
# ------------------------------------------------------------
# nombre_de_jours_de_ruptures_intrants_palu_2023
# BFA_donnees_gestion_stock_2016_2021_update
# tt_consultations_2020_HF
# all_causes_visits
# pop_DS_2016_2022               # columns: adm2, year, District.Pop
# population_moins_de_5_ans_et_total
# BFA_trtm_region
# yearly_DS_incidence
# coverag3

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

# Normalize administrative names:
# - trim spaces, remove DS/CHR/CHU prefixes, harmonize special cases,
# - remove accents, set to lowercase.
normalize_adm <- function(x) {
  x |>
    str_trim() |>
    str_replace_all("\\s+", " ") |>
    str_remove_all("^(DS|CHR|CHU)\\s+") |>
    str_replace_all("(?i)Hôpital\\s+Paul\\s+VI", "Sig-Noghin") |>
    str_replace_all("(?i)Hôpital\\s+Saint\\s+Camille\\s+de\\s+Ouagadougou", "Bogodogo") |>
    str_replace_all("(?i)Gynéco\\s+Obstétricale\\s+Hôpital\\s+SCHIPHRA", "Nongr-Massom") |>
    str_replace_all("’", "'") |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower()
}

# French month -> numeric
month_fr_to_num <- function(m) {
  key <- c("janvier"=1,"fevrier"=2,"février"=2,"mars"=3,"avril"=4,"mai"=5,"juin"=6,
           "juillet"=7,"aout"=8,"août"=8,"septembre"=9,"octobre"=10,"novembre"=11,"decembre"=12,"décembre"=12)
  unname(key[str_to_lower(stri_trans_general(m, "Latin-ASCII"))])
}

# Cap stockout days by the true number of days in that month (handles leap years)
cap_days_by_month <- function(days, month, year) {
  mdays <- days_in_month(make_date(year, month, 1))
  pmin(coalesce(as.numeric(days), 0), as.numeric(mdays))
}

# Replace NaN with 0
nan0 <- function(x) { x[is.nan(x)] <- 0; x }

# ACT pack-size columns
act_cols <- c("AL_plaq6_stockout_days","AL_plaq12_stockout_days","AL_plaq18_stockout_days","AL_plaq24_stockout_days")

# All stockout columns to process
stock_vars <- c("llins_stockout_days", act_cols, "artinj_stockout_days", "rdt_stockout_days", "iptp_stockout_days")

# ------------------------------------------------------------
# A) STOCKOUTS 2023 (monthly)
# ------------------------------------------------------------
stock23_raw <- nombre_de_jours_de_ruptures_intrants_palu_2023 |>
  select(
    adm1 = orgunitlevel2,
    adm3 = orgunitlevel3,
    adm2_raw = orgunitlevel4,
    hf = organisationunitname,
    period = periodname,
    llins_stockout_days = `Intrant-MILDA routine Nombre de jours de rupture`,
    AL_plaq6_stockout_days  = `Intrant-Artemether + Lumefantrine 20mg/120mg pl/6 cp Nombre de jours de rupture`,
    AL_plaq12_stockout_days = `Intrant-Artemether + Lumefantrine 20mg/120mg pl/12 cp Nombre de jours de rupture`,
    AL_plaq18_stockout_days = `Intrant-Artemether + Lumefantrine 20mg/120mg pl/18 cp Nombre de jours de rupture`,
    AL_plaq24_stockout_days = `Intrant-Artemether + Lumefantrine 20mg/120mg pl/24cp Nombre de jours de rupture`,
    artinj_stockout_days    = `Intrant-Artesunate injectable 60mg Nombre de jours de rupture`,
    rdt_stockout_days       = `Intrant-TDR paludisme Nombre de jours de rupture`,
    iptp_stockout_days      = `Intrant-Sulfadoxine Pyrimethamine (500/25) mg pour TPIg Nombre de jours de rupture`
  ) |>
  separate_wider_delim(period, delim = " ", names = c("month_fr","year")) |>
  mutate(
    month = month_fr_to_num(month_fr),
    year  = as.integer(year),
    adm2  = normalize_adm(coalesce(adm2_raw, adm3))
  ) |>
  select(-month_fr, -adm2_raw)

stock23 <- stock23_raw |>
  mutate(across(all_of(stock_vars), ~ cap_days_by_month(.x, month, year))) |>
  group_by(adm2, month, year) |>
  summarise(across(all_of(stock_vars), ~ mean(as.numeric(.x), na.rm = TRUE)), .groups = "drop") |>
  mutate(across(all_of(stock_vars), ceiling)) |>
  mutate(across(where(is.numeric), nan0))

# ------------------------------------------------------------
# B) STOCKOUTS 2016–2021 (monthly)
# ------------------------------------------------------------
stock16_21 <- BFA_donnees_gestion_stock_2016_2021_update |>
  transmute(
    adm1,
    adm2 = normalize_adm(adm2),
    month = as.integer(month),
    year  = as.integer(year),
    llins_stockout_days,
    AL_plaq6_stockout_days  = AL_plaq6_sotckout_days,
    AL_plaq12_stockout_days = AL_plaq12_sotckout_days,
    AL_plaq18_stockout_days = AL_plaq18_sotckout_days,
    AL_plaq24_stockout_days = AL_plaq24_sotckout_days,
    artinj_stockout_days,
    rdt_stockout_days,
    iptp_stockout_days
  ) |>
  mutate(across(all_of(stock_vars), ~ cap_days_by_month(.x, month, year))) |>
  group_by(adm2, month, year) |>
  summarise(across(all_of(stock_vars), ~ mean(as.numeric(.x), na.rm = TRUE)), .groups = "drop") |>
  mutate(across(all_of(stock_vars), ceiling)) |>
  mutate(across(where(is.numeric), nan0)) |>
  filter(year != 2019) # drop 2019 if required

# ------------------------------------------------------------
# C) MERGE 2016–2021 + 2023 and derive annual indicators
# ------------------------------------------------------------
stock_2016_2023 <- bind_rows(stock16_21, stock23) |>
  mutate(adm2 = str_replace_all(adm2, "\\s+", " "))

# Row-wise mean ACT (across 4 ACT presentations)
stock_2016_2023 <- stock_2016_2023 |>
  rowwise() |>
  mutate(mean_ACT = ceiling(mean(c_across(all_of(act_cols)), na.rm = TRUE))) |>
  ungroup()

# Annual sums by district
stock_year_2016_2023 <- stock_2016_2023 |>
  group_by(adm2, year) |>
  summarise(across(c(all_of(stock_vars), mean_ACT), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(
    # targeted spelling fixes
    adm2 = case_when(
      adm2 == "bittou" ~ "bitou",
      adm2 == "gorom-gorom" ~ "gorom",
      adm2 == "karangassovigue" ~ "karangasso vigue",
      adm2 == "n'dorola" ~ "ndorola",
      TRUE ~ adm2
    )
  )

# ------------------------------------------------------------
# D) ALL-CAUSE OUTPATIENT CONSULTATIONS (monthly → annual)
# ------------------------------------------------------------

# 2020 aggregation from age/sex buckets
consult_2020 <- tt_consultations_2020_HF |>
  rowwise() |>
  mutate(
    Janvier   = sum(`Janvier 2020 < 1 ans`, `Janvier 2020 1-4 ans`, `Janvier 2020 5-14 ans`, `Janvier 2020 Adultes Masculins`, `Janvier 2020 Adultes Feminins`, na.rm = TRUE),
    Fevrier   = sum(`Février 2020 < 1 ans`, `Février 2020 1-4 ans`, `Février 2020 5-14 ans`, `Février 2020 Adultes Masculins`, `Février 2020 Adultes Feminins`, na.rm = TRUE),
    Mars      = sum(`Mars 2020 < 1 ans`, `Mars 2020 1-4 ans`, `Mars 2020 5-14 ans`, `Mars 2020 Adultes Masculins`, `Mars 2020 Adultes Feminins`, na.rm = TRUE),
    Avril     = sum(`Avril 2020 < 1 ans`, `Avril 2020 1-4 ans`, `Avril 2020 5-14 ans`, `Avril 2020 Adultes Masculins`, `Avril 2020 Adultes Feminins`, na.rm = TRUE),
    Mai       = sum(`Mai 2020 < 1 ans`, `Mai 2020 1-4 ans`, `Mai 2020 5-14 ans`, `Mai 2020 Adultes Masculins`, `Mai 2020 Adultes Feminins`, na.rm = TRUE),
    Juin      = sum(`Juin 2020 < 1 ans`, `Juin 2020 1-4 ans`, `Juin 2020 5-14 ans`, `Juin 2020 Adultes Masculins`, `Juin 2020 Adultes Feminins`, na.rm = TRUE),
    Juillet   = sum(`Juillet 2020 1-4 ans`, `Juillet 2020 < 1 ans`, `Juillet 2020 5-14 ans`, `Juillet 2020 Adultes Masculins`, `Juillet 2020 Adultes Feminins`, na.rm = TRUE),
    Aout      = sum(`Août 2020 < 1 ans`, `Août 2020 1-4 ans`, `Août 2020 5-14 ans`, `Août 2020 Adultes Masculins`, `Août 2020 Adultes Feminins`, na.rm = TRUE),
    Septembre = sum(`Septembre 2020 < 1 ans`, `Septembre 2020 1-4 ans`, `Septembre 2020 5-14 ans`, `Septembre 2020 Adultes Masculins`, `Septembre 2020 Adultes Feminins`, na.rm = TRUE),
    Octobre   = sum(`Octobre 2020 < 1 ans`, `Octobre 2020 1-4 ans`, `Octobre 2020 5-14 ans`, `Octobre 2020 Adultes Masculins`, `Octobre 2020 Adultes Feminins`, na.rm = TRUE),
    Novembre  = sum(`Novembre 2020 < 1 ans`, `Novembre 2020 1-4 ans`, `Novembre 2020 5-14 ans`, `Novembre 2020 Adultes Masculins`, `Novembre 2020 Adultes Feminins`, na.rm = TRUE),
    Decembre  = sum(`Décembre 2020 < 1 ans`, `Décembre 2020 1-4 ans`, `Décembre 2020 5-14 ans`, `Décembre 2020 Adultes Masculins`, `Décembre 2020 Adultes Feminins`, na.rm = TRUE),
    adm2_raw  = ifelse(is.na(orgunitlevel4), orgunitlevel3, orgunitlevel4)
  ) |>
  ungroup() |>
  select(adm2_raw, Janvier:Decembre) |>
  pivot_longer(Janvier:Decembre, names_to = "month_fr", values_to = "allout") |>
  mutate(
    month = recode(month_fr,
      "Janvier"=1, "Fevrier"=2, "Février"=2, "Mars"=3, "Avril"=4, "Mai"=5, "Juin"=6,
      "Juillet"=7, "Aout"=8, "Août"=8, "Septembre"=9, "Octobre"=10, "Novembre"=11, "Decembre"=12, "Décembre"=12
    ),
    year  = 2020L,
    adm2  = normalize_adm(adm2_raw)
  ) |>
  select(adm2, month, year, allout) |>
  group_by(adm2, month, year) |>
  summarise(allout = sum(allout, na.rm = TRUE), .groups = "drop")

# 2023 (example pivot from wide "Month Year" columns)
consult_2023 <- all_causes_visits |>
  pivot_longer(cols = `Janvier 2023`:`Decembre 2023`, names_to = "period", values_to = "allout") |>
  separate_wider_delim(period, delim = " ", names = c("month_fr","year")) |>
  mutate(
    year  = as.integer(year),
    month = month_fr_to_num(month_fr),
    adm2  = normalize_adm(ifelse(is.na(orgunitlevel4), orgunitlevel3, orgunitlevel4))
  ) |>
  select(adm2, month, year, allout) |>
  group_by(adm2, month, year) |>
  summarise(allout = sum(allout, na.rm = TRUE), .groups = "drop")

consult_monthly <- bind_rows(consult_2020, consult_2023)

# ------------------------------------------------------------
# E) POPULATIONS (total + U5) and consultation rate
# ------------------------------------------------------------
# U5 population long (adm2, year, U5_pop)
pop_u5_long <- population_moins_de_5_ans_et_total |>
  mutate(adm2 = normalize_adm(orgunitlevel4)) |>
  select(adm2, `2016`:`2022`) |>
  pivot_longer(cols = !adm2, names_to = "year", values_to = "U5_pop") |>
  mutate(year = as.integer(year))

# Total population (adm2, year, District.Pop)
pop_total <- pop_DS_2016_2022 |>
  mutate(
    adm2 = normalize_adm(adm2),
    year = as.integer(year),
    District.Pop = as.numeric(as.character(District.Pop))
  )

# ------------------------------------------------------------
# F) TREATMENT SEEKING (regional example; optional)
# ------------------------------------------------------------
trtm_region <- BFA_trtm_region |>
  transmute(
    adm1 = normalize_adm(adm1),
    year,
    trtm_seeking = prop_publ + prop_priv,
    no_trtm = No_treat
  )

# ------------------------------------------------------------
# G) BUILD AN ANALYSIS-READY COVARIATE DATASET (annual)
# ------------------------------------------------------------
consult_year <- consult_monthly |>
  group_by(adm2, year) |>
  summarise(allout = sum(allout, na.rm = TRUE), .groups = "drop")

covars <- consult_year |>
  left_join(pop_total, by = c("adm2","year")) |>
  mutate(usage_rate = round(allout / District.Pop, 4)) |>
  left_join(pop_u5_long, by = c("adm2","year")) |>
  left_join(stock_year_2016_2023, by = c("adm2","year")) |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# SMC cycles (example: districts with 5 cycles in 2021–2022)
districts_5cycles <- c('banfora','mangodara','sindou','leo','sapouy','po','pama','dafra',
                       'do','hounde','karangasso vigue','lena','ndorola','orodara',
                       'batie','dano','diebougou','gaoua','kampti')

data_cps <- yearly_DS_incidence |>
  transmute(adm2 = normalize_adm(adm2), year) |>
  left_join(coverag3 |> mutate(adm2 = normalize_adm(adm2)), by = "adm2") |>
  mutate(cycles_SMC = case_when(
    nbre_year_cps > 0 & year %in% c(2016, 2017, 2018, 2020) ~ 4,
    nbre_year_cps > 0 & year %in% c(2021, 2022) & adm2 %in% districts_5cycles ~ 5,
    nbre_year_cps == 0 ~ 0,
    TRUE ~ 4
  ))

covars <- covars |>
  left_join(data_cps, by = c("adm2","year")) |>
  mutate(cycles_SMC = replace_na(cycles_SMC, 0L))

# ------------------------------------------------------------
# H) (Optional) ITN access/usage shocks / interpolation can be joined here
# ------------------------------------------------------------
# itn_cov_adj <- itn_cov |> mutate( ... )  # join by adm2, year later

# ------------------------------------------------------------
# I) FINAL: variables ready for multivariable analysis
# ------------------------------------------------------------
covars_final <- covars |>
  transmute(
    adm2,
    year,
    usage_rate,                 # outpatient consultations per capita
    U5_pop,
    llins_stockout_days,
    rdt_stockout_days,
    artinj_stockout_days,
    iptp_stockout_days,
    mean_ACT,                   # mean #days stockout across ACT pack sizes
    cycles_SMC                  # SMC cycles (0/4/5)
    # add ITN_access / ITN_usage if available
  )

glimpse(covars_final)
