###

### Lib
library(raster, exclude = c("select"))
library(haven)
library(fasterize)
library(tidyverse)
library(sf)
library(lubridate)
library(knitr)
#library(haven)
#library(plotly)
library(ggsci)
library(gridExtra)
library(tmap)
library(scico)
#library(plotly)
library(janitor)
library(naniar)
library(rio)
library(dplyr)
library(readxl)
library(reshape2)
library(viridis)
library(tidyr)
library(stringdist)
library(ggplot2)

##

HDsh <- readOGR("~/NU-malaria-team Dropbox/data/togo/Shapefiles/Districts/District sanitaires.shp", use_iconv=TRUE, encoding= "UTF-8")
HD_sf <- st_as_sf(HDsh)
setwd('C:/Users/ood5226.FSM/NU-malaria-team Dropbox/projects/hbhi_burkina/project_notes/publication/retrospective analysis 2023/data/Raw/')
data <- list.files(pattern = ".xls", full.names = TRUE)
myfiles = lapply(data, read_xls)

raw_data2 <- Données_Palu_2024 %>% 
  mutate(adm1 = orgunitlevel2, adm2 = orgunitlevel4) %>%
  #filter(orgunitlevel4 != "") %>% 
  dplyr::select(adm1, adm2, hf = organisationunitname, period =periodname,
                susp_u5 = `Cas suspects de Paludisme enfant <5 ans`, susp_514 = `Cas suspects de Paludisme 5_14 ans`,
                susp_ov15 = `Cas suspects de Paludisme 15 ans et plus`, testrdt_u5 = `TDR realises enfant <5 ans`,
                testrdt_514 = `TDR realises enfant 5-14 ans`, testrdt_ov15 = `TDR realises 15 ans et +`, 
                testmic_u5 = `GE realises enfant <5 ans`, testmic_514 = `GE realises 5-14 ans`, testmic_ov15 = `GE realises 15 ans et +`,
                confrdt_u5 = `TDR positif enfant <5 ans`, confrdt_514 = `TDR positif 5-14 ans`, confrdt_ov15 = `TDR positif 15 ans et +`,
                confmic_u5 = `GE positif enfant <5 ans`, confmic_514 = `GE positif 5-14 ans`, confmic_ov15 = `GE positif 15 ans et +`,
                maltreat_u5 = `Cas de paludisme simple confirmes traites aux ACT enfant <5 ans`, maltreat_514 = `Cas de paludisme simple confirmes traites aux ACT 5-14 ans`,
                maltreat_ov15 = `Cas de paludisme simple confirmes traites aux ACT 15 ans et +`, pres_u5 = `Cas presume de paludisme simple enfant <5 ans`,
                pres_514 = `Cas presume de paludisme simple  5_14 ans`, pres_ov15 = `Cas presume de paludisme simple  15 ans et plus`,
                conf_u5 = `Cas confirmes de paludisme simple enfant <5 ans`, conf_514 = `Cas confirmes de paludisme simple 5-14ans`,
                conf_ov15 = `Cas confirmes de paludisme simple 15 ans et +`, pressev_u5 =`Cas presume de Paludisme grave enfant <5 ans`,
                pressev_514 = `Cas presume de Paludisme grave  5_14 ans`, pressev_ov15 = `Cas presume de Paludisme grave  15 ans et plus`,
                #pressev_preg = `Cas presume de Paludisme grave  femmes enceintes`, 
                confsev_u5 = `Cas confirmes de paludisme grave enfant <5 ans`, confsev_514 = `Cas confirmes de paludisme grave 5-14ans`,
                confsev_ov15 = `Cas confirmes de paludisme grave 15 ans et +`, #confsev_preg = `Cas confirmes de paludisme grave femme enceinte`,
                #iptp3 = `Nombre de femmes enceintes ayant reçu le TPI3`,
                #maldth_preg = `Deces dus au paludisme grave femme enceinte`,
                maldth_u5 = `Deces dus au paludisme grave enfants de <5 ans`, maldth_514 = `Deces dus au paludisme grave 5-14 ans`,
                maldth_ov15 = `Deces dus au paludisme grave 15 ans et +`)%>%
  rowwise() %>%
  mutate(susp_ov5 = sum(susp_514, susp_ov15, na.rm = T), susp = sum(susp_u5, susp_ov5, na.rm = T),
         testrdt_ov5 = sum(testrdt_514, testrdt_ov15, na.rm = T), testrdt = sum(testrdt_u5, testrdt_ov5, na.rm = T),
         testmic_ov5 = sum(testmic_514, testmic_ov15, na.rm = T), testmic = sum(testmic_u5, testmic_ov5, na.rm = T),
         confrdt_ov5 = sum(confrdt_514, confrdt_ov15, na.rm = T), confrdt = sum(confrdt_u5, confrdt_ov5, na.rm = T),
         confmic_ov5 = sum(confmic_514, confmic_ov15, na.rm = T), confmic = sum(confmic_u5, confmic_ov5, na.rm = T),
         conf = sum(confrdt, confmic, na.rm = T), test = sum(testrdt, testmic, na.rm = T), test_u5 = sum(testrdt_u5, testmic_u5, na.rm = T),
         test_ov5 = sum(testrdt_ov5, testmic_ov5, na.rm = T),
         maltreat_ov5 = sum(maltreat_514, maltreat_ov15, na.rm = T), maltreat = sum(maltreat_u5, maltreat_ov5, na.rm = T),
         pres_ov5 = sum(pres_514, pres_ov15, na.rm = T), pres = sum(pres_u5, pres_ov5, na.rm = T), conf_ov5 = sum(conf_514, conf_ov15, na.rm = T),
         pressev_ov5 = sum(pressev_514, pressev_ov15, na.rm = T), pressev = sum(pressev_u5, pressev_ov5, na.rm = T),
         confsev_ov5 = sum(confsev_514, confsev_ov15, na.rm = T), confsev =  sum(confsev_u5, confsev_ov5, na.rm = T),
         maldth_ov5 = sum(maldth_514, maldth_ov15, na.rm = T), maldth = sum(maldth_u5, maldth_ov5, na.rm = T)) %>% 
  separate(period, into = c("month", "year"), sep = " ") %>% 
  mutate(month = case_when(month == "Janvier" ~ 1,
                           month == "Février" ~ 2,
                           month == "Mars" ~ 3,
                           month == "Avril" ~ 4,
                           month == "Mai" ~ 5,
                           month == "Juin" ~ 6,
                           month == "Juillet" ~ 7,
                           month == "Août" ~ 8,
                           month == "Septembre" ~ 9,
                           month == "Octobre" ~ 10,
                           month == "Novembre" ~ 11,
                           month == "Décembre" ~ 12
  )) %>% 
  mutate(month = as.numeric(month), year = as.numeric(year))


## Check the number of month reported by hf to drop all hf which is not reporting

raw_data1 = raw_data2 %>%
mutate(across(.cols = everything(),
              .fns = ~ stringi::stri_trans_general(., id = "Latin-ASCII")))




### Rename all HD

raw_data1$adm2 <- raw_data1$adm2
raw_data1$adm2[raw_data1$adm2 == "DS Banfora"] <- "Banfora"
raw_data1$adm2[raw_data1$adm2 == "DS Barsalogho"] <- "Barsalogho"
raw_data1$adm2[raw_data1$adm2 == "DS Baskuy"] <- "Baskuy"
raw_data1$adm2[raw_data1$adm2 == "DS Batie"] <- "Batie"
raw_data1$adm2[raw_data1$adm2 == "DS Bittou"] <- "Bittou"
raw_data1$adm2[raw_data1$adm2 == "DS Bogande"] <- "Bogande"
raw_data1$adm2[raw_data1$adm2 == "DS Bogodogo"] <- "Bogodogo"
raw_data1$adm2[raw_data1$adm2 == "DS Boromo"] <- "Boromo"
raw_data1$adm2[raw_data1$adm2 == "DS Boulmiougou"] <- "Boulmiougou"
raw_data1$adm2[raw_data1$adm2 == "DS Boulsa"] <- "Boulsa"
raw_data1$adm2[raw_data1$adm2 == "DS Bousse"] <- "Bousse"
raw_data1$adm2[raw_data1$adm2 == "DS Boussouma"] <- "Boussouma"
raw_data1$adm2[raw_data1$adm2 == "DS Dafra"] <- "Dafra"
raw_data1$adm2[raw_data1$adm2 == "DS Dande"] <- "Dande"
raw_data1$adm2[raw_data1$adm2 == "DS Dano"] <- "Dano"
raw_data1$adm2[raw_data1$adm2 == "DS Dedougou"] <- "Dedougou"
raw_data1$adm2[raw_data1$adm2 == "DS Diapaga"] <- "Diapaga"
raw_data1$adm2[raw_data1$adm2 == "DS Diebougou"] <- "Diebougou"
raw_data1$adm2[raw_data1$adm2 == "DS Djibo"] <- "Djibo"
raw_data1$adm2[raw_data1$adm2 == "DS Do"] <- "Do"
raw_data1$adm2[raw_data1$adm2 == "DS Dori"] <- "Dori"
raw_data1$adm2[raw_data1$adm2 == "DS Fada"] <- "Fada"
raw_data1$adm2[raw_data1$adm2 == "DS Gaoua"] <- "Gaoua"
raw_data1$adm2[raw_data1$adm2 == "DS Garango"] <- "Garango"
raw_data1$adm2[raw_data1$adm2 == "DS Gayeri"] <- "Gayeri"
raw_data1$adm2[raw_data1$adm2 == "DS Gorom-Gorom"] <- "Gorom"
raw_data1$adm2[raw_data1$adm2 == "DS Gourcy"] <- "Gourcy"
raw_data1$adm2[raw_data1$adm2 == "DS Hounde"] <- "Hounde"
raw_data1$adm2[raw_data1$adm2 == "DS Kampti"] <- "Kampti"
raw_data1$adm2[raw_data1$adm2 == "DS Karangasso Vigue"] <- "Karangasso Vigue"
raw_data1$adm2[raw_data1$adm2 == "DS Kaya"] <- "Kaya"
raw_data1$adm2[raw_data1$adm2 == "DS Kombissiri"] <- "Kombissiri"
raw_data1$adm2[raw_data1$adm2 == "DS Kongoussi"] <- "Kongoussi"
raw_data1$adm2[raw_data1$adm2 == "DS Koudougou"] <- "Koudougou"
raw_data1$adm2[raw_data1$adm2 == "DS Koupela"] <- "Koupela"
raw_data1$adm2[raw_data1$adm2 == "DS Lena"] <- "Lena"
raw_data1$adm2[raw_data1$adm2 == "DS Leo"] <- "Leo"
raw_data1$adm2[raw_data1$adm2 == "DS Manga"] <- "Manga"
raw_data1$adm2[raw_data1$adm2 == "DS Mangodara"] <- "Mangodara"
raw_data1$adm2[raw_data1$adm2 == "DS Manni"] <- "Manni"
raw_data1$adm2[raw_data1$adm2 == "DS N'Dorola"] <- "Ndorola"
raw_data1$adm2[raw_data1$adm2 == "DS Nanoro"] <- "Nanoro"
raw_data1$adm2[raw_data1$adm2 == "DS Nongr-Massom"] <- "Nongr-Massom"
raw_data1$adm2[raw_data1$adm2 == "DS Nouna"] <- "Nouna"
raw_data1$adm2[raw_data1$adm2 == "DS Orodara"] <- "Orodara"
raw_data1$adm2[raw_data1$adm2 == "DS Ouahigouya"] <- "Ouahigouya"
raw_data1$adm2[raw_data1$adm2 == "DS Ouargaye"] <- "Ouargaye"
raw_data1$adm2[raw_data1$adm2 == "DS Pama"] <- "Pama"
raw_data1$adm2[raw_data1$adm2 == "DS Po"] <- "Po"
raw_data1$adm2[raw_data1$adm2 == "DS Pouytenga"] <- "Pouytenga"
raw_data1$adm2[raw_data1$adm2 == "DS Reo"] <- "Reo"
raw_data1$adm2[raw_data1$adm2 == "DS Sabou"] <- "Sabou"
raw_data1$adm2[raw_data1$adm2 == "DS Sapone"] <- "Sapone"
raw_data1$adm2[raw_data1$adm2 == "DS Sapouy"] <- "Sapouy"
raw_data1$adm2[raw_data1$adm2 == "DS Sebba"] <- "Sebba"
raw_data1$adm2[raw_data1$adm2 == "DS Seguenega"] <- "Seguenega"
raw_data1$adm2[raw_data1$adm2 == "DS Sig-Noghin"] <- "Sig-noghin"
raw_data1$adm2[raw_data1$adm2 == "DS Sindou"] <- "Sindou"
raw_data1$adm2[raw_data1$adm2 == "DS Solenzo"] <- "Solenzo"
raw_data1$adm2[raw_data1$adm2 == "DS Tenado"] <- "Tenado"
raw_data1$adm2[raw_data1$adm2 == "DS Tenkodogo"] <- "Tenkodogo"
raw_data1$adm2[raw_data1$adm2 == "DS Thiou"] <- "Thiou"
raw_data1$adm2[raw_data1$adm2 == "DS Titao"] <- "Titao"
raw_data1$adm2[raw_data1$adm2 == "DS Toma"] <- "Toma"
raw_data1$adm2[raw_data1$adm2 == "DS Tougan"] <- "Tougan"
raw_data1$adm2[raw_data1$adm2 == "DS Tougouri"] <- "Tougouri"
raw_data1$adm2[raw_data1$adm2 == "DS Yako"] <- "Yako"
raw_data1$adm2[raw_data1$adm2 == "DS Zabre"] <- "Zabre"
raw_data1$adm2[raw_data1$adm2 == "DS Ziniare"] <- "Ziniare"
raw_data1$adm2[raw_data1$adm2 == "DS Zorgho"] <- "Zorgho"

raw_data1 = raw_data1 %>%mutate(UID = paste(adm1, adm2, hf))

ver=raw_data1 %>% select(adm1, adm2, hf, year, month, conf)%>%mutate(UID= paste(adm1, adm2, hf), Date=make_date(year=year, month = month, day=1))

ver[, 'conf'][ver[, 'conf']==0]<-NA


### Data from 2016 to 2022

BFA_routine_data_2016_2022 = read_excel('C:/Users/ood5226.FSM/Downloads/BFA_monthly_routine_data_2016_2022.xlsx')

HF_cases_active <- read.csv("C:/Users/ood5226.FSM/Downloads/HF_cases_active.csv")

liste_hf_2023 = raw_data1 %>% select(adm1, adm2, hf) %>% distinct() %>%
  mutate(UID = paste(adm1, adm2, hf))

liste_hf_2016_2022 = HF_cases_active %>% select(adm1, adm2, hf) %>%
  distinct() %>%
  mutate(UID = paste(adm1, adm2, hf))

# library(fuzzyjoin)
# matched_facilities <- stringdist_left_join(liste_hf_2016_2022, liste_hf_2023,
#                                            by = c("adm2", "hf", "UID"),
#                                            method = "jw",
#                                            distance_col = "similarity",
#                                            max_dist = 0.2)


### Matching previous health facility (2016 to 2022) and new datasets (2023)


matching <- function(column1, column2) {
  # Créer toutes les combinaisons possibles entre column1 et column2
  results <- expand.grid(Col1 = column1, Col2 = column2, stringsAsFactors = FALSE) %>%
    mutate(Match_Score = (1 - stringdist(Col1, Col2, method = "jw")) * 100)
  
  # Identification des correspondances exactes
  exact_matches <- results %>%
    filter(Col1 == Col2) %>%
    mutate(Match_Status = "Match", Match_Score = 100)
  
  # Filtrer les correspondances inexactes
  inexact_matches <- results %>%
    filter(!Col1 %in% column2) %>%  # Exclure les correspondances exactes
    group_by(Col1) %>%
    slice_max(order_by = Match_Score, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Match_Status = "Unmatch")
  
  # Assurer que chaque valeur de Col2 est unique en supprimant les doublons
  used_col2 <- c(exact_matches$Col2, inexact_matches$Col2)
  inexact_matches <- inexact_matches %>%
    filter(!Col2 %in% used_col2[duplicated(used_col2)])
  
  # Identifier les éléments de column2 qui ne sont pas appariés
  unmatched_col2 <- setdiff(column2, c(exact_matches$Col2, inexact_matches$Col2))
  unmatched_df <- tibble(Col1 = ifelse(unmatched_col2 %in% column1, unmatched_col2, NA),
                         Col2 = unmatched_col2,
                         Match_Score = 0, 
                         Match_Status = "Unmatch")
  
  # Fusionner les résultats et garantir qu'il n'y a pas de doublons
  final_results <- bind_rows(exact_matches, inexact_matches, unmatched_df) %>%
    distinct(Col1, Col2, .keep_all = TRUE)  # Garantit l’unicité des paires Col1-Col2
  
  return(final_results)
}

data_match = matching(liste_hf_2016_2022$UID, liste_hf_2023$UID)

liste_fusion = data_match %>% select(UID = Col1, new_name = Col2) %>% left_join(liste_hf_2023, by='UID')%>%select(adm1, adm2,UID,new_name)

raw_data_finale <- raw_data1 %>%
  left_join(liste_fusion, by = c("adm1", "adm2", "UID")) %>%
  select(-UID) %>%
  rename(UID = new_name)



##
# Calculer le nombre de 0 pour les cas confirmés par formation sanitaire
## 1. transform all column in numeric


vv[, c(7:11)] = apply(vv[,c(7:11)], 2, function(x) as.numeric(x))
##



## 2016-2022
HF_activity_2016_2022 = read.csv('C:/Users/ood5226.FSM/Downloads/HF_cases_active.csv') 

HF_activity = HF_cases_active %>% select(adm1, adm2, hf, month, year, Date, conf, HF_status) %>%
  mutate(UID= paste(adm1, adm2, hf))

HF_activity =HF_activity[order(HF_activity$UID, HF_activity$Date),]


# Define inactivity threshold
inactive_threshold <- 6

# Function to process and classify activity status
classify_activity_status <- function(df, conf_col = "conf") {
  df %>%
    group_by(UID) %>%
    complete(Date = {
      min_date <- min(Date, na.rm = TRUE)
      max_date <- max(Date, na.rm = TRUE)
      dates <- seq(min_date, max_date, by = "month")
      dates[!year(dates) %in% 2019]
    }, fill = setNames(list(NA), conf_col)) %>%
    filter(year(Date) != 2019) %>%
    arrange(Date, .by_group = TRUE) %>%
    mutate(
      temp_status = if_else(is.na(.data[[conf_col]]) | .data[[conf_col]] == 0, "inactive", "active"),
      active_status = zoo::na.locf(ifelse(temp_status == "active", temp_status, NA), 
                                   na.rm = FALSE, fromLast = FALSE, default = "inactive"),
      inactive_run = with(rle(active_status == "inactive"), rep(lengths * values, lengths)),
      HF_status = case_when(
        all(active_status == "inactive") ~ "inactive",
        inactive_run >= inactive_threshold ~ "inactive",
        TRUE ~ active_status
      )
    ) %>%
    ungroup() %>%
    select(-temp_status, -active_status, -inactive_run)
}

# Prep and process HF_activity (assumed 2016–2022 or broader)
HF_cases <- HF_activity %>%
  mutate(Date = make_date(year = year, month = month, day = 1)) %>%
  classify_activity_status(conf_col = "conf")

# Prep and process ver (2023 data)
HF_cases_2023 <- ver %>%
  #mutate(Date = as_date(as.character(Date), format = "%Y-%m-%d")) %>%
  classify_activity_status(conf_col = "conf")

# Optional: Combine into one dataset if desired
HF_cases_combined <- bind_rows(HF_cases, HF_cases_2023)



### Reporting rate

# HF averages per month (historical, unaffected by current status)
hf_avg_monthly <- HF_cases_combined %>%
  group_by(adm2, UID, month) %>%
  summarise(avg_cases = mean(conf, na.rm = TRUE), .groups = "drop") %>%
  mutate(avg_cases = replace(avg_cases, is.na(avg_cases), 0))

# District totals per month
district_avg_monthly <- hf_avg_monthly %>%
  group_by(adm2, month) %>%
  summarise(district_total_avg = sum(avg_cases, na.rm = TRUE), .groups = "drop")

# Weights (based on historical averages)
hf_weights <- hf_avg_monthly %>%
  left_join(district_avg_monthly, by = c("adm2", "month")) %>%
  mutate(weight = avg_cases / district_total_avg,
         weight = replace(weight, is.na(weight) | is.infinite(weight), 0))

# Join weights, ensure unique rows, and adjust averages for inactivity
hf_data_weighted <- HF_cases_combined %>%
  distinct(adm2, Date, UID, .keep_all = TRUE) %>%  # Remove duplicates
  left_join(hf_weights %>% select(UID, month, weight, avg_cases), by = c("UID", "month")) %>%
  mutate(avg_cases_adjusted = if_else(HF_status == "inactive", 0, avg_cases))

# Weighted reporting rate (raw sum of weights for active HFs)
district_reporting_rates <- hf_data_weighted %>%
  group_by(adm2, Date) %>%
  summarise(
    n_hfs = n(),
    n_active = sum(HF_status == "active"),
    total_weight_all = sum(weight, na.rm = TRUE),  # Debug: should ≈ 1
    weighted_rate = sum(weight[HF_status == "active"], na.rm = TRUE),  # Raw sum
    unweighted_rate = mean(!is.na(conf) & conf > 0 & HF_status == "active", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(unweighted_rate = replace(unweighted_rate, is.na(unweighted_rate), 0))

# Check results
print("Problematic Rates:")
district_reporting_rates %>%
  filter(n_active < n_hfs & weighted_rate >= 1) %>%
  print(n = 20)

print("Sample Output:")
district_reporting_rates %>%
  filter(adm2 == "Boromo") %>%  # Replace with a district
  print(n = 12)


data_reporting = data_reporting %>% mutate(Date = make_date(year = year, month = month, day = "1"))

p = ggplot(district_report, aes(x = Date, y = adm2, fill = weighted_rep_rate)) +
  geom_tile() +
  scale_x_yearmon("Date", breaks = sort(unique(district_report$Date))[c(seq(1,84,6), 84)],
                  labels = sort(unique(district_report$Date))[c(seq(1,84,6), 84)]) +
  scale_fill_viridis(option = 'D') +
  xlab("Year") +
  ylab("Health districts") +
  theme_classic()
# Customize theme
p = p + theme(axis.text.x = element_text(face = "bold", size = 7, angle = 45))
p = p + theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
  # Note: 'angle=45' is not a valid argument here; it's already set in axis.text.x
)


g = ggplot(district_report, aes(x = Date, y = adm2, fill = rep_rate)) +
  geom_tile() +
  scale_x_yearmon("Date", breaks = sort(unique(district_report$Date))[c(seq(1,84,6), 84)],
                  labels = sort(unique(district_report$Date))[c(seq(1,84,6), 84)]) +
  scale_fill_viridis(option = 'D') +
  xlab("Year") +
  ylab("Health districts") +
  theme_classic()
# Customize theme
g = g + theme(axis.text.x = element_text(face = "bold", size = 7, angle = 45))
g = g + theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
  # Note: 'angle=45' is not a valid argument here; it's already set in axis.text.x
)
g


### Detection of outliers and replace them

cols = c("adm1", "adm2", "hf", "month", "year", "susp_u5", "susp_514", 
         "susp_ov15", "testrdt_u5", "testrdt_514", "testrdt_ov15", "testmic_u5", 
         "testmic_514", "testmic_ov15", "confrdt_u5", "confrdt_514", "confrdt_ov15", 
         "confmic_u5", "confmic_514", "confmic_ov15", "maltreat_u5", "maltreat_514", 
         "maltreat_ov15", "pres_u5", "pres_514", "pres_ov15", "conf_u5", 
         "conf_514", "conf_ov15", "pressev_u5", "pressev_514", "pressev_ov15", 
         "pressev_preg", "confsev_u5", "confsev_514", "confsev_ov15", 
         "confsev_preg", "iptp3", "maldth_u5", "maldth_514", "maldth_ov15", 
         "maldth_preg", "susp_ov5", "susp", "testrdt_ov5", "testrdt", 
         "testmic_ov5", "testmic", "confrdt_ov5", "confrdt", "confmic_ov5", 
         "confmic", "conf", "test", "test_u5", "test_ov5", "maltreat_ov5", 
         "maltreat", "pres_ov5", "pres", "conf_ov5", "pressev_ov5", "pressev", 
         "confsev_ov5", "confsev", "maldth_ov5", "maldth", "UID")


bfa_data_monthly_hf_2016_2022 = HF_cases_active %>%
  select(any_of(cols))

cols_num = c( "month", "year", "susp_u5", "susp_514", 
              "susp_ov15", "testrdt_u5", "testrdt_514", "testrdt_ov15", "testmic_u5", 
              "testmic_514", "testmic_ov15", "confrdt_u5", "confrdt_514", "confrdt_ov15", 
              "confmic_u5", "confmic_514", "confmic_ov15", "maltreat_u5", "maltreat_514", 
              "maltreat_ov15", "pres_u5", "pres_514", "pres_ov15", "conf_u5", 
              "conf_514", "conf_ov15", "pressev_u5", "pressev_514", "pressev_ov15", 
              "pressev_preg", "confsev_u5", "confsev_514", "confsev_ov15", 
              "confsev_preg", "iptp3", "maldth_u5", "maldth_514", "maldth_ov15", 
              "maldth_preg", "susp_ov5", "susp", "testrdt_ov5", "testrdt", 
              "testmic_ov5", "testmic", "confrdt_ov5", "confrdt", "confmic_ov5", 
              "confmic", "conf", "test", "test_u5", "test_ov5", "maltreat_ov5", 
              "maltreat", "pres_ov5", "pres", "conf_ov5", "pressev_ov5", "pressev", 
              "confsev_ov5", "confsev", "maldth_ov5", "maldth")


raw_data_finale[, cols_num] = lapply(raw_data_finale[cols_num], as.numeric)

bfa_data_monthly_hf_2016_2023 = bfa_data_monthly_hf_2016_2022 %>% bind_rows(raw_data_finale)


df = bfa_data_monthly_hf_2016_2023 %>% replace(is.na(.), 0)

cols_for_all_outliers =c("adm1", "adm2", "hf", "month", "year", "susp_u5", "susp_514", 
         "susp_ov15", "testrdt_u5", "testrdt_514", "testrdt_ov15", "testmic_u5", 
         "testmic_514", "testmic_ov15", "confrdt_u5", "confrdt_514", "confrdt_ov15", 
         "confmic_u5", "confmic_514", "confmic_ov15", "maltreat_u5", "maltreat_514", 
         "maltreat_ov15", "pres_u5", "pres_514", "pres_ov15", "conf_u5", 
         "conf_514", "conf_ov15", "pressev_u5", "pressev_514", "pressev_ov15", 
         "pressev_preg", "confsev_u5", "confsev_514", "confsev_ov15", 
         "confsev_preg", "iptp3", "maldth_u5", "maldth_514", "maldth_ov15", 
         "maldth_preg", "UID")

cols_only_value_outliers =c("susp_u5", "susp_514", 
                 "susp_ov15", "testrdt_u5", "testrdt_514", "testrdt_ov15", "testmic_u5", 
                 "testmic_514", "testmic_ov15", "confrdt_u5", "confrdt_514", "confrdt_ov15", 
                 "confmic_u5", "confmic_514", "confmic_ov15", "maltreat_u5", "maltreat_514", 
                 "maltreat_ov15", "pres_u5", "pres_514", "pres_ov15", "conf_u5", 
                 "conf_514", "conf_ov15", "pressev_u5", "pressev_514", "pressev_ov15", 
                 "pressev_preg", "confsev_u5", "confsev_514", "confsev_ov15", 
                 "confsev_preg", "iptp3", "maldth_u5", "maldth_514", "maldth_ov15", 
                 "maldth_preg")

df_only_data_for_outliers = data_hf_routine %>% select(any_of(cols_for_all_outliers))

# 1.  Moyenne plus ou moins 3 ecart-types

data_long = df_only_data_for_outliers %>%
  pivot_longer(cols = susp_u5:maldth_preg, names_to = 'Variables', values_to = 'valeurs')


###############


data_long1 <- data_long1 %>%
  group_by(adm1, adm2, hf, UID, Variables, year) %>%
  summarise(
    moyenne = ceiling(mean(valeurs)),
    sd = ceiling(sd(valeurs)),
    mediane = ceiling(median(valeurs)),
    median_absolute = ceiling(mad(valeurs, constant = 1)),
    Q1 = quantile(valeurs, 0.25, na.rm = TRUE),
    Q3 = quantile(valeurs, 0.75, na.rm = TRUE),
    IQR = quantile(valeurs, 0.75, na.rm = TRUE) - quantile(valeurs, 0.25, na.rm = TRUE),
    lower_bound = moyenne - 3 * sd,
    upper_bound = moyenne + 3 * sd,
    BI_median = mediane - 15 * median_absolute,
    BS_median = mediane + 15 * median_absolute,
    lower_bound_iqr = quantile(valeurs, 0.25, na.rm = TRUE) - 1.5 * (quantile(valeurs, 0.75, na.rm = TRUE) - quantile(valeurs, 0.25, na.rm = TRUE)),
    upper_bound_iqr = quantile(valeurs, 0.75, na.rm = TRUE) + 1.5 * (quantile(valeurs, 0.75, na.rm = TRUE) - quantile(valeurs, 0.25, na.rm = TRUE))
  ) %>%
  ungroup()



data_long1 = data_long1 %>%
  left_join(data_long2, by=c('adm1','adm2','UID',"hf",'year', 'Variables')) %>%
  mutate(outliers_moyenne = ifelse(valeurs < lower_bound | valeurs > upper_bound, 'outliers', 'normales values'),
         outliers_halper = ifelse(valeurs < BI_median | valeurs > BS_median, 'outliers', 'normales values'),
         outliers_IQR = ifelse(valeurs < lower_bound_iqr | valeurs > upper_bound_iqr, 'outliers', 'normales values'))


detect_outliers <- function(df, column) {
  # Ensure the column exists in the dataframe
  if (!column %in% names(df)) {
    stop(paste("Column", column, "not found in dataframe"))
  }
  
  # Check if the column is numeric
  if (!is.numeric(df[[column]])) {
    stop(paste("Column", column, "is not numeric"))
  }
  
  # Group and calculate statistics
  df_stats <- df %>%
    group_by(adm1, adm2, hf, UID, year) %>%  # Removed Variables if it's not needed for grouping
    summarise(
      moyenne = ceiling(mean(.data[[column]], na.rm = TRUE)),
      sd = ceiling(sd(.data[[column]], na.rm = TRUE)),
      mediane = ceiling(median(.data[[column]], na.rm = TRUE)),
      median_absolute = ceiling(mad(.data[[column]], constant = 1, na.rm = TRUE)),
      Q1 = quantile(.data[[column]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[column]], 0.75, na.rm = TRUE),
      IQR = quantile(.data[[column]], 0.75, na.rm = TRUE) - quantile(.data[[column]], 0.25, na.rm = TRUE),
      lower_bound = moyenne - 3 * sd,
      upper_bound = moyenne + 3 * sd,
      BI_median = mediane - 15 * median_absolute,
      BS_median = mediane + 15 * median_absolute,
      lower_bound_iqr = quantile(.data[[column]], 0.25, na.rm = TRUE) - 1.5 * (quantile(.data[[column]], 0.75, na.rm = TRUE) - quantile(.data[[column]], 0.25, na.rm = TRUE)),
      upper_bound_iqr = quantile(.data[[column]], 0.75, na.rm = TRUE) + 1.5 * (quantile(.data[[column]], 0.75, na.rm = TRUE) - quantile(.data[[column]], 0.25, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Join stats back to original data and classify outliers
  df_result <- df %>%
    left_join(df_stats, by = c("adm1", "adm2", "hf", "UID", "year")) %>%
    mutate(
      outliers_moyenne = ifelse(.data[[column]] < lower_bound | .data[[column]] > upper_bound, "outliers", "normales values"),
      outliers_halper = ifelse(.data[[column]] < BI_median | .data[[column]] > BS_median, "outliers", "normales values"),
      outliers_IQR = ifelse(.data[[column]] < lower_bound_iqr | .data[[column]] > upper_bound_iqr, "outliers", "normales values")
    )
  
  return(df_result)
}


### Remplacer les valeurs manquantes (impuTS)

ind <- which(data_long1$outliers_halper == "outliers")

data_long1$valeurs_imputees = rep(NA, length(nrow(data_long1)))

# Assuming data_long1 is your dataframe and ind is the index of rows to impute

# Function to find the nearest non-zero, non-NA values before and after an index
find_valid_neighbors <- function(i, valeurs) {
  # Look backward for the last valid value (not 0 or NA)
  last_valid <- NA
  for (j in (i - 1):1) {
    if (!is.na(valeurs[j]) && valeurs[j] != 0) {
      last_valid <- valeurs[j]
      break
    }
  }
  
  # Look forward for the next valid value (not 0 or NA)
  next_valid <- NA
  for (j in (i + 1):length(valeurs)) {
    if (!is.na(valeurs[j]) && valeurs[j] != 0) {
      next_valid <- valeurs[j]
      break
    }
  }
  
  # Return the mean of valid neighbors, or NA if none found
  if (is.na(last_valid) || is.na(next_valid)) {
    return(NA)
  } else {
    return(mean(c(last_valid, next_valid)))
  }
}

# Apply imputation using the valid neighbors
data_long1$valeurs_imputees[ind] <- sapply(ind, function(i) find_valid_neighbors(i, data_long1$valeurs))

#data_long1$valeurs_imputees[ind] <- sapply(ind, function(i) with(data_long1, mean(c(valeurs[i-1], valeurs[i+1]))))

data_long3 = data_long1 %>%
  mutate(valeurs_imputees = ceiling(ifelse(is.na(valeurs_imputees), valeurs, valeurs_imputees)))


data_long3 = data_long3 %>%
  pivot_wider(names_from = Variables, values_from = valeurs_imputees)


data_large = data_large %>%
  rowwise() %>%
  mutate(susp_ov5 = sum(susp_514, susp_ov15, na.rm = T), susp = sum(susp_u5, susp_ov5, na.rm = T),
         testrdt_ov5 = sum(testrdt_514, testrdt_ov15, na.rm = T), testrdt = sum(testrdt_u5, testrdt_ov5, na.rm = T),
         testmic_ov5 = sum(testmic_514, testmic_ov15, na.rm = T), testmic = sum(testmic_u5, testmic_ov5, na.rm = T),
         confrdt_ov5 = sum(confrdt_514, confrdt_ov15, na.rm = T), confrdt = sum(confrdt_u5, confrdt_ov5, na.rm = T),
         confmic_ov5 = sum(confmic_514, confmic_ov15, na.rm = T), confmic = sum(confmic_u5, confmic_ov5, na.rm = T),
         conf = sum(confrdt, confmic, na.rm = T), test = sum(testrdt, testmic, na.rm = T), test_u5 = sum(testrdt_u5, testmic_u5, na.rm = T),
         test_ov5 = sum(testrdt_ov5, testmic_ov5, na.rm = T),
         maltreat_ov5 = sum(maltreat_514, maltreat_ov15, na.rm = T), maltreat = sum(maltreat_u5, maltreat_ov5, na.rm = T),
         pres_ov5 = sum(pres_514, pres_ov15, na.rm = T), pres = sum(pres_u5, pres_ov5, na.rm = T), conf_ov5 = sum(conf_514, conf_ov15, na.rm = T),
         pressev_ov5 = sum(pressev_514, pressev_ov15,pressev_preg, na.rm = T), pressev = sum(pressev_u5, pressev_ov5, na.rm = T),
         confsev_ov5 = sum(confsev_514, confsev_ov15, confsev_preg, na.rm = T), confsev =  sum(confsev_u5, confsev_ov5, na.rm = T),
         maldth_ov5 = sum(maldth_514, maldth_ov15, maldth_preg, na.rm = T), maldth = sum(maldth_u5, maldth_ov5, na.rm = T))




### Outliers plots
x = c('Angola', 'Benin', 'Burkina','Burundi','Cameroon', 'Gambia',
      'Ghana', 'Guinea', 'Kenya', 'Liberia', 'Madagascar', 'Malawi',
      'Mali', 'Mozambique', 'Nigeria', 'Rwanda', 
      'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')

#gg + annotate("text",x=-Inf,y=-Inf,hjust=0,vjust=0.5,label="From the regression model, the effect of year was 0.001 with a p-value 0.00135")
l = data_long1 %>% distinct(Variables)
#gg + geom_text(aes(label = distinct(paste0('From the regression model, the effect of year was 0.001 with a p-value 0.00135'))),  vjust = 0, hjust = 0)
lapply(l$Variables[2:length(l$Variables)], function(x) {
  t = data_long1[data_long1$Variables==x, ]
  i <- which(l$Variables==x)
  s <- paste0("Outliers/",i,"-",x)
  gg = t %>% 
    ggplot(aes(x = date, y = valeurs, color = as.factor(outliers_halper)))+
    geom_point() +
    facet_wrap(~type)+
    theme_classic() +
    theme(legend.position="bottom") +
    xlab('') +
    ylab('')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  pdf(paste0(s,"-trend.pdf"))
  print(gg)
  dev.off()
  res <- lm(pfpr_rur~year_survey2, data = t)
  res2 <- lm(test_result~year_survey2, data = t)
  sink("Trend/sink.txt",append=TRUE)
  print(list(x,summary(res$glm),summary(res),summary(res2)))
  sink()
})




ggplot(aes(x = date, y = valeurs, color = as.factor(outliers_halper)))+
  geom_point() +
  facet_wrap(~type)+
  theme_classic() +
  theme(legend.position="bottom") +
  xlab('') +
  ylab('')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Prep data
HF_cases <- HF_activity %>% mutate(Date = make_date(year=year, month = month, day=1))
HF_cases <- HF_cases %>%
  distinct(UID, Date, .keep_all = T) %>%
  arrange(UID, Date)

# Define inactivity threshold
inactive_threshold <- 6

# Classify activity status


classify_activity_status <- function(df, conf_col = "conf") {
  df %>%
    # Initial filter to exclude 2019
    filter(year(Date) != 2019) %>%
    group_by(UID) %>%
    # Complete sequence, explicitly avoiding 2019
    complete(Date = {
      min_date <- min(Date, na.rm = TRUE)
      max_date <- max(Date, na.rm = TRUE)
      # Generate sequence excluding 2019
      dates <- seq(min_date, max_date, by = "month")
      dates[!year(dates) %in% 2019]  # Exclude 2019 explicitly
    }, fill = setNames(list(NA), conf_col)) %>%
    # Second filter to catch any stragglers
    filter(year(Date) != 2019) %>%
    arrange(Date, .by_group = TRUE) %>%
    mutate(
      # Step 1: Initial status based on conf
      temp_status = if_else(is.na(.data[[conf_col]]) | .data[[conf_col]] == 0, "inactive", "active"),
      # Step 2: Carry "inactive" forward until first "active"
      active_status = zoo::na.locf(ifelse(temp_status == "active", temp_status, NA), 
                                   na.rm = FALSE, fromLast = FALSE, default = "inactive"),
      # Step 3: Recompute runs to catch post-reporting gaps
      final_run_lengths = rep(rle(temp_status)$lengths, rle(temp_status)$lengths),
      final_run_values = rep(rle(temp_status)$values, rle(temp_status)$lengths),
      # Step 4: Final status
      HF_status = case_when(
        all(temp_status == "inactive") ~ "inactive",              # Never reported
        final_run_values == "inactive" & final_run_lengths >= inactive_threshold ~ "inactive",  # 6+ month gaps
        TRUE ~ active_status                                      # Otherwise keep status
      )
    ) %>%
    ungroup() %>%
    select(-temp_status, -active_status, -final_run_lengths, -final_run_values)
}

# Prep and process HF_activity, excluding 2019
HF_cases <- HF_activity %>%
  mutate(Date = make_date(year = year, month = month, day = 1)) %>%
  classify_activity_status(conf_col = "conf")

# Prep and process ver (2023 data), excluding 2019
HF_cases_2023 <- ver %>%
  mutate(Date = as_date(as.character(Date), format = "%Y-%m-%d")) %>%
  classify_activity_status(conf_col = "conf")

# Optional: Combine datasets
HF_cases_all <- bind_rows(HF_cases1, HF_cases_2023)



# Define a function to classify reporting status based on max months
classify_reporting <- function(n_zeros, max_months) {
  case_when(
    n_zeros >= max_months ~ "0",                   # No reports all year
    n_zeros > 0 & n_zeros < max_months ~ "at least one report",
    n_zeros == 0 ~ "100%",                         # Reported every month
    TRUE ~ ""                                      # Fallback (shouldn’t happen)
  )
}

# Single pipeline to process all years
bfa_hf_reporting_summary <- HF_cases_all %>%
  group_by(UID, year) %>%
  summarise(Nombre_de_zeros = sum(is.na(conf), na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Nombre_de_zeros)) %>%
  mutate(
    max_months = case_when(
      year == 2023 ~ 12,                           # 1 year = 12 months
      year %in% 2016:2022 ~ 72,                    # 7 years = 72 months (assuming monthly data)
      TRUE ~ 12                                    # Default (adjust if needed)
    ),
    reporting = classify_reporting(Nombre_de_zeros, max_months)
  ) %>%
  group_by(year, reporting) %>%
  summarise(total = n(), .groups = "drop") %>%
  filter(reporting != "" | is.na(reporting) | reporting !='0')  # Remove empty categories



### Fig2. Number of health facilities

Fig2 = bfa_hf_reporting_summary %>%
  filter(reporting != "0") %>%
  mutate(reporting = trimws(tolower(reporting))) %>%  # Standardize: remove spaces, lowercase
  ggplot(aes(x = year, y = total, color = reporting)) +
  geom_line(size = 1, alpha = 0.8) +              # Thicker, slightly transparent lines
  geom_point(size = 3, shape = 16) +              # Larger, solid points
  scale_x_continuous(breaks = seq(2016, 2023, 1)) + # Explicit year breaks
  scale_color_manual(
    values = c("100%" = "#1b9e77", "at least one report" = "#d95f02"),
    labels = c("Full Reporting (100%)", "At Least One Report")
  ) +
  labs(
    title = "Health Facilities Reporting Trends (2016–2023)",
    subtitle = "Excluding Facilities with No Reports",
    x = "Year",
    y = "Number of Health Facilities",
    color = "Reporting Status"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

### Reporting rate
# Calculate the total number of facilities and active facilities per district and month
tmp_total_HF <- HF_cases_2023 %>%
  group_by(adm2, year, month) %>%
  summarise(
    total_HF = n(),
    total_HFs_active = sum(HF_status == "active", na.rm = TRUE),  # Ensure NA values are ignored
    .groups = 'drop'
  )

data1[, 'conf'][data1[, 'conf'] == 0] <- NA
# Calculate average counts per health facility per month
average_HF_counts_per_month <- HF_cases_2023 %>%
  group_by(adm2, hf, month) %>%
  summarise(
    na_counts_HF = sum(is.na(conf)),
    average_counts_HF = sum(conf, na.rm = TRUE) / (1 - sum(is.na(conf))),
    .groups = 'drop'
  ) %>%
  mutate(
    average_counts_HF = replace_na(average_counts_HF, 0)
  )


# Join the data
average_HF_counts_per_date <- average_HF_counts_per_month %>%
  left_join(tmp_total_HF, by = c("adm2", "month")) %>%
  left_join(HF_cases_active_2023 %>% select(hf, month, HF_status), by = c("hf", "month")) %>%
  mutate(
    average_counts_HF = if_else(is.na(HF_status), 0, average_counts_HF)
  )

# Calculate the sum of averages per district per date
average_District_counts_per_month <- average_HF_counts_per_date %>%
  group_by(adm2, month) %>%
  summarise(
    sum_avg_counts_HF = sum(average_counts_HF),
    .groups = 'drop'
  )

# Join and calculate weights
average_counts_per_date <- average_HF_counts_per_date %>%
  left_join(average_District_counts_per_month, by = c("adm2", "month")) %>%
  mutate(
    weights = average_counts_HF / sum_avg_counts_HF
  )


# Calculate the weighted total per district per date
tmp_total_HF_weighted <- average_counts_per_date %>%
  group_by(adm2, month) %>%
  summarise(
    total_HF_weighted = sum(weights),
    .groups = 'drop'
  )

# Prepare the data
data1 <- data1 %>%
  mutate(month_year = paste(year, month, sep = "-"))  # Create a month-year variable

# Calculate the average number of malaria cases reported per facility by month across all years
facility_avg_cases <- data1 %>%
  group_by(hf, month) %>%
  summarize(avg_cases = mean(conf, na.rm = TRUE), .groups = "drop")

# Calculate the total sum of average cases for each month across all facilities in the district
district_avg_cases <- data1 %>%
  group_by(adm2, month) %>%
  summarize(district_sum = sum(conf, na.rm = TRUE), .groups = "drop")

# Merge the averages back into the original data
data_with_avg <- data1 %>%
  left_join(facility_avg_cases, by = c("hf", "month")) %>%
  left_join(district_avg_cases, by = c("adm2", "month"))

# Calculate the weight for each facility (avg_cases / district_sum) for each month
data_with_avg <- data_with_avg %>%
  mutate(weight = ifelse(district_sum == 0, 0, avg_cases / district_sum))

# Calculate the weighted reporting rate for each district by month (sum of weights of active facilities)
district_weighted_rate <- data_with_avg %>%
  filter(active_status == "Active") %>%  # Only include active health facilities
  group_by(adm2, month) %>%
  summarize(weighted_reporting_rate = sum(weight, na.rm = TRUE), .groups = "drop")

# Calculate the unweighted reporting rate (sum of reported cases) for each district by month
district_unweighted_rate <- data %>%
  filter(active_status == "Active") %>%  # Only include active health facilities
  group_by(adm2, month) %>%
  summarize(unweighted_reporting_rate = sum(conf, na.rm = TRUE), .groups = "drop")

# Merge the weighted and unweighted reporting rates
reporting_rate_comparison <- district_weighted_rate %>%
  left_join(district_unweighted_rate, by = c("adm2", "month"))

# Display the results
print(reporting_rate_comparison)

# Optionally, you can plot the comparison for each district
library(ggplot2)
ggplot(reporting_rate_comparison, aes(x = month, y = weighted_reporting_rate, color = "Weighted")) +
  geom_line() +
  geom_line(aes(y = unweighted_reporting_rate, color = "Unweighted")) +
  facet_wrap(~adm2) +
  labs(title = "Comparison of Weighted vs Unweighted Reporting Rates by District",
       x = "Month", y = "Reporting Rate") +
  scale_color_manual(name = "Reporting Type", values = c("Weighted" = "blue", "Unweighted" = "red"))

### Aggregate at DS and monthly level
monthly_data_DS = raw_data %>%
  group_by(adm1, adm2, year, month) %>%
  dplyr::summarise(across(susp_u5:maldth, ~ sum(.x, na.rm = TRUE)))

## data for 2016-2022
data_2016_2022 = read.csv('/Users/ousmanediallo/Library/CloudStorage/OneDrive-Personnel/BFA_SNT/Retros_data/Data_excel/Data_2022/BFA_data_2016_2022_for_analysis.csv')
  
  
  
