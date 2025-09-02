### Stock outs data cleaning

## Create a function to split the column 

# month = c('Janvier', "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
# 
# 
# # Function to split strings
# split_month <- function(x){
#   unlist(strsplit(x,month))
# }


data_stock_out = nombre_de_jours_de_ruptures_intrants_palu_2023 %>% dplyr::select(adm1 = orgunitlevel2, adm3 = orgunitlevel3, adm2 = orgunitlevel4, hf = organisationunitname,
                                              period = periodname,llins_stockout_days = `Intrant-MILDA routine Nombre de jours de rupture`,
                                              AL_plaq6_stockout_days = `Intrant-Artemether + Lumefantrine 20mg/120mg pl/6 cp Nombre de jours de rupture`,
                                              AL_plaq12_stockout_days = `Intrant-Artemether + Lumefantrine 20mg/120mg pl/12 cp Nombre de jours de rupture`,
                                              AL_plaq18_stockout_days = `Intrant-Artemether + Lumefantrine 20mg/120mg pl/18 cp Nombre de jours de rupture`,
                                              AL_plaq24_stockout_days = `Intrant-Artemether + Lumefantrine 20mg/120mg pl/24cp Nombre de jours de rupture`,
                                              artinj_stockout_days = `Intrant-Artesunate injectable 60mg Nombre de jours de rupture`,
                                              rdt_stockout_days = `Intrant-TDR paludisme Nombre de jours de rupture`,
                                              iptp_stockout_days = `Intrant-Sulfadoxine Pyrimethamine (500/25) mg pour TPIg Nombre de jours de rupture`)%>%
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


data_stock_out_DS_2022_2023 = data_stock_out_DS_2022_2023 %>% mutate(llins_stockout_days = ifelse(llins_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
  llins_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(llins_stockout_days > 29 & month == 2, 28, llins_stockout_days))),
  AL_plaq6_stockout_days = ifelse(AL_plaq6_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
    AL_plaq6_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(AL_plaq6_stockout_days> 29 & month == 2, 28, AL_plaq6_stockout_days))),
  AL_plaq12_stockout_days = ifelse(AL_plaq12_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
    AL_plaq12_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(AL_plaq12_stockout_days > 29 & month == 2, 28, AL_plaq12_stockout_days))),
  AL_plaq18_stockout_days = ifelse(AL_plaq18_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
    AL_plaq18_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(AL_plaq18_stockout_days > 29 & month == 2, 28, AL_plaq18_stockout_days))),
  AL_plaq24_stockout_days = ifelse(AL_plaq24_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
    AL_plaq24_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(AL_plaq24_stockout_days > 29 & month == 2, 28, AL_plaq24_stockout_days))),
  rdt_stockout_days = ifelse(rdt_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
    rdt_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(rdt_stockout_days > 29 & month == 2, 28, rdt_stockout_days))),
  artinj_stockout_days = ifelse(artinj_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
    artinj_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(artinj_stockout_days > 29 & month == 2, 28, artinj_stockout_days))),
  iptp_stockout_days = ifelse(iptp_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
    iptp_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(iptp_stockout_days > 29 & month == 2, 28, iptp_stockout_days))))


## Recoding data_stock_out_2022$adm2
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHR Banfora"] <- "Banfora"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHR Dédougou"] <- "Dedougou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHR Dori"] <- "Dori"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHR Fada N'Gourma"] <- "Fada"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHR Gaoua"] <- "Gaoua"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHR Kaya"] <- "Kaya"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHR Koudougou"] <- "Koudougou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHR Tenkodogo"] <- "Tenkodogo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHR Ziniaré"] <- "Ziniare"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHU Bogodogo"] <- "Bogodogo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Banfora"] <- "Banfora"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Barsalogho"] <- "Barsalogho"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Baskuy"] <- "Baskuy"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Batié"] <- "Batie"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Bittou"] <- "Bittou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Bogande"] <- "Bogande"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Bogodogo"] <- "Bogodogo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Boromo"] <- "Boromo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Boulmiougou"] <- "Boulmiougou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Boulsa"] <- "Boulsa"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Boussé"] <- "Bousse"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Boussouma"] <- "Boussouma"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Dafra"] <- "Dafra"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Dande"] <- "Dande"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Dano"] <- "Dano"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Dedougou"] <- "Dedougou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Diapaga"] <- "Diapaga"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Diébougou"] <- "Diebougou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Djibo"] <- "Djibo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Do"] <- "Do"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Dori"] <- "Dori"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Fada"] <- "Fada"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Gaoua"] <- "Gaoua"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Garango"] <- "Garango"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Gayeri"] <- "Gayeri"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Gorom-Gorom"] <- "Gorom-Gorom"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Gourcy"] <- "Gourcy"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Hounde"] <- "Hounde"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Kampti"] <- "Kampti"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Karangasso Vigue"] <- "KarangassoVigue"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Kaya"] <- "Kaya"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Kombissiri"] <- "Kombissiri"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Kongoussi"] <- "Kongoussi"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Koudougou"] <- "Koudougou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Koupéla"] <- "Koupela"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Léna"] <- "Lena"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Léo"] <- "Leo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Manga"] <- "Manga"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Mangodara"] <- "Mangodara"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Manni"] <- "Manni"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS N'Dorola"] <- "N'Dorola"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Nanoro"] <- "Nanoro"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Nongr-Massom"] <- "Nongr-Massom"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Nouna"] <- "Nouna"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Orodara"] <- "Orodara"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Ouahigouya"] <- "Ouahigouya"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Ouargaye"] <- "Ouargaye"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Pama"] <- "Pama"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Po"] <- "Po"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Pouytenga"] <- "Pouytenga"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Réo"] <- "Reo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Sabou"] <- "Sabou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Sapone"] <- "Sapone"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Sapouy"] <- "Sapouy"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Sebba"] <- "Sebba"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Séguénéga"] <- "Seguenega"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Sig-Noghin"] <- "Sig-Noghin"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Sindou"] <- "Sindou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Solenzo"] <- "Solenzo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Tenado"] <- "Tenado"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Tenkodogo"] <- "Tenkodogo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Thiou"] <- "Thiou"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Titao"] <- "Titao"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Toma"] <- "Toma"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Tougan"] <- "Tougan"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Tougouri"] <- "Tougouri"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Yako"] <- "Yako"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Zabré"] <- "Zabre"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Ziniaré"] <- "Ziniare"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "DS Zorgho"] <- "Zorgho"
## Recoding data_stock_out_2022$adm2
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHU Pédiatrique CDG"] <- "Bogodogo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHU Sanou Souro"] <- "Do"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHU Tengandogo"] <- "Tengandogo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHU Yalgado"] <- "Basky"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CHUR Ouahigouya"] <- "Ouahigouya"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "Hôpital Paul VI"] <- "Sig-Noghin"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "Hôpital Saint Camille de Ouagadougou"] <- "Bogodogo"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CNLAT"] <- "Baskuy"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CNAOB"] <- "Baskuy"
data_stock_out_2022$adm2[data_stock_out_2022$adm2 == "CNRMPR"] <- "Boulmiougou"

data_stock_out_DS_2022 <- data_stock_out_2022 %>%
  # filter(!is.na(adm2)) %>%  # Uncomment if you need to exclude rows with missing adm2
  group_by(adm2, month, year) %>%
  summarise(across(llins_stockout_days:iptp_stockout_days, ~ mean(as.numeric(.x), na.rm = TRUE)),
           .groups = "drop")


data_stock_out_DS_2022 = data_stock_out_DS_2022 %>%
  mutate(across(llins_stockout_days:iptp_stockout_days, ~ ceiling(.x)))


data_stock_out_DS_2022 <- data_stock_out_DS_2022 %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.nan(.x), 0)))




BFA_donnees_gestion_stock_2016_2021 = BFA_donnees_gestion_stock_2016_2021_update %>%
  dplyr::select(adm1, adm2, month, year, llins_stockout_days,   AL_plaq6_stockout_days = AL_plaq6_sotckout_days, AL_plaq12_stockout_days = AL_plaq12_sotckout_days,
         AL_plaq18_stockout_days = AL_plaq18_sotckout_days,  AL_plaq24_stockout_days = AL_plaq24_sotckout_days, artinj_stockout_days, 
         rdt_stockout_days, iptp_stockout_days) %>%
  mutate(llins_stockout_days = ifelse(llins_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
    llins_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(llins_stockout_days > 29 & month == 2, 29, llins_stockout_days))),
    AL_plaq6_stockout_days = ifelse(AL_plaq6_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
      AL_plaq6_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(AL_plaq6_stockout_days> 29 & month == 2, 29, AL_plaq6_stockout_days))),
    AL_plaq12_stockout_days = ifelse(AL_plaq12_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
      AL_plaq12_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(AL_plaq12_stockout_days > 29 & month == 2, 29, AL_plaq12_stockout_days))),
    AL_plaq18_stockout_days = ifelse(AL_plaq18_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
      AL_plaq18_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(AL_plaq18_stockout_days > 29 & month == 2, 29, AL_plaq18_stockout_days))),
    AL_plaq24_stockout_days = ifelse(AL_plaq24_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
      AL_plaq24_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(AL_plaq24_stockout_days > 29 & month == 2, 29, AL_plaq24_stockout_days))),
    rdt_stockout_days = ifelse(rdt_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
      rdt_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(rdt_stockout_days > 29 & month == 2, 29, rdt_stockout_days))),
    artinj_stockout_days = ifelse(artinj_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
      artinj_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(artinj_stockout_days > 29 & month == 2, 29, artinj_stockout_days))),
    iptp_stockout_days = ifelse(iptp_stockout_days >31 & month %in% c(1,3,5,7,8,10,12), 31, ifelse(
      iptp_stockout_days >30 & month %in% c(4,6,9,11), 30, ifelse(iptp_stockout_days > 29 & month == 2, 29, iptp_stockout_days))))


data_stock_out_2016_2021 = BFA_donnees_gestion_stock_2016_2021%>%
  mutate(llins_stockout_days = ifelse(llins_stockout_days == 29 & month == 2 & year %in% c(2017, 2018, 2021), 28, llins_stockout_days),
         AL_plaq6_stockout_days = ifelse(AL_plaq6_stockout_days == 29 & month == 2 & year %in% c(2017, 2018, 2021), 28, AL_plaq6_stockout_days),
         AL_plaq12_stockout_days = ifelse(AL_plaq12_stockout_days == 29 & month == 2 & year %in% c(2017, 2018, 2021), 28, AL_plaq12_stockout_days),
         AL_plaq18_stockout_days = ifelse(AL_plaq18_stockout_days == 29 & month == 2 & year %in% c(2017, 2018, 2021), 28, AL_plaq18_stockout_days),
         AL_plaq24_stockout_days = ifelse(AL_plaq24_stockout_days == 29 & month == 2 & year %in% c(2017, 2018, 2021), 28, AL_plaq24_stockout_days),
         rdt_stockout_days = ifelse(rdt_stockout_days == 29 & month == 2 & year %in% c(2017, 2018, 2021), 28, rdt_stockout_days),
         artinj_stockout_days = ifelse(artinj_stockout_days == 29 & month == 2 & year %in% c(2017, 2018, 2021), 28, artinj_stockout_days),
         iptp_stockout_days = ifelse(iptp_stockout_days == 29 & month == 2 & year %in% c(2017, 2018, 2021), 28, iptp_stockout_days))



### 
# cols = c( "llins_stockout_days", 
#          "AL_plaq6_stockout_days", "AL_plaq12_stockout_days", "AL_plaq18_stockout_days", 
#          "AL_plaq24_stockout_days", "artinj_stockout_days", "rdt_stockout_days", 
#          "iptp_stockout_days")


data_stock_out_2016_2021 <- data_stock_out_2016_2021 %>%
  # filter(!is.na(adm2)) %>%  # Uncomment if you need to exclude rows with missing adm2
  group_by(adm2, month, year) %>%
  summarise(across(llins_stockout_days:iptp_stockout_days, ~ mean(as.numeric(.x), na.rm = TRUE)),
            .groups = "drop")

### Ceilling the number of days
data_stock_out_2016_2021 = data_stock_out_2016_2021 %>%
  mutate(across(llins_stockout_days:iptp_stockout_days, ~ ceiling(.x)))


data_stock_out_2016_2021 <- data_stock_out_2016_2021 %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.nan(.x), 0)))



data_stock_out_DS_2016_2021_without_2019 = data_stock_out_2016_2021 %>%
  filter(year!= 2019)
  



### rbind

data_stock_out_DS_2016_2023 = data_stock_out_2016_2021 %>% bind_rows(data_stock_out_DS_2022_2023) %>%
  mutate(adm2 = tolower(adm2))

# Regroup all for class of the ACT drugs and group them by year
data_stock_out_DS_2016_2023_for_analysis$mean_ACT = ceiling(apply(data_stock_out_DS_2016_2023_for_analysis[, c(5:9)], 1, mean))

data_stock_out_DS_year_2016_2023 = data_stock_out_DS_2016_2022 %>%
  group_by(adm2, year) %>%
  summarise(across(llins_stockout_days:mean_ACT, ~sum(.x, na.rm = T)))

## Recoding data_stock_out_DS_year_2016_2022$adm2
data_stock_out_DS_year_2016_2022$adm2[data_stock_out_DS_year_2016_2022$adm2 == "bittou"] <- "bitou"
data_stock_out_DS_year_2016_2022$adm2[data_stock_out_DS_year_2016_2022$adm2 == "gorom-gorom"] <- "gorom"
data_stock_out_DS_year_2016_2022$adm2[data_stock_out_DS_year_2016_2022$adm2 == "karangassovigue"] <- "karangasso vigue"
data_stock_out_DS_year_2016_2022$adm2[data_stock_out_DS_year_2016_2022$adm2 == "n'dorola"] <- "ndorola"

### Cleaning 2020 all outpatients database
CHU_CHR_2 = CHU_CHR %>%
  rowwise() %>%
  mutate(Janvier = sum(`Janvier 2020 Ambulatoire < 1 ans`, `Janvier 2020 Ambulatoire 1-4 ans`, `Janvier 2020 Ambulatoire 5-14 ans`, `Janvier 2020 Ambulatoire Adultes Masculins`,
                       `Janvier 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Fevrier = sum(`Février 2020 Ambulatoire < 1 ans`, `Février 2020 Ambulatoire 1-4 ans`, `Février 2020 Ambulatoire 5-14 ans`, `Février 2020 Ambulatoire Adultes Masculins`,
                       `Février 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Mars = sum(`Mars 2020 Ambulatoire < 1 ans`, `Mars 2020 Ambulatoire 1-4 ans`, `Mars 2020 Ambulatoire 5-14 ans`, `Mars 2020 Ambulatoire Adultes Masculins`,
                    `Mars 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Avril = sum(`Avril 2020 Ambulatoire < 1 ans`, `Avril 2020 Ambulatoire 1-4 ans`, `Avril 2020 Ambulatoire 5-14 ans`, `Avril 2020 Ambulatoire Adultes Masculins`,
                     `Avril 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Mai = sum(`Mai 2020 Ambulatoire < 1 ans`, `Mai 2020 Ambulatoire 1-4 ans`, `Mai 2020 Ambulatoire 5-14 ans`, `Mai 2020 Ambulatoire Adultes Masculins`,
                   `Mai 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Juin = sum(`Juin 2020 Ambulatoire < 1 ans`, `Juin 2020 Ambulatoire 1-4 ans`, `Juin 2020 Ambulatoire 5-14 ans`, `Juin 2020 Ambulatoire Adultes Masculins`,
                    `Juin 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Juillet = sum(`Juillet 2020 Ambulatoire < 1 ans`, `Juillet 2020 Ambulatoire 1-4 ans`, `Juillet 2020 Ambulatoire 5-14 ans`, `Juillet 2020 Ambulatoire Adultes Masculins`,
                       `Juillet 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Aout = sum(`Août 2020 Ambulatoire < 1 ans`, `Août 2020 Ambulatoire 1-4 ans`, `Août 2020 Ambulatoire 5-14 ans`, `Août 2020 Ambulatoire Adultes Masculins`,
                    `Août 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Septembre = sum(`Septembre 2020 Ambulatoire < 1 ans`, `Septembre 2020 Ambulatoire 1-4 ans`, `Septembre 2020 Ambulatoire 5-14 ans`, `Septembre 2020 Ambulatoire Adultes Masculins`,
                         `Septembre 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Octobre = sum(`Octobre 2020 Ambulatoire < 1 ans`, `Octobre 2020 Ambulatoire 1-4 ans`, `Octobre 2020 Ambulatoire 5-14 ans`, `Octobre 2020 Ambulatoire Adultes Masculins`,
                       `Octobre 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Novembre = sum(`Novembre 2020 Ambulatoire < 1 ans`, `Novembre 2020 Ambulatoire 1-4 ans`, `Novembre 2020 Ambulatoire 5-14 ans`, `Novembre 2020 Ambulatoire Adultes Masculins`,
                        `Novembre 2020 Ambulatoire Adultes Feminins`, na.rm = T),
         Decembre =sum(`Décembre 2020 Ambulatoire < 1 ans`, `Décembre 2020 Ambulatoire 1-4 ans`, `Décembre 2020 Ambulatoire 5-14 ans`, `Décembre 2020 Ambulatoire Adultes Masculins`,
         `Décembre 2020 Ambulatoire Adultes Feminins`, na.rm = T))%>%
  select(adm2 = organisationunitname, Janvier, Fevrier, Mars, Avril, Mai, Juin, Juillet, Aout, Septembre, Octobre, Novembre, Decembre) %>%
  gather(month, allout, Janvier:Decembre)%>%
  mutate(month = case_when(month == "Janvier" ~ 1,
                           month == "Fevrier" ~ 2,
                           month == "Mars" ~ 3,
                           month == "Avril" ~ 4,
                           month == "Mai" ~ 5,
                           month == "Juin" ~ 6,
                           month == "Juillet" ~ 7,
                           month == "Aout" ~ 8,
                           month == "Septembre" ~ 9,
                           month == "Octobre" ~ 10,
                           month == "Novembre" ~ 11,
                           month == "Decembre" ~ 12),
         year = rep(2020, 204)) %>% 
  mutate(month = as.numeric(month), year = as.numeric(year)) %>%
  select(adm2, month, year, allout)
  

tt_consultations_2020_HF_CHU = tt_consultations_2020_HF %>%
  rowwise() %>%
  mutate(Janvier = sum(`Janvier 2020 < 1 ans`, `Janvier 2020 1-4 ans`, `Janvier 2020 5-14 ans`, `Janvier 2020 Adultes Masculins`,`Janvier 2020 Adultes Feminins`, na.rm = T),
         Fevrier = sum(`Février 2020 < 1 ans`, `Février 2020 1-4 ans`, `Février 2020 5-14 ans`, `Février 2020 Adultes Masculins`, `Février 2020 Adultes Feminins`, na.rm = T),
         Mars = sum(`Mars 2020 < 1 ans`, `Mars 2020 1-4 ans`, `Mars 2020 5-14 ans`, `Mars 2020 Adultes Masculins`, `Mars 2020 Adultes Feminins`, na.rm = T),
         Avril = sum(`Avril 2020 < 1 ans`, `Avril 2020 1-4 ans`, `Avril 2020 5-14 ans`, `Avril 2020 Adultes Masculins`, `Avril 2020 Adultes Feminins`, na.rm = T),
         Mai = sum(`Mai 2020 < 1 ans`, `Mai 2020 1-4 ans`, `Mai 2020 5-14 ans`, `Mai 2020 Adultes Masculins`, `Mai 2020 Adultes Feminins`, na.rm = T),
         Juin = sum(`Juin 2020 < 1 ans`, `Juin 2020 1-4 ans`, `Juin 2020 5-14 ans`, `Juin 2020 Adultes Masculins`, `Juin 2020 Adultes Feminins`, na.rm = T),
         Juillet = sum(`Juillet 2020 1-4 ans`, `Juillet 2020 < 1 ans`, `Juillet 2020 5-14 ans`, `Juillet 2020 Adultes Masculins`, `Juillet 2020 Adultes Feminins`, na.rm = T),
         Aout = sum(`Août 2020 < 1 ans`, `Août 2020 1-4 ans`, `Août 2020 5-14 ans`, `Août 2020 Adultes Masculins`, `Août 2020 Adultes Feminins`, na.rm = T),
         Septembre = sum(`Septembre 2020 < 1 ans`, `Septembre 2020 1-4 ans`, `Septembre 2020 5-14 ans`, `Septembre 2020 Adultes Masculins`, `Septembre 2020 Adultes Feminins`, na.rm = T),
         Octobre = sum(`Octobre 2020 < 1 ans`, `Octobre 2020 1-4 ans`, `Octobre 2020 5-14 ans`, `Octobre 2020 Adultes Masculins`, `Octobre 2020 Adultes Feminins`, na.rm = T),
         Novembre = sum(`Novembre 2020 < 1 ans`, `Novembre 2020 1-4 ans`, `Novembre 2020 5-14 ans`, `Novembre 2020 Adultes Masculins`, `Novembre 2020 Adultes Feminins`, na.rm = T),
         Decembre = sum(`Décembre 2020 < 1 ans`, `Décembre 2020 1-4 ans`, `Décembre 2020 5-14 ans`, `Décembre 2020 Adultes Masculins`, `Décembre 2020 Adultes Feminins`, na.rm = T),
         adm2 = ifelse(is.na(orgunitlevel4), orgunitlevel3, orgunitlevel4))%>%
  dplyr::select(adm2, Janvier, Fevrier, Mars, Avril, Mai, Juin, Juillet, Aout, Septembre, Octobre, Novembre, Decembre) %>%
  gather(month, allout, Janvier:Decembre)%>%
  mutate(month = case_when(month == "Janvier" ~ 1,
                           month == "Fevrier" ~ 2,
                           month == "Mars" ~ 3,
                           month == "Avril" ~ 4,
                           month == "Mai" ~ 5,
                           month == "Juin" ~ 6,
                           month == "Juillet" ~ 7,
                           month == "Aout" ~ 8,
                           month == "Septembre" ~ 9,
                           month == "Octobre" ~ 10,
                           month == "Novembre" ~ 11,
                           month == "Decembre" ~ 12),
         year = rep(2020, 44904)) %>% 
  mutate(month = as.numeric(month), year = as.numeric(year)) %>%
  dplyr::select(adm2, month, year, allout) %>%
  bind_rows(CHU_CHR_2)

### All-causes outpatients visits for 2023

tt_consultations_2020_HF_CHU = all_causes_visits %>% pivot_longer(cols = `Août 2023`: `Septembre 2023`,names_to = "period" ,values_to = 'allout')%>%
  separate(period, into = c("month", "year"), sep = " ") %>%
  mutate(adm2 = ifelse(is.na(orgunitlevel4), orgunitlevel3, orgunitlevel4),
  month = case_when(month == "Janvier" ~ 1,
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
                           month == "Décembre" ~ 12),
         year = rep(2023, 37788)) %>% 
  mutate(month = as.numeric(month), year = as.numeric(year)) %>%
  dplyr::select(adm2, month, year, allout)
  


## Recoding tt_consultations_2020_HF_CHU$adm2
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHR Banfora"] <- "Banfora"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHR Dédougou"] <- "Dedougou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHR Dori"] <- "Dori"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHR Fada N'Gourma"] <- "Fada"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHR Gaoua"] <- "Gaoua"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHR Kaya"] <- "Kaya"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHR Koudougou"] <- "Koudougou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHR Tenkodogo"] <- "Tenkodogo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHR Ziniaré"] <- "Ziniare"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHU Bogodogo"] <- "Bogodogo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHU Pédiatrique CDG"] <- "Bogodogo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHU Sanou Souro"] <- "Do"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHU Tengandogo"] <- "Tengandogo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHU Yalgado"] <- "Baskuy"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CHUR Ouahigouya"] <- "Ouahigouya"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CNAOB"] <- "Baskuy"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CNLAT"] <- "Baskuy"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "CNRMPR"] <- "Boulmiougou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Banfora"] <- "Banfora"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Barsalogho"] <- "Barsalogho"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Baskuy"] <- "Baskuy"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Batié"] <- "Batie"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Bittou"] <- "Bitou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Bogande"] <- "Bogande"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Bogodogo"] <- "Bogodogo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Boromo"] <- "Boromo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Boulmiougou"] <- "Boulmiougou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Boulsa"] <- "Boulsa"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Boussé"] <- "Bousse"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Boussouma"] <- "Boussouma"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Dafra"] <- "Dafra"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Dande"] <- "Dande"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Dano"] <- "Dano"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Dedougou"] <- "Dedougou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Diapaga"] <- "Diapaga"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Diébougou"] <- "Diebougou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Djibo"] <- "Djibo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Do"] <- "Do"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Dori"] <- "Dori"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Fada"] <- "Fada"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Gaoua"] <- "Gaoua"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Garango"] <- "Garango"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Gayeri"] <- "Gayeri"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Gorom-Gorom"] <- "Gorom"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Gourcy"] <- "Gourcy"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Hounde"] <- "Hounde"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Kampti"] <- "Kampti"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Karangasso Vigue"] <- "Karangasso Vigue"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Kaya"] <- "Kaya"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Kombissiri"] <- "Kombissiri"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Kongoussi"] <- "Kongoussi"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Koudougou"] <- "Koudougou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Koupéla"] <- "DS Koupela"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Léna"] <- "Lena"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Léo"] <- "Leo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Manga"] <- "Manga"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Mangodara"] <- "Mangodara"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Manni"] <- "Manni"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS N'Dorola"] <- "NDorola"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Nanoro"] <- "Nanoro"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Nongr-Massom"] <- "Nongr-Massom"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Nouna"] <- "Nouna"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Orodara"] <- "Orodara"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Ouahigouya"] <- "Ouahigouya"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Ouargaye"] <- "Ouargaye"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Pama"] <- "Pama"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Po"] <- "Po"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Pouytenga"] <- "Pouytenga"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Réo"] <- "Reo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Sabou"] <- "Sabou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Sapone"] <- "Sapone"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Sapouy"] <- "Sapouy"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Sebba"] <- "Sebba"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Séguénéga"] <- "Seguenega"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Sig-Noghin"] <- "Sig-Noghin"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Sindou"] <- "Sindou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Solenzo"] <- "Solenzo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Tenado"] <- "Tenado"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Tenkodogo"] <- "Tenkodogo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Thiou"] <- "Thiou"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Titao"] <- "Titao"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Toma"] <- "Toma"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Tougan"] <- "Tougan"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Tougouri"] <- "Tougouri"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Yako"] <- "Yako"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Zabré"] <- "Zabre"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Ziniaré"] <- "Ziniare"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "DS Zorgho"] <- "Zorgho"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "Hôpital Paul VI"] <- "Sig-Noghin"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "Hôpital Saint Camille de Ouagadougou"] <- "Bogodogo"
tt_consultations_2020_HF_CHU$adm2[tt_consultations_2020_HF_CHU$adm2 == "Gynéco Obstétricale Hôpital SCHIPHRA"] <- "Nongr-Massom"

tt_consultations_2020_HF_CHU = tt_consultations_2020_HF_CHU %>%
  group_by(adm2, month, year)%>%
  summarise(allout = sum(allout, na.rm = T))
  
### Estimations des autres covariables
## Access and usage of ITN

years_25pct <- c(2017, 2020, 2023)  # 1 year after 2016, 2019, 2022
years_50pct <- c(2018, 2021, 2024)  # 2 years after 2016, 2017, 2022

# Apply reductions
data_adjust <- data %>%
  mutate(across(access:Usage, 
                ~ case_when(
                  year %in% years_25pct ~ .x * 0.75,  # 25% reduction (multiply by 0.75)
                  year %in% years_50pct ~ .x * 0.50,  # 50% reduction (multiply by 0.50)
                  TRUE ~ .x                           # No change for other years
                )))

# View the adjusted data
head(data_stock_out_adjusted)
# 1. Health facility usage (ratio of new consultations to the total population)
# 5. SMC (number of SMC year and number of cycles) # Banfora, Mangodara et Sindou, Leo et Sapouy,Po,Pama,Dafra,
# Do, Hounde, Karangasso-Vigue, Lena, N'dorola et Orodara,  Batie, Dano, Diebougou, Gaoua et Kampti
Districts = c('banfora', 'mangodara', 'sindou', 'leo', 'sapouy', 'po', 'pama', 'dafra', 'do', 'hounde', 'karangasso vigue',
              'lena', 'ndorola', 'orodara', 'batie', 'dano', 'diebougou', 'gaoua', 'kampti')


data_cps = yearly_DS_incidence %>% select(adm2, year) %>%
  left_join(coverag3, by = c('adm2'))%>%
  mutate(cycles_SMC = ifelse(nbre_year_cps > 0 & year %in% c(2016, 2017, 2018, 2020), 4, ifelse(nbre_year_cps > 0 & year %in% c(2021, 2022) & 
                                                                                                  adm2 %in% Districts, 5, 
                                                                                            ifelse(nbre_year_cps == 0, 0, 4))))

data_cps = data_cps %>%
  mutate(cycles_SMC = ifelse(nbre_year_cps > 0 & year %in% c(2016, 2017, 2018, 2020), 4, ifelse(nbre_year_cps > 0 & year %in% c(2021, 2022) & 
                                                                                                  adm2 %in% Districts, 5, 
                                                                                                ifelse(nbre_year_cps == 0, 0, 4))))



data_routine_stock_out_DS_year = monthly_DS_year_finale %>%
  left_join(tt_consultations_2020_HF_CHU, by=c('adm2','month', 'year')) %>% 
  mutate(allout.x = ifelse(year == 2020, allout.y, allout.x)) %>% 
  select(-allout.y) %>%
  group_by(adm2, year) %>%
  summarise(across(allout.x:pres_ov5, ~sum(.x, na.rm = TRUE))) %>%
  left_join(pop_DS_2016_2022, by=c('adm2', 'year'))%>%
  mutate(District.Pop = as.numeric(as.character(District.Pop)),
         usage_rate = round(allout.x/District.Pop,2),
         U5_pop = ceiling(District.Pop * .18))%>%
         #prop_U5 = round(U5_pop/District.Pop,2)) %>%
  left_join(data_stock_out_DS_year_2016_2022, by=c("adm2", 'year')) %>%
  left_join(data_cps, by=c("adm1","adm2", "year"))

 
## 2. Health facility access (proportion of the population living within a radius of 5 km from a HF)

# 3. Proportion of health facility closed by region
# 4. Fraction of children under-five living in the areas (HD)
# Recoding population_moins_de_5_ans_et_total$orgunitlevel4 into population_moins_de_5_ans_et_total$adm2
population_moins_de_5_ans_et_total$adm2 <- population_moins_de_5_ans_et_total$orgunitlevel4 %>%
  fct_recode(
    "Banfora" = "DS Banfora",
    "Barsalogho" = "DS Barsalogho",
    "Baskuy" = "DS Baskuy",
    "Batie" = "DS Batié",
    "Bitou" = "DS Bittou",
    "Bogande" = "DS Bogande",
    "Bogodogo" = "DS Bogodogo",
    "Boromo" = "DS Boromo",
    "Boulmiougou" = "DS Boulmiougou",
    "Boulsa" = "DS Boulsa",
    "Bousse" = "DS Boussé",
    "Boussouma" = "DS Boussouma",
    "Dafra" = "DS Dafra",
    "Dande" = "DS Dande",
    "Dano" = "DS Dano",
    "Dedougou" = "DS Dedougou",
    "Diapaga" = "DS Diapaga",
    "Diebougou" = "DS Diébougou",
    "Djibo" = "DS Djibo",
    "Do" = "DS Do",
    "Dori" = "DS Dori",
    "Fada" = "DS Fada",
    "Gaoua" = "DS Gaoua",
    "Garango" = "DS Garango",
    "Gayeri" = "DS Gayeri",
    "Gorom" = "DS Gorom-Gorom",
    "Gourcy" = "DS Gourcy",
    "Hounde" = "DS Hounde",
    "Kampti" = "DS Kampti",
    "Karangasso Vigue" = "DS Karangasso Vigue",
    "Kaya" = "DS Kaya",
    "Kombissiri" = "DS Kombissiri",
    "Kongoussi" = "DS Kongoussi",
    "Koudougou" = "DS Koudougou",
    "Koupela" = "DS Koupéla",
    "Lena" = "DS Léna",
    "Leo" = "DS Léo",
    "Manga" = "DS Manga",
    "Mangodara" = "DS Mangodara",
    "Manni" = "DS Manni",
    "Ndorola" = "DS N'Dorola",
    "Nanoro" = "DS Nanoro",
    "Nongr-Massom" = "DS Nongr-Massom",
    "Nouna" = "DS Nouna",
    "Orodara" = "DS Orodara",
    "Ouahigouya" = "DS Ouahigouya",
    "Ouargaye" = "DS Ouargaye",
    "Pama" = "DS Pama",
    "Po" = "DS Po",
    "Pouytenga" = "DS Pouytenga",
    "Reo" = "DS Réo",
    "Sabou" = "DS Sabou",
    "Sapone" = "DS Sapone",
    "Sapouy" = "DS Sapouy",
    "Sebba" = "DS Sebba",
    "Seguenega" = "DS Séguénéga",
    "Sig-Noghin" = "DS Sig-Noghin",
    "Sindou" = "DS Sindou",
    "Solenzo" = "DS Solenzo",
    "Tenado" = "DS Tenado",
    "Tenkodogo" = "DS Tenkodogo",
    "Thiou" = "DS Thiou",
    "Titao" = "DS Titao",
    "Toma" = "DS Toma",
    "Tougan" = "DS Tougan",
    "Tougouri" = "DS Tougouri",
    "Yako" = "DS Yako",
    "Zabre" = "DS Zabré",
    "Ziniare" = "DS Ziniaré",
    "Zorgho" = "DS Zorgho"
  )

population_moins_de_5_ans_et_total = population_moins_de_5_ans_et_total %>% select(adm2, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`,`2022`)%>% pivot_longer(cols = c(`2016`, `2017`, `2018`, `2019`, `2020`, `2021`,`2022`),
                                                                                         names_to = 'year',
                                                                                         values_to = 'U5_pop')
# 6. Treatment seeking (No_treatment, treatment seeking)
data_trtm_seeking = BFA_trtm_region %>% select(adm1, year, publ_cov = prop_publ, priv_cov = prop_priv, notrt_cov = No_treat) %>%
  group_by(adm1, year) %>%
  mutate(trtm_seeking = sum(publ_cov, priv_cov, na.rm = T),
         no_trtm = notrt_cov) %>%
  select(-publ_cov, -priv_cov, -notrt_cov)


### 
tmap::tm_shape(nvlle_HD_rate) +
  tmap::tm_polygons(col = "adm1") +
  tmap::tm_legend(show=FALSE) +
  tmap::tm_text("adm1", size = 1/2)















