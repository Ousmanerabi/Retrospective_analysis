################################################################################
# Retrospective Malaria Trend Analysis (Burkina Faso, 2016–2024)
# Author: Ousmane DIALLO
# Purpose: End-to-end script to compute WHO-style incidence indicators,
#          visualize monthly trends, run STL + Mann-Kendall trend tests,
#          and build maps of Sen’s slopes and incidence categories.
#
# NOTE: This script assumes that several data frames already exist
#       in your environment (same names you used in your notebook):
#       - adj_incidence_all_pop
#       - monthly_DS_incide   (built below)
#       - annual_incidence
#       - STL_result_DF_slopes (derived below)
#       - district_report, care_seeking_data2, data_population_yearly (if used)
#       - Shapefile path: BFA_Districts_Sanitaires_2022.*
#
################################################################################


## 1) Libraries -------------------------------------------------------------------
# Data manipulation, dates, time series, spatial, plotting, IO, etc.
library(dplyr)       # Data wrangling
library(lubridate)   # Date handling
library(plyr)        # Compatibility utilities (if needed)
library(season)      # Mann-Kendall and Sen's slope (also see 'trend' package)
library(sf)          # Spatial vector data
library(tmap)        # Thematic maps
library(ggplot2)     # Graphics
library(readxl)      # Read Excel files
library(scales)      # Scales for ggplot
library(data.table)  # Fast binds / list -> data.frame
library(stringr)     # String utilities
library(trend)       # smk.test, sea.sens.slope
library(zoo)         # yearmon and time series convenience
library(stlplus)     # STL decomposition with a nice interface

## 2) Load data -------------------------------------------------------------------


# Example CSV used in the manuscript code
data_for_trend <- read.csv(
  "~/Downloads/data_for_trend_analysis_2016_2023.csv"
)


## 3) WHO-style incidence indicators ---------------------------------------------
# Explanation: Compute monthly indicators for crude and adjusted incidence
# (Adj1–Adj3), following your WHO framework.

monthly_DS_incide <- adj_incidence_all_pop %>%
  dplyr::mutate(
    Date        = make_date(year = Year, month = Month, day = 1),
    TPR         = `cas positif` / `test realises`,
    pop_monthly = `Population totale`/12,
    # Outputs per 1,000 pop
    mal_cases                           = (Nbrut / pop_monthly) * 1000,
    incidence_adj_presumed_cases        = (N1 / pop_monthly) * 1000,
    incidence_adj_presumed_cases_RR     = (N2 / pop_monthly) * 1000,
    incidence_adj_presumed_cases_RR_TSR = (N3 / pop_monthly) * 1000
  ) %>%
  dplyr::mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .)))

## 4) Monthly trends visualization (crude example) --------------------------------
# Explanation: Plot monthly crude incidence with transparent lines per district.

p1 <- ggplot2::ggplot(
  monthly_DS_incide,
  aes(x = Date, y = `Incidence brute`, group = as.factor(districts))
) +
  geom_line(alpha = 0.25, size = 1, show.legend = FALSE, color = "blue") +
  ylab("") + ggtitle("Monthly crude incidence") +
  scale_x_yearmon(
    "Date",
    breaks = sort(unique(monthly_DS_incide$Date))[c(seq(1, 60, 6), 60)],
    labels = sort(unique(monthly_DS_incide$Date))[c(seq(1, 60, 6), 60)]
  ) +
  theme_bw() +
  theme(
    plot.title   = element_text(hjust = 0.5),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),
    panel.grid   = element_blank(),
    axis.line    = element_line(colour = "black")
  )

# Save 
 pdf("~/Desktop/Trend_analysis/monthly_incidence_plots.pdf", width = 10, height = 8)
 print(p1)
 dev.off()

## 5) Rate variation analysis (2016 vs 2023) --------------------------------
# Explanation: Compute relative changes and classify start vs end categories

Augment_dimuni <- annual_incidence %>%
  dplyr::group_by(districts) %>%
  dplyr::arrange(year) %>%
  dplyr::summarise(
    first_crude = first(inc_brut_yr),
    last_crude  = last(inc_brut_yr),
    first_N3    = first(inc_N3_yr),
    last_N3     = last(inc_N3_yr),
    first_N2    = first(inc_N2_yr),
    last_N2     = last(inc_N2_yr),
    first_N1    = first(inc_N1_yr),
    last_N1     = last(inc_N1_yr),
    rate   = (last_crude - first_crude) / first_crude * 100,
    rateN3 = (last_N3 - first_N3)       / first_N3   * 100,
    rateN2 = (last_N2 - first_N2)       / first_N2   * 100,
    rateN1 = (last_N1 - first_N1)       / first_N1   * 100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(districts, .keep_all = TRUE)

yearly_DS_incidence <- Augment_dimuni %>%
  dplyr::mutate(
    # Baseline category at first year for N3
    categ3 = dplyr::case_when(
      first_N3 < 100 ~ "very low",
      first_N3 >= 100 & first_N3 < 250 ~ "low",
      first_N3 >= 250 & first_N3 < 450 ~ "Moderate",
      first_N3 >= 450 ~ "High",
      TRUE ~ "low"
    ),
    # Final category at last year for N3
    categ4 = dplyr::case_when(
      last_N3 < 100 ~ "very low",
      last_N3 >= 100 & last_N3 < 250 ~ "low",
      last_N3 >= 250 & last_N3 < 450 ~ "Moderate",
      last_N3 >= 450 ~ "High",
      TRUE ~ "low"
    ),
    # Classify the level of transmission
    categ5 = paste(categ3, categ4, sep = "-"),
    cat3 = dplyr::case_when(
      categ5 %in% c("High-low", "High-Moderate", "Moderate-low") ~ "Decrease",
      categ5 %in% c("High-High", "Moderate-Moderate", "low-low") ~ "Stable",
      TRUE ~ "Increase"
    )
  )

# Quick diagnostic plot of relative change vs final level (N3)
ggplot2::ggplot(yearly_DS_incidence, aes(x = last_N3, y = rateN3, color = categ5)) +
  geom_point() +
  theme_bw() +
  xlab("Adjusted incidence 3 (final year)") +
  ylab("Relative percent change (%)") +
  ggtitle("Variation of adjusted incidence 3 by district")

## 6) STL decomposition + Mann-Kendall / Sen’s slope ------------------------------
# Explanation:
#  - Normalize each series for comparability
#  - Decompose trend/seasonality with STL
#  - Test trend significance with Mann-Kendall and estimate Sen’s slope

# 6.1 Normalization helper
getNormalized <- function(vec) {
  if (!is.numeric(vec) || all(is.na(vec))) {
    warning("Input vector is non-numeric or all NA; returning original vector")
    return(vec)
  }
  vec_mean <- mean(vec, na.rm = TRUE)
  vec_sd   <- sd(vec,  na.rm = TRUE)
  if (is.na(vec_sd) || vec_sd == 0) {
    warning("Standard deviation is 0 or NA; returning original vector")
    return(vec)
  }
  (vec - vec_mean) / vec_sd
}

monthly_DS_incidence <- monthly_DS_incide %>%
  dplyr::mutate(
    mal_cases_norm                            = getNormalized(`Incidence brute`),
    incidence_adj_presumed_cases_norm         = getNormalized(Adj1),
    incidence_adj_presumed_cases_RR_norm      = getNormalized(Adj2),
    incidence_adj_presumed_cases_RR_TSR_norm  = getNormalized(Adj3)
  )

# 6.2 STL loop over districts and indicators
STL_result_DF_norm_list <- list()
indicators <- list(
  list(col = "mal_cases_norm",                           type = "Crude Incidence"),
  list(col = "incidence_adj_presumed_cases_norm",        type = "Adjusted Presumed Cases"),
  list(col = "incidence_adj_presumed_cases_RR_norm",     type = "Adjusted Presumed Cases RR"),
  list(col = "incidence_adj_presumed_cases_RR_TSR_norm", type = "Adjusted Presumed Cases RR TSR")
)

for (DS in base::sort(base::unique(monthly_DS_incidence$districts))) {
  cases_dist <- monthly_DS_incidence %>%
    dplyr::filter(districts == DS) %>%
    dplyr::arrange(Date)
  
  if (nrow(cases_dist) == 0) next
  
  for (ind in indicators) {
    if (!ind$col %in% names(cases_dist)) next
    
    ind_values  <- cases_dist[[ind$col]]
    valid_idx   <- !is.na(ind_values)
    valid_vals  <- ind_values[valid_idx]
    valid_dates <- cases_dist$Date[valid_idx]
    
    if (length(valid_vals) < 2) next
    
    start_year  <- base::as.numeric(format(valid_dates[1], "%Y"))
    start_month <- base::as.numeric(format(valid_dates[1], "%m"))
    ind_ts      <- stats::ts(valid_vals, start = c(start_year, start_month), deltat = 1/12)
    ind_stl     <- stlplus::stlplus(ind_ts, s.window = "periodic")
    
    # Build row-wise result
    ind_stl_ts            <- base::as.data.frame(ind_stl$data[, 1:4])  # seasonal/trend/remainder + data
    ind_stl_ts$type       <- ind$type
    ind_stl_ts$dates      <- valid_dates
    ind_stl_ts$MK_p       <- trend::smk.test(ind_ts)$p.value
    ind_stl_ts$sens_slope <- trend::sea.sens.slope(ind_ts)
    ind_stl_ts$District   <- DS
    
    STL_result_DF_norm_list[[base::paste(DS, ind$col, sep = "_")]] <- ind_stl_ts
  }
}

STL_result_DF_norm   <- data.table::rbindlist(STL_result_DF_norm_list, fill = TRUE)
STL_result_DF_slopes <- base::unique(STL_result_DF_norm[, c("type", "MK_p", "sens_slope", "District")])

#  trend panel
g2 <- ggplot(STL_result_DF_norm, aes(x = dates, y = trend)) +
     geom_line(aes(group = District), show.legend = FALSE, color = "blue") +
     facet_wrap(~type, ncol = 3) +
     theme_bw()

## 7) District-level SI plots ------------------------------------------------------
# Explanation: Build small panels for subsets of districts to visualize trends.

for (i in seq(1, 70, 4)) {
  DS_list <- unique(STL_result_DF_norm[order(STL_result_DF_norm$District), "District"])[i:(i+2)]
  plotting_DF <- STL_result_DF_norm[which(STL_result_DF_norm$District %in% DS_list$District),]
  plotting_DF$District <- stringr::str_to_title(plotting_DF$District)
  
  plot_trend_1 <- ggplot2::ggplot(
    plotting_DF[
      which(plotting_DF$type %in% c("Crude Incidence", "Adjusted Presumed Cases",
                                    "Adjusted Presumed Cases RR", "Adjusted Presumed Cases RR TSR")), ],
    aes(x = dates, y = trend)
  ) +
    geom_line(aes(
      group   = type, linetype = "solid",
      color   = factor(type, levels = c("Crude Incidence",
                                        "Adjusted Presumed Cases",
                                        "Adjusted Presumed Cases RR",
                                        "Adjusted Presumed Cases RR TSR"))
    ),
    show.legend = FALSE) +
    scale_color_manual("", values = c("#913058", "#F6851F", "#00A08A", "#8971B3")) +
    scale_linetype_identity("") +
    xlab("") + ylab("") +
    facet_wrap(~District, ncol = 2, scales = "free_y") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line        = element_line(colour = "black", size = 0.5),
      panel.spacing.x  = unit(2, "mm"),
      strip.background = element_blank(),
      axis.text        = element_text(colour = "black", size = 8),
      axis.ticks       = element_line(colour = "black", size = 0.5),
      axis.text.x      = element_text(angle = 45, hjust = 1)
    )
  
  # Save each subset as PDF 
   pdf(paste0("~/Desktop/Trend_analysis/stl/SI_decomp_v3_", i, ".pdf"),
       width = 5.75, height = 2.8)
   print(plot_trend_1)
   dev.off()
}

## 8) Map of Sen’s slopes ----------------------------------------------------------
# Explanation: Read districts shapefile, harmonize names, join Sen’s slopes,
# categorize slopes and map significance.

# Read shapefile (update the path to your local file)
nlleshapefile_sf <- sf::st_read(
  "~/Library/CloudStorage/OneDrive-Personnel/BFA_SNT/Retros_data/WHO_Shapefiles/BFA_Districts_Sanitaires_2022.shp"
)

# Harmonize district names
nlleshapefile_sf$adm2 <- nlleshapefile_sf$Nom_DS
nlleshapefile_sf$adm2 <- dplyr::recode(nlleshapefile_sf$adm2,
                                       "DS Banfora" = "Banfora", "DS Barsalogho" = "Barsalogho", "DS Baskuy" = "Baskuy",
                                       "DS Batié" = "Batie", "DS Bittou" = "Bitou", "DS Bogande" = "Bogande",
                                       "DS Bogodogo" = "Bogodogo", "DS Boromo" = "Boromo", "DS Boulmiougou" = "Boulmiougou",
                                       "DS Boulsa" = "Boulsa", "DS Boussé" = "Bousse", "DS Boussouma" = "Boussouma",
                                       "DS Dafra" = "Dafra", "DS Dande" = "Dande", "DS Dano" = "Dano",
                                       "DS Dedougou" = "Dedougou", "DS Diapaga" = "Diapaga", "DS Diébougou" = "Diebougou",
                                       "DS Djibo" = "Djibo", "DS Do" = "Do", "DS Dori" = "Dori",
                                       "DS Fada" = "Fada", "DS Gaoua" = "Gaoua", "DS Garango" = "Garango",
                                       "DS Gayeri" = "Gayeri", "DS Gorom-Gorom" = "Gorom", "DS Gourcy" = "Gourcy",
                                       "DS Hounde" = "Hounde", "DS Kampti" = "Kampti", "DS Karangasso-Vigue" = "Karangasso Vigue",
                                       "DS Kaya" = "Kaya", "DS Kombissiri" = "Kombissiri", "DS Kongoussi" = "Kongoussi",
                                       "DS Koudougou" = "Koudougou", "DS Koupéla" = "Koupela", "DS Lena" = "Lena",
                                       "DS Léo" = "Leo", "DS Manga" = "Manga", "DS Mangodara" = "Mangodara",
                                       "DS Manni" = "Manni", "DS N'dorola" = "Ndorola", "DS Nanoro" = "Nanoro",
                                       "DS Nongr-Massom" = "Nongr-Massom", "DS Nouna" = "Nouna", "DS Orodara" = "Orodara",
                                       "DS Ouahigouya" = "Ouahigouya", "DS Ouargaye" = "Ouargaye", "DS Pama" = "Pama",
                                       "DS Pô" = "Po", "DS Pouytenga" = "Pouytenga", "DS Réo" = "Reo",
                                       "DS Sabou" = "Sabou", "DS Saponé" = "Sapone", "DS Sapouy" = "Sapouy",
                                       "DS Sebba" = "Sebba", "DS Séguénéga" = "Seguenega", "DS Sig- noghin" = "Sig-Noghin",
                                       "DS Sindou" = "Sindou", "DS Solenzo" = "Solenzo", "DS Tenado" = "Tenado",
                                       "DS Tenkodogo" = "Tenkodogo", "DS Thiou" = "Thiou", "DS Titao" = "Titao",
                                       "DS Toma" = "Toma", "DS Tougan" = "Tougan", "DS Tougouri" = "Tougouri",
                                       "DS Yako" = "Yako", "DS Zabré" = "Zabre", "DS Ziniaré" = "Ziniare",
                                       "DS Zorgho" = "Zorgho"
)

# Prepare slope table (from STL results)
STL_result_DF_slopes <- STL_result_DF_slopes %>%
  dplyr::mutate(
    plotting_sens_slope = ifelse(MK_p > 0.05, NA, sens_slope),
    slope = dplyr::case_when(
      is.na(plotting_sens_slope) ~ "Not significant",
      plotting_sens_slope < 0    ~ "Decrease",
      plotting_sens_slope > 0    ~ "Increase",
      TRUE                       ~ "Not significant"
    ),
    adm2 = District
  )

nvlle_HD_slope <- nlleshapefile_sf %>%
  dplyr::full_join(STL_result_DF_slopes, by = "adm2") %>%
  dplyr::mutate(
    slope_cat = dplyr::case_when(
      MK_p > 0.05                  ~ "Not Significant",
      plotting_sens_slope <= -0.3  ~ "-0.3",
      plotting_sens_slope <= -0.2  ~ "-0.2",
      plotting_sens_slope <= -0.1  ~ "-0.1",
      plotting_sens_slope <=  0.0  ~ "0.0",
      plotting_sens_slope <=  0.1  ~ "0.1",
      plotting_sens_slope <=  0.2  ~ "0.2",
      TRUE                         ~ ">0.2"
    ),
    slope_cat = factor(
      slope_cat,
      levels = c("-0.3", "-0.2", "-0.1", "0.0", "0.1", "0.2", ">0.2", "Not Significant")
    )
  )

slope_colors <- c(
  "-0.3" = "#313695", "-0.2" = "#4575B4", "-0.1" = "#74ADD1",
  "0.0"  = "#D1E5F0",  "0.1" = "#FDDBC7", "0.2" = "#F46D43",
  ">0.2" = "#A50026",  "Not Significant" = "grey70"
)

p5 <- ggplot(nvlle_HD_slope) +
  geom_sf(aes(fill = slope_cat), color = "white", size = 0.2) +
  scale_fill_manual(
    name = "Sen's Slope\n(Gray = Not Significant)",
    values = slope_colors, drop = FALSE
  ) +
  facet_wrap(~type, ncol = 2) +
  theme_void() +
  theme(
    strip.text   = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    plot.title   = element_text(hjust = 0.5)
  )

# Save 
 pdf("~/Desktop/Trend_analysis/sens_slope.pdf", width = 5.75, height = 2.8)
 print(p5)
 dev.off()

## 9) Yearly incidence maps (crude/Adj1/Adj2/Adj3) --------------------------------
# Explanation: Join monthly incidence to shapefile, bin categories, and map.

nvlle_HD <- nlleshapefile_sf %>%
  dplyr::full_join(monthly_DS_incidence, by = "adm2") %>%
  dplyr::mutate(
    incidence_cat      = cut(mal_cases, breaks = c(0, 250, 350, 450, 1000, 2000),
                             labels = c("0–250", "250–350", "350–450", "450–1000", "1000–2000"),
                             include.lowest = TRUE),
    incidence_adj1_cat = cut(incidence_adj_presumed_cases, breaks = c(0, 250, 350, 450, 1000, 2000),
                             labels = c("0–250", "250–350", "350–450", "450–1000", "1000–2000"),
                             include.lowest = TRUE),
    incidence_adj2_cat = cut(incidence_adj_presumed_cases_RR, breaks = c(0, 250, 350, 450, 1000, 2000),
                             labels = c("0–250", "250–350", "350–450", "450–1000", "1000–2000"),
                             include.lowest = TRUE),
    incidence_adj3_cat = cut(incidence_adj_presumed_cases_RR_TSR, breaks = c(0, 250, 350, 450, 1000, 2000),
                             labels = c("0–250", "250–350", "350–450", "450–1000", "1000–2000"),
                             include.lowest = TRUE)
  )

colors_inc <- c("#2166AC", "#4393C3", "#FDDBC7", "#FFFF33", "#B2182B")

p9 <- tm_shape(nvlle_HD) +
  tm_polygons(
    "incidence_cat", title = "Crude incidence (per 1000)",
    style = "fixed", breaks = c(0, 250, 350, 450, 1000, 2000),
    labels = c("0–250", "250–350", "350–450", "450–1000", "1000–2000"),
    palette = colors_inc
  ) +
  tm_facets("year", ncol = 2) +
  tm_layout(legend.outside = TRUE, legend.title.size = 0.6, legend.text.size = 0.6, frame = FALSE,
            main.title = "Crude incidence by year")

p8 <- tm_shape(nvlle_HD) +
  tm_polygons(
    "incidence_adj1_cat", title = "Adjusted incidence 1 (per 1000)",
    style = "fixed", breaks = c(0, 250, 350, 450, 1000, 2000),
    labels = c("0–250", "250–350", "350–450", "450–1000", "1000–2000"),
    palette = colors_inc
  ) +
  tm_facets("year", ncol = 2) +
  tm_layout(legend.outside = TRUE, legend.title.size = 0.6, legend.text.size = 0.6, frame = FALSE,
            main.title = "Adjusted incidence 1 by year")

p7 <- tm_shape(nvlle_HD) +
  tm_polygons(
    "incidence_adj2_cat", title = "Adjusted incidence 2 (per 1000)",
    style = "fixed", breaks = c(0, 250, 350, 450, 1000, 2000),
    labels = c("0–250", "250–350", "350–450", "450–1000", "1000–2000"),
    palette = colors_inc
  ) +
  tm_facets("year", ncol = 2) +
  tm_layout(legend.outside = TRUE, legend.title.size = 0.6, legend.text.size = 0.6, frame = FALSE,
            main.title = "Adjusted incidence 2 by year")

p6 <- tm_shape(nvlle_HD) +
  tm_polygons(
    "incidence_adj3_cat", title = "Adjusted incidence 3 (per 1000)",
    style = "fixed", breaks = c(0, 250, 350, 450, 1000, 2000),
    labels = c("0–250", "250–350", "350–450", "450–1000", "1000–2000"),
    palette = colors_inc
  ) +
  tm_facets("year", ncol = 2) +
  tm_layout(legend.outside = TRUE, legend.title.size = 0.6, legend.text.size = 0.6, frame = FALSE,
            main.title = "Adjusted incidence 3 by year")

# Save
 pdf("~/Desktop/Trend_analysis/incidence_maps.pdf", width = 10, height = 8)
 print(p9); print(p8); print(p7); print(p6)
 dev.off()

## 10) Maps of rate variations -----------------------------------------------------
# Explanation: Join variation table to shapefile and map relative changes.

HD <- nlleshapefile_sf %>%
  dplyr::full_join(Augment_dimuni, by = "adm2")

# Output base path (adjust to your machine)
base_path <- "~/Desktop/Trend_analysis/"

# Helper to create and save a tmap for a given rate column
create_rate_map <- function(data, rate_col, file_name, breaks, title) {
  tm <- tm_shape(data) +
    tm_polygons(
      col = rate_col,
      title = title,
      breaks = breaks,
      palette = "-RdYlBu",  # Diverging palette
      legend.show = TRUE
    ) +
    tm_layout(
      main.title = title,
      main.title.size = 1,
      main.title.position = "center",
      legend.outside = TRUE,
      legend.title.size = 0.6,
      legend.text.size = 0.6,
      frame = FALSE
    )
  
   pdf(file_name, width = 8, height = 6); print(tm); dev.off()

}

rate_configs <- list(
  list(col = "rate",  file = paste0(base_path, "N_rate_crude.pdf"),
       breaks = c(-100, 0, 50, 100, 250), title = "Relative change (crude)"),
  list(col = "rateN1", file = paste0(base_path, "N1_rate.pdf"),
       breaks = c(-100, 0, 50, 100, 250), title = "Relative change (Adj1)"),
  list(col = "rateN2", file = paste0(base_path, "N2_rate.pdf"),
       breaks = c(-100, 0, 50, 100, 250), title = "Relative change (Adj2)"),
  list(col = "rateN3", file = paste0(base_path, "N3_rate.pdf"),
       breaks = c(-100, 0, 50, 100, 250), title = "Relative change (Adj3)")
)

invisible(lapply(rate_configs, function(cfg) {
  create_rate_map(
    data = HD,
    rate_col  = cfg$col,
    file_name = cfg$file,
    breaks    = cfg$breaks,
    title     = cfg$title
  )
}))

################################################################################
# End of script
################################################################################
