################################################################################
# Risk Factors Analysis (GAMM) — Burkina Faso
# Author: Ousmane DIALLO
# Goal: Explore covariates and model adjusted malaria cases using GAMM with
#       district random effects and optional AR(1) temporal correlation.
#
# INPUTS (adjust paths as needed):
#   - data_for_gamm.csv : one row per district-year (or district-month aggregated
#                         to year), containing at least:
#       * response: cases_adjusted_presumed_RR_TSR
#       * offset : District.Pop
#       * covariates: usage_rate, Usage, mean_rain, mean_ACT, rdt_stockout_days,
#                     llins_stockout_days, iptp_stockout_days, nbre_year_cps,
#                     cycles_SMC, access, year, adm2
# 
################################################################################


## 1) Libraries -----------------------------------------------------------------
library(dplyr)     # data wrangling
library(ggplot2)   # graphics
library(mgcv)      # GAM/GAMM (gamm() uses nlme under the hood)
library(zoo)       # yearmon, time helpers
library(corrplot)  # correlation plot
library(gratia)    # nicer GAM visualizations


## 2) Load analysis dataset ------------------------------------------------------
# Replace with your actual path. Keep column names consistent with your workflow.
data_for_gamm <- read.csv('/Users/ousmanediallo/Library/CloudStorage/OneDrive-Personnel/Rainfall_2022/data_adjusted1.csv')



# Ensure types
data_for_gamm <- data_for_gamm %>%
  dplyr::mutate(
    adm2 = as.factor(adm2),
    year = as.numeric(year)
  )

# Create a clean analysis frame with only needed columns
vars_keep <- c("adm2","year","District.Pop","cases_adjusted_presumed_RR_TSR",
               "usage_rate","Usage","mean_rain","mean_ACT",
               "rdt_stockout_days","llins_stockout_days","iptp_stockout_days",
               "nbre_year_cps","cycles_SMC","access")

data_for_gamm_clean <- data_for_gamm %>%
  dplyr::select(all_of(vars_keep)) %>%
  dplyr::filter(!is.na(cases_adjusted_presumed_RR_TSR),
         !is.na(District.Pop))

## 3) Exploratory correlation (numeric covariates only)
covars_numeric <- c("usage_rate","Usage","mean_rain","mean_ACT",
                    "rdt_stockout_days","llins_stockout_days","iptp_stockout_days",
                    "nbre_year_cps","cycles_SMC","access")

cor_mat <- data_for_gamm_clean %>%
  dplyr::select(all_of(covars_numeric)) %>%
  dplyr::mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>%
  stats::cor(use = "pairwise.complete.obs", method = "pearson")

# correlation heatmap
pdf("correlation_matrix.pdf", width = 7, height = 7)
corrplot(cor_mat, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
dev.off()

## 4) Simple log–log exploratory plots vs outcome
# NOTE: We use cases per capita (per person) as outcome for visual checks only.

y_pc <- with(data_for_gamm_clean,
             log((cases_adjusted_presumed_RR_TSR + 1) / District.Pop))

# Function
quick_scatter <- function(x, xlab, file = NULL) {
  df <- data.frame(x = x, y = y_pc)
  p  <- ggplot(df, aes(x = log(x + 1), y = y)) +
    geom_point(alpha = .6) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(x = paste0("log( ", xlab, " + 1 )"),
         y = "log( cases_adjusted / population )",
         title = paste("Exploration:", xlab))
  if (!is.null(file)) {
     ggsave(file, p, width = 6, height = 4)
  }
  print(p)
}

# Apply the function
quick_scatter(data_for_gamm_clean$usage_rate,       "usage_rate")
quick_scatter(data_for_gamm_clean$Usage,            "Usage (bednet use)")
quick_scatter(data_for_gamm_clean$mean_rain,        "mean_rain")
quick_scatter(data_for_gamm_clean$rdt_stockout_days,"rdt_stockout_days")
quick_scatter(data_for_gamm_clean$llins_stockout_days,"llins_stockout_days")
quick_scatter(data_for_gamm_clean$iptp_stockout_days,"iptp_stockout_days")
quick_scatter(data_for_gamm_clean$nbre_year_cps,    "nbre_year_cps")
quick_scatter(data_for_gamm_clean$cycles_SMC,       "cycles_SMC")
quick_scatter(data_for_gamm_clean$mean_ACT,         "mean_ACT")
quick_scatter(data_for_gamm_clean$access,           "access")

## 5) GAMM specification ---------------------------------------------------------
# Rationale:
# - Response: cases_adjusted_presumed_RR_TSR
# - Offset: log(District.Pop) to model rates
# - Random effect: adm2 (district-level heterogeneity)
# - Smooth on usage_rate (typical non-linear) + linear terms for others to limit
#   concurvity; adjust based on your correlation checks.
# - Family: Poisson (or quasi-Poisson/negative binomial if overdispersion matters)
# - AR(1) correlation within district over time (year)

# Base GAMM without AR(1) (useful as a baseline)
mod_base <- gamm(
  cases_adjusted_presumed_RR_TSR ~ offset(log(District.Pop)) +
    s(usage_rate, k = 5) +
    Usage + mean_rain + mean_ACT +
    rdt_stockout_days + llins_stockout_days + iptp_stockout_days +
    nbre_year_cps + cycles_SMC + access,
  random = list(adm2 = ~ 1),
  data   = data_for_gamm_clean,
  family = poisson(link = "log"),
  method = "REML"
)

cat("\n=== Base GAMM (no AR) ===\n")
print(summary(mod_base$gam))

# Final GAMM with AR(1) by district over year
mod_ar1 <- gamm(
  cases_adjusted_presumed_RR_TSR ~ offset(log(District.Pop)) +
    s(usage_rate, k = 5) +
    Usage + mean_rain + mean_ACT +
    rdt_stockout_days + llins_stockout_days + iptp_stockout_days +
    nbre_year_cps + cycles_SMC + access,
  random      = list(adm2 = ~ 1),
  correlation = corAR1(form = ~ year | adm2),  # AR(1) within district
  data        = data_for_gamm_clean,
  family      = poisson(link = "log"),
  method      = "REML"
)

cat("\n=== GAMM with AR(1) ===\n")
print(summary(mod_ar1$gam))
cat("\n--- LME (correlation) summary ---\n")
print(summary(mod_ar1$lme))

## 6) Model diagnostics

 pdf("gam_check_ar1.pdf", width = 7, height = 5)
par(mfrow = c(1, 2))
gam.check(mod_ar1$gam)      
 dev.off()

# Residual ACF/PACF to check remaining autocorrelation
resid_lme <- resid(mod_ar1$lme, type = "normalized")

pdf("acf_pacf_ar1_residuals.pdf", width = 7, height = 4)
par(mfrow = c(1, 2))
acf(resid_lme, lag.max = 36, main = "ACF of normalized residuals")
pacf(resid_lme, lag.max = 36, main = "PACF of normalized residuals")
dev.off()

## 7) Effect plots -----------------------------------------------------

 p_smooths <- gratia::draw(mod_ar1, residuals = FALSE, rug = TRUE)
 ggsave("gamm_smooths_ar1.pdf", p_smooths, width = 8, height = 6)

################################################################################
# End of file
################################################################################
