# Retrospective Analysis of Malaria Trends in Burkina Faso (2016–2023)

[![Made with R](https://img.shields.io/badge/Made%20with-R-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Portfolio](https://img.shields.io/badge/Portfolio-Ousmane%20Diallo-orange)](https://ousmanerabi.github.io/projects/retrospective_analysis.html)

---

## Overview  
This project analyzes **routine malaria surveillance data** from Burkina Faso (2016–2023) to:  
- Assess malaria incidence trends at the **health district level**  
- Adjust incidence using WHO’s framework (testing, reporting, care-seeking)  
- Identify **key drivers of malaria transmission** (interventions & climate)  

The analysis combines **routine HMIS data**, **DHS/MIS surveys**, **PNLP intervention coverage**, and **CHIRPS rainfall data**.  

---

##  Data Sources  
- **HMIS (Health Management Information System):** malaria cases, 2016–2023  
- **PNLP (Programme National de Lutte contre le Paludisme):** ITN, SMC, stockouts  
- **DHS/MIS:** ITN ownership, treatment-seeking, IPTp coverage  
- **CHIRPS:** Rainfall estimates  

**Note**: Routine & intervention data are **not public**. DHS/MIS & CHIRPS climate data are publicly available.  

---

## Methods  

### Data Management  
- Standardized district & health facility names  
- Removed duplicates / inconsistencies  
- Classified facility reporting status (active vs inactive)  
- Detected & imputed outliers (3×SD, MAD, IQR methods)  

### Incidence Adjustments  
- **Crude incidence** = confirmed cases ÷ population × 1000  
- **Adjustment 1** = + testing completeness  
- **Adjustment 2** = + reporting completeness  
- **Adjustment 3** = + treatment-seeking rates  

### Trend Analysis  
- Seasonal-Trend decomposition (STL)  
- Sen’s slope (trend magnitude)  
- Mann-Kendall test (trend significance, α=0.05)  

### Drivers of Malaria Incidence  
- Generalized Additive Models (GAM)  
- Covariates: rainfall, ITN/SMC coverage, stockouts, treatment-seeking  

---

## Key Insights  
- **Adjustment 1 ≈ crude incidence** → effective testing policies  

---

## Repository Structure  
```plaintext
Retrospective_analysis/
├── data/                 # Placeholder for raw data (not shared)
├── scripts/              # R scripts (cleaning, analysis, figures)
├── figures/              # Generated maps & plots
├── outputs/              # Results, tables, exports
├── retrospective_analysis.qmd   # Quarto project page
└── README.md             # Project description
