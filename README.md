# 🏘️ Housing Deficit Peru
This repository contains data processing scripts and a dashboard prototype to estimate and visualize the housing deficit across Peruvian districts using census microdata. The project was originally developed as part of a consulting engagement with the Peruvian Association of Real Estate Companies (ASEI).

## 📊 Project Overview
### Objective
To provide granular, district-level estimates of Peru's housing deficit by type (quantitative vs. qualitative) using 2017 Census data, and to visualize the results in an interactive dashboard.

### Tools and Technologies
- R for data manipulation, analysis, and dashboard development
- Redatam for querying microdata from the Peruvian National Census
- Shiny for interactive visualization

## 📁 Repository Structure
```
HousingDeficitPeru/
│
├── data/                           # Contains output datasets
│   ├── housing_deficit.csv         # Contains housing deficit indicators based on INEI's methodology
│   └── housing_deficit_new.csv     # Contains housing deficit indicators based on a new methodology
│
├── census_scripts/                 # Scripts for data processing and transformation using REDATAM
│   ├── housing_indicators.spc      # Script for generating housing deficit indicators based on INEI's methodology
│   └── housing_new_indicators.spc  # Script for generating housing deficit indicators based on a new methodology
│
├── dashboard/              # Shiny dashboard source code
│   └── app.R
│
└── README.md               # Project documentation (this file)
```

## 📐 Methodology
The housing deficit is estimated based on official definitions provided by the [National Office of Statistics and Informatics (INEI)](https://proyectos.inei.gob.pe/iinei/srienaho/Descarga/DocumentosMetodologicos/2023-18/08_Ficha_Metodologica_Bono_Familiar_Habitacional.pdf), distinguishing between:
- **Quantitative Deficit**: Households without a dwelling or living in non-durable dwellings.
- **Qualitative Deficit**: Households living in dwellings lacking basic services or requiring major improvements.

Key steps:
- Data extraction using [Redatam](https://redatam.org/es) based on selected variables.
- Data cleaning to standardize responses and define indicator thresholds.
- Visualization using Shiny for dynamic exploration of results.

## 🌐 Access the Dashboard
Explore the interactive dashboard here:
👉 [Housing Deficit in Peru Dashboard](https://cesarnunezh.shinyapps.io/DeficitHabitacional/)

Use the interface to:
- Filter by region and housing deficit type
- Compare district-level estimates