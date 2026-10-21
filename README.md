# Pricing COVID-19 Vaccines in Southeast Asia: retrospective modelling to inform prospective negotiations

## Overview

This repository contains the R code used to conduct the analyses presented in the manuscript:

**Title:** Pricing COVID-19 Vaccines in Southeast Asia: retrospective modelling to inform prospective negotiations

**Authors:** Afifah Rahman-Shepherd, Dorothy Boyle, Benjamin Coghlan, Mutiara Indriani, Bryan Mercurio, Jeremy Turner-Gorospe, Nakorn Premsri, Justin Wong, Kiesha Prem

**Journal:** BMJ Public Health  

**DOI:** [Manuscript DOI, if available]

The study aimed to illustrate the potential economic and equity impact of regional pooled procurement by modelling 3 post-hoc scenarios of COVID-19 vaccine procurement in ASEAN: bilateral procurement scenario, uniform price procurement scenario, and tiered price procurement scenario. The tiered price mechanism proposed involves price adjustment by a measure of country economic capacity. In this analysis, we present tiered price by gross domestic product (GDP) per capita and tiered price by current health expenditure (CHE) per capita.

The code is provided to support research transparency and reproducibility.

## 1. Repository structure

The repository is organised as follows:

```text
vaccine_group_buy/
├── data/
└── output/
│   ├── plots/
│   ├── intermediate/
│   ├── tables/
├── renv/
├── scripts/
│   ├── 0_helper_functions/
│   ├── 1_data_preparation/
│   ├── 2_psa/
│   └── 3_tables/
│   └── 4_plotting/
├── README.md
```

The R scripts perform the following tasks:

| Directory | Description |
|---|---|
| `0_helper_functions/` | Helper functions |
| `1_data_preparation/` | Preparation of datasets used in the analysis |
| `2_psa/` | Sampling from empirical distribution and running outcomes for PSA  |
| `3_tables/` | Running pricing scenarios, generating tables in manuscript |
| `4_plotting/` | Generation of plots presented in the manuscript |

## 2. Software requirements

All analyses were conducted using **R version [4.5.3]**.

The analyses require the following R packages:

| Package | Purpose |
|---|---|
| `countrycode` | Labelling iso3code for countries |
| `econocharts` | Assists plotting for equity outcomes |
| `grid` | Plotting |
| `rineq` | Calculating concentration index (equity outcome) |
| `[package]` | [Purpose] |

The package versions used in the analyses are documented in `renv.lock`.

To restore the package environment, run:

```r
install.packages("renv")
renv::restore()
```

## 3. Data availability

The datasets used in this study are publicly available from the [MI4A](https://www.who.int/publications/m/item/2026-mi4a-public-vaccine-purchase-dataset), [World Bank](https://data.worldbank.org) and [World Health Organization (WHO)](https://data.who.int/) databases. The MI4A dataset used was last updated on September 2025.

## 4. Reproducing the analyses

To reproduce the analyses:

1. Download or clone this repository.
2. Open `vaccine_group_buy.Rproj` in RStudio.
3. Restore the R package environment using `renv::restore()`.
4. Execute the R scripts in the following order:
   - `1_data_preparation/get_main_data.R`
   - `1_data_preparation/get_mi4a_data.R`
   - `2_psa/psa_main_script.R`
   - `3_tables/run_all_tables.R`
   - `4_plotting/run_all_plots.R`
5. Review the generated outputs in the `outputs/` directory.

All file paths are relative to the project directory.

## 5. Mapping R scripts to results displayed in the manuscript.

| Manuscript component | R script |
|---|---|
| Table 2a-b: Vaccine price per dose and expenditure under bilateral, fixed and tiered (adjusted by GDP and CHE per capita) pricing scenarios | `3_tables/table2ab_base_fullcoverage.R` |
| Figure 2a-h: Vaccine price per dose, country-level and regional expenditure under the pricing scenarios | `4_plotting/mainpaper_plot2.R` |
| Figure 3a-p: Equity of vaccine pricing burden in terms of share of GDP per capita and CHE per capita incurred  | `4_plotting/mainpaper_plot3.R` |
| Supplementary Figure 1: Health expenditure and out-of-pocket spending in health plotted against GDP per capita | `4_plotting/supp_plot1_gdppc_oops_plot` |
| Supplementary Table 1a-b: Varying $\alpha$ in tiered pricing | `3_tables/supptable1ab_alpha_fullcoverage.R` |
| Supplementary Figure 2: Distribution of vaccine price per dose for WHO WPRO and SEARO according to MI4A database | `4_plotting/supp_plot2_mi4a_boxplots.R` |
| Supplementary Table 2a-d: Outcome ranges from probabilistic sensitivity analysis (PSA) for the pricing scenarios | `3_tables/supptable2a-d_psa_fullcoverage.R` |
| Supplementary Figure 3: Total population vs population covered by 1 dose of COVID-19 vaccine as of 31 Dec 2023 | `4_plotting/supp_plot3_pop_vaxcoverage.R` |
| Supplementary Table 3a-d: Expenditure outcomes for the pricing scenarios with real vaccination coverage | `3_tables/supptable3a-d_base_realcoverage.R` |
| Supplementary Table 4a-b: Varying $\alpha$ in tiered pricing with real vaccination coverage | `3_tables/supptable4ab_alpha_realcoverage.R` |
| Supplementary Table 5a-d: Outcome ranges from PSA for the pricing scenarios with real vaccination coverage  | `3_tables/supptable5a-d_supp_psa_realcoverage.R` |

All analyses correspond to the methods and results reported in the manuscript.

## 6. Analytical Flow

### 6.1 Base Analysis
 
In our base analysis, we applied the assumption of 100% vaccination coverage, meaning that the total number of doses procured by a country equals its population size. For each pricing scenario considered, we calculated country-level expenditure, which is the total number of doses procured by a country multiplied by vaccine price per dose. 

To assess economic gains, for pooled procurement scenarios (uniform and tiered pricing), we calculated country-level savings from expenditure under bilateral procurement. 

To assess equity gains, we defined vaccine price burden as the share of GDP per capita or CHE per capita incurred by one dose. We then calculated the relative concentration index of vaccine price burden with respect to GDP per capita and CHE per capita accordingly.

### 6.2 Sensitivity Analyses

#### Varying $\alpha$
$\alpha$ is the price adjustment factor applied for tiered pricing. In this section, we explored how outcomes shift as we varied $\alpha$ between 0, 10 and 20. For the detailed rationale and formula of tiered pricing, refer to Supplementary Material. 

#### Accounting for uncertainty in vaccine price per dose using probabilistic sensitivity analysis (PSA)
To capture the full uncertainty of vaccine price per dose and its effect on calculated outcomes, we conducted PSA by resampling from the empirical distribution of COVID-19 vaccine prices obtained from the MI4A database extraction and calculated outcomes based on resampled values. For the detailed step-by-step of our PSA, refer to Supplementary Material.

#### Vaccination coverage
As our base analysis assumes 100% vaccination coverage, we also considered how our outcomes would change when WHO COVID-19 vaccination coverage data was applied. We present outcomes calculated with this assumption for base analysis and the two preceding sensitivity analyses sections.

## 7. Reproducibility notes

- The code was developed using R version [4.5.3] in RStudio version [2026.1.2.418] on Windows.
- Package dependencies are documented in `renv.lock`.
- Random seeds were specified for analyses involving stochastic procedures, where applicable.
- The scripts were tested in [describe testing environment].
- [Describe any known limitations affecting reproducibility.]

## 8. Citation

If you use or adapt the code, please cite:

Afifah Rahman-Shepherd, Dorothy Boyle, Benjamin Coghlan, Mutiara Indriani, Bryan Mercurio, Jeremy Turner-Gorospe, Nakorn Premsri, Justin Wong, Kiesha Prem. Pricing COVID-19 Vaccines in Southeast Asia: retrospective modelling to inform prospective negotiations. *BMJ Public Health*. [Year];[Volume]:[Article number]. DOI: [DOI].

## 9. License

This repository is distributed under the GNU General Public license. See the `LICENSE` file for details.

## 10. Contact

For questions regarding the code or analyses, please contact:

[Contact name or research group]  
[Institution]  
[Professional email address]
