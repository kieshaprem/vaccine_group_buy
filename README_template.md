# [Manuscript Title]

## Overview

This repository contains the R code used to conduct the analyses presented in the manuscript:

**Title:** Pricing COVID-19 Vaccines in Southeast Asia: retrospective modelling to inform prospective negotiations  
**Authors:** [Author names]  
**Journal:** BMJ Public Health  
**DOI:** [Manuscript DOI, if available]

The study aimed to [briefly describe the study objectives].

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

The datasets used in this study are publicly available from the [MI4A](https://www.who.int/publications/m/item/2026-mi4a-public-vaccine-purchase-dataset) and [World Bank](https://data.worldbank.org) database.

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

## 5. Correspondence with manuscript results

The following table identifies the scripts used to generate the results presented in the manuscript.

| Manuscript component | R script |
|---|---|
| Table 1: [Title] | `02_descriptive_analysis.R` |
| Table 2: [Title] | `03_statistical_analysis.R` |
| Figure 1: [Title] | `04_generate_figures.R` |
| Supplementary Table S1: [Title] | `[script_name].R` |
| Supplementary Figure S1: [Title] | `[script_name].R` |

All analyses correspond to the methods and results reported in the manuscript.

## 6. Analytical considerations

The following methodological decisions were implemented in the R scripts:

- **Study population:** [Describe relevant inclusion and exclusion criteria.]
- **Variable definitions:** [Describe important derived variables or recoding decisions.]
- **Missing data:** [Describe how missing observations were handled.]
- **Statistical methods:** [Summarise the main statistical methods.]
- **Sensitivity analyses:** [Describe additional analyses, if applicable.]

Further methodological details are provided in the manuscript.

## 7. Reproducibility notes

- The code was developed using R version [X.X.X].
- Package dependencies are documented in `renv.lock`.
- Random seeds were specified for analyses involving stochastic procedures, where applicable.
- The scripts were tested in [describe testing environment].
- [Describe any known limitations affecting reproducibility.]

## 8. Citation

If you use or adapt the code, please cite:

[Authors]. [Manuscript title]. *BMJ Public Health*. [Year];[Volume]:[Article number]. DOI: [DOI].

For the archived code repository, please cite:

[Authors]. [Repository title]. [Repository platform]. [Year]. DOI: [Repository DOI].

## 9. License

This repository is distributed under the [license name] license. See the `LICENSE` file for details.

## 10. Contact

For questions regarding the code or analyses, please contact:

[Contact name or research group]  
[Institution]  
[Professional email address]
