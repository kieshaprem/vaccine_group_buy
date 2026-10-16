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
project/
├── README.md
├── .gitignore
├── vaccine_group_buy.Rproj
├── renv.lock
├── data/
│   └── README.md

└── output/
    ├── intermediate/
        └── hic_psa.rdata
        └── lmic_psa.rdata
        └── main_data.rdata
        └── psa_countrylevel_base.rdata
        └── psa_countrylevel_real.rdata
        └── psa_outcomes_base.rdata
        └── psa_outcomes_real.rdata
        └── psa_price_base.rdata
        └── umic_psa.rdata
    ├── plots/
        └── 2ag_full.png
        └── 3ap_full.png
        └── supp1_gdp_oops_che_all.png
        └── supp2_mi4a_incomegrp_procurement.png
        └── supp3_pop_covered.png
    ├── tables/
        └── compiled/
            └── alpha_full.xlsx
            └── alpha_real.xlsx
            └── base_analysis_full.xlsx
            └── base_analysis_real.xlsx
            └── psa_analysis_full.xlsx
            └── psa_analysis_real.xlsx
        └── specific/
            └── adj_chepc_full_alpha_max.rdata
            └── adj_chepc_full_alpha_min.rdata
            └── adj_chepc_full_base.rdata
            └── adj_chepc_real_alpha_max.rdata
            └── adj_chepc_real_alpha_min.rdata
            └── adj_chepc_real_base.rdata
            └── adj_gdppc_full_alpha_max.rdata
            └── adj_gdppc_full_alpha_min.rdata
            └── adj_gdppc_full_base.rdata
            └── adj_gdppc_real_alpha_max.rdata
            └── adj_gdppc_real_alpha_min.rdata
            └── adj_gdppc_real_base.rdata
            └── bilateral_full_base.rdata
            └── bilateral_real_base.rdata
            └── uniform_full_base.rdata
            └── uniform_real_base.rdata
├── scripts/
    └── plotting/
        └── general_helper_plotting_functions.R
        └── mainpaper_plot2.R
        └── mainpaper_plot2a-c.R
        └── mainpaper_plot2d-f.R
        └── mainpaper_plot2g.R
        └── mainpaper_plot2h.R
        └── mainpaper_plot2_helper.R
        └── mainpaper_plot3.R
        └── mainpaper_plot3a-d_i-l.R
        └── mainpaper_plot3e-h_m-p.R
        └── mainpaper_plot3_helper.R
        └── run_all_plots.R
        └── supp1_gdppc_oops_plot.R
        └── supp2_helper_get_box_plot.R
        └── supp2_mi4a_boxplots.R
        └── supp3_pop_vaxcoverage.R
│   └── 0_get_main_data.R
    └── 0_load_mi4a_data.R
    └── 0_process_calculations.R
    └── 1_0_psa_main_script.R
    └── 1_1_psa_price.R
    └── 1_2_psa_outcome.R
    └── 2_0_tables_main_script.R
    └── 2_1_table_2ab.R
    └── 2_2_table_supp_alpha.R
    └── 2_3_table_supp_psa.R
    └── 2_4_table_supp_base_realcoverage.R
    └── 2_5_table_supp_alpha_realcoverage.R
    └── 2_6_table_supp_psa_realcoverage.R
    └── helper_calculation_function.R
    └── helper_examine_psa_results.R
    └── helper_format_table_all.R
    └── helper_format_table_scenario.R
    └── helper_get_prices.R
    └── helper_run_scenarios.R
```

The R scripts perform the following tasks:

| Script | Description |
|---|---|
| `scripts/0_get_main_data.R` | Data cleaning, variable recoding, and preparation of the analytical dataset |
| `02_descriptive_analysis.R` | Descriptive statistics and summary of study characteristics |
| `03_statistical_analysis.R` | Statistical analyses reported in the manuscript |
| `04_generate_figures.R` | Generation of figures presented in the manuscript |
| `05_generate_tables.R` | Generation of tables presented in the manuscript |

## 2. Software requirements

All analyses were conducted using **R version [X.X.X]**.

The analyses require the following R packages:

| Package | Purpose |
|---|---|
| `dplyr` | Data manipulation |
| `tidyr` | Data transformation |
| `ggplot2` | Data visualisation |
| `[package]` | [Purpose] |
| `[package]` | [Purpose] |

The package versions used in the analyses are documented in `renv.lock`.

To restore the package environment, run:

```r
install.packages("renv")
renv::restore()
```

## 3. Data availability

[Choose and customise the appropriate statement; delete the unused options.]

**Option A — Data are publicly available**

The datasets used in this study are publicly available from [repository or source] at [URL/DOI].

## 4. Reproducing the analyses

To reproduce the analyses:

1. Download or clone this repository.
2. Open `[project_name].Rproj` in RStudio.
3. Restore the R package environment using `renv::restore()`.
4. Obtain the required datasets as described in Section 3.
5. Place the datasets in the `data/` directory, using the filenames specified in the scripts.
6. Execute the R scripts in the following order:
   - `01_data_preparation.R`
   - `02_descriptive_analysis.R`
   - `03_statistical_analysis.R`
   - `04_generate_figures.R`
   - `05_generate_tables.R`
7. Review the generated outputs in the `outputs/` directory.

All file paths are relative to the project directory.

**Note:** [Specify whether scripts must be executed sequentially, whether intermediate files are generated, and whether additional manual steps are required.]

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
