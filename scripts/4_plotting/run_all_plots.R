source('scripts/4_plotting/helper_plotting.R')
source('scripts/0_helper_functions/helper_process_calculations.R')
source('scripts/0_helper_functions/helper_examine_psa_results.R')
library(countrycode)

load('output/tables/specific/bilateral_full_base.rdata')
bilateral <- bilateral_output$calc_output

load('output/tables/specific/uniform_full_base.rdata')
uniform <- uniform_output$calc_output

load('output/tables/specific/adj_gdppc_full_base.rdata')
adj_gdppc <- adj_gdppc_output$calc_output

load('output/tables/specific/adj_chepc_full_base.rdata')
adj_chepc <- adj_chepc_output$calc_output

# ------------------------------------------------------------------------------
# Main Paper Figure 2a-h for comparing uniform, tiered pricing adjusted by
# GDP per capita and CHE per capita in terms of expenditure and savings from 
# bilateral mechanism at the country and regional level, accounting for 
# uncertainty in vaccine price per dose and the parameter alpha which governs
# the steepness of the tiered pricing gradient
# ------------------------------------------------------------------------------
source('scripts/4_plotting/mainpaper_plot2.R')

# ------------------------------------------------------------------------------
# Main Paper Figure 3a-p for comparing bilateral, uniform, tiered pricing 
# adjusted by GDP per capita and CHE per capita in terms of affordability
# (financial burden) of vaccine price per dose as % of GDP per capita (3a-h)
# and % of CHE per capita (3i-p)
# ------------------------------------------------------------------------------
source('scripts/4_plotting/mainpaper_plot3.R')

# ------------------------------------------------------------------------------
# Supplementary Figure 1 for comparing health out-of-pocket spending per capita,
# CHE per capita and GDP per capita in all countries, highlighting the vast
# differences between ASEAN countries
# ------------------------------------------------------------------------------
source('scripts/4_plotting/supp_plot1_gdppc_oops_plot.R')

# ------------------------------------------------------------------------------
# Supplementary Figure 2 for the distribution of COVID-19 vaccine price per dose
# across procurement mechanisms in WHO WPRO and SEARO countries from MI4A 
# database, stratified by country income group
# ------------------------------------------------------------------------------
source('scripts/4_plotting/supp_plot2_mi4a_boxplots.R')

# ------------------------------------------------------------------------------
# Supplementary Figure 3 for comparing total population vs population covered by
# at least 1 dose of COVID-19 vaccine as of 31 Dec 2023 for each country
# ------------------------------------------------------------------------------
source('scripts/4_plotting/supp_plot3_pop_vaxcoverage.R')
