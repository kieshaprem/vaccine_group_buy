source('scripts/plotting/general_helper_plotting_functions.R')
source('scripts/0_process_calculations.R')
source('scripts/helper_examine_psa_results.R')
library(countrycode)

load('output/tables/specific/bilateral_full_base.rdata')
bilateral <- bilateral_output$calc_output

load('output/tables/specific/uniform_full_base.rdata')
uniform <- uniform_output$calc_output

load('output/tables/specific/adj_gdppc_full_base.rdata')
adj_gdppc <- adj_gdppc_output$calc_output

load('output/tables/specific/adj_chepc_full_base.rdata')
adj_chepc <- adj_chepc_output$calc_output

uniform <- get_savings_per_dose(alternate_pricing_df = uniform, 
                                bilateral_pricing_df = bilateral)
adj_gdppc <- get_savings_per_dose(alternate_pricing_df = adj_gdppc, 
                                  bilateral_pricing_df = bilateral)
adj_chepc <- get_savings_per_dose(alternate_pricing_df = adj_chepc, 
                                  bilateral_pricing_df = bilateral)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Main Paper Figure 2a-h for comparing GDP per capita and CHE per capita 
source('scripts/plotting/plot_2a-f_compare_gdp_chepc.R')
# ------------------------------------------------------------------------------
# For 2g: Comparing % share and its uncertainty (derived from uncertainty in 
# vaccine price per dose) for each country derived from PSA. For tiered pricing,
# as % share for each country remains the same in PSA, we display the range 
# derived from varying alpha

# Get df for uncertainty in country-level outcomes
country_level_psa_summary <- get_country_level_psa_summary_all('full')

# bilateral mechanism
bilateral_country_level <- country_level_psa_summary$bilateral
relevant_rows <- which(sapply(rownames(bilateral_country_level),
                              function(x){substr(x,start = 5,
                                                 stop = nchar(x))})=='percent')
percent_share_df <- bilateral_country_level[relevant_rows,c('2.5%','97.5%')]
# For median point, use values from base analysis rather than PSA since median
# % share for bilateral mechanism under base analysis is different from PSA,
# unlike other pricing mechanisms
percent_share_df[,'50%'] <- bilateral$percent_share
percent_share_df <- percent_share_df[,c('2.5%','50%','97.5%')]

# Ranges when alpha is varied
##Tiered pricing by GDP per capita
### alpha = 0
load('output/tables/specific/adj_gdppc_full_alpha_min.rdata')
adj_gdppc_min <- adj_gdppc_output$calc_output
### alpha = 20
load('output/tables/specific/adj_gdppc_full_alpha_max.rdata')
adj_gdppc_max <- adj_gdppc_output$calc_output

## Tiered pricing by CHE per capita
### alpha = 0
load('output/tables/specific/adj_chepc_full_alpha_min.rdata')
adj_chepc_min <- adj_chepc_output$calc_output
### alpha = 20
load('output/tables/specific/adj_chepc_full_alpha_max.rdata')
adj_chepc_max <- adj_chepc_output$calc_output

# ------------------------------------------------------------------------------
# For 2h: Comparing the region's total expenditure across all pricing mechanisms
# accounting for uncertainty in vaccine price per dose

source('scripts/helper_format_table_scenario.R')
source('scripts/helper_calculation_function.R')

# Get df for summarised regional outcomes
outcomes_summary <- get_psa_summary('full')

# Convert df to a plot friendly format
total_exp_df <- data.frame(array(dim=c(4,3)))
colnames(total_exp_df) <- c('lower','median','upper')
rownames(total_exp_df) <- c('Bilateral','Fixed','GDPPC-\nadjusted',
                            'CHEPC-\nadjusted')
total_exp_df[1,c(1,3)] <- outcomes_summary['bilateral_total',c(1,3)]
total_exp_df[2,c(1,3)] <- outcomes_summary['uniform_total',c(1,3)]
total_exp_df[3,c(1,3)] <- outcomes_summary['adj_gdppc_total',c(1,3)]
total_exp_df[4,c(1,3)] <- outcomes_summary['adj_chepc_total',c(1,3)]
total_exp_df[1,2] <- sum(bilateral$total_perdose)
total_exp_df[2,2] <- sum(uniform$total_perdose)
total_exp_df[3,2] <- sum(adj_gdppc$total_perdose)
total_exp_df[4,2] <- sum(adj_chepc$total_perdose)

# ------------------------------------------------------------------------------
# Draw the panelled plot
plot_compare_gdp_chepc_3rows('full')
# ---------------------------+---------------------------------------------------


# 3a-f using GDPPC price adjustment
bilateral <- bilateral[order(bilateral$gdppc),]
uniform <- uniform[order(uniform$gdppc),]
adj_gdppc <- adj_gdppc[order(adj_gdppc$gdppc),]
adj_chepc <- adj_chepc[order(adj_chepc$gdppc),]

bilateral <- add_affordability_percent(bilateral,'gdppc')
uniform <- add_affordability_percent(uniform,'gdppc')
adj_gdppc <- add_affordability_percent(adj_gdppc,'gdppc')
adj_chepc <- add_affordability_percent(adj_chepc,'gdppc')

bilateral <- add_affordability_percent(bilateral,'chepc')
uniform <- add_affordability_percent(uniform,'chepc')
adj_gdppc <- add_affordability_percent(adj_gdppc,'chepc')
adj_chepc <- add_affordability_percent(adj_chepc,'chepc')
source('scripts/plotting/4_get_splines.R')

source('scripts/plotting/plot_3a-f.R')
#get_plot_3af(weighting_mechanism = 'gdp_adj', affordability_measure = 'perdose_gdppc_percent')
#get_plot_3af(weighting_mechanism = 'chepc_adj', affordability_measure = 'perdose_gdppc_percent')
#get_plot_3af(weighting_mechanism = 'gdp_adj', affordability_measure = 'perdose_chepc_percent')
#get_plot_3af(weighting_mechanism = 'chepc_adj', affordability_measure = 'perdose_chepc_percent')

#get_plot_3af_bilateral(affordability_measure = 'perdose_gdppc_percent')
#get_plot_3af_bilateral(affordability_measure = 'perdose_chepc_percent')

#get_plot_3af_4cols('perdose_gdppc_percent','full')
#get_plot_3af_4cols('perdose_chepc_percent','full')

get_plot_3af_fullpage('full')

source('scripts/plotting/waterfall_plot_only.R')
waterfall_plot_only(mechanism = 'bilateral', dataset = bilateral,affordability_metric = 'perdose_gdppc_percent')
