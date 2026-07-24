source('scripts/plotting/general_helper_plotting_functions.R')
source('scripts/0_process_calculations.R')
#source('scripts/plotting/plot_2a-f.R')
library(countrycode)

load('output/final_output/bilateral_full_base.rdata')
bilateral <- bilateral_output$calc_output

load('output/final_output/uniform_full_base.rdata')
uniform <- uniform_output$calc_output

load('output/final_output/adj_gdppc_full_base.rdata')
adj_gdppc <- adj_gdppc_output$calc_output

load('output/final_output/adj_chepc_full_base.rdata')
adj_chepc <- adj_chepc_output$calc_output

uniform <- get_savings_per_dose(alternate_pricing_df = uniform, bilateral_pricing_df = bilateral)
adj_gdppc <- get_savings_per_dose(alternate_pricing_df = adj_gdppc, bilateral_pricing_df = bilateral)
adj_chepc <- get_savings_per_dose(alternate_pricing_df = adj_chepc, bilateral_pricing_df = bilateral)

#2a-f for comparing GDP per capita and CHE per capita
source('scripts/plotting/plot_2a-f_compare_gdp_chepc.R')
plot_compare_gdp_chepc(coverage = 'full')
plot_compare_gdp_gov_exp(coverage = 'real')

load('output/final_output/adj_gdppc_full_alpha_min.rdata')
adj_gdppc_min <- adj_gdppc_output$calc_output
load('output/final_output/adj_gdppc_full_alpha_max.rdata')
adj_gdppc_max <- adj_gdppc_output$calc_output
#load('output/final_output/adj_gdppc_full_base.rdata')
#adj_gdppc <- adj_gdppc_output$calc_output

load('output/final_output/adj_chepc_full_alpha_min.rdata')
adj_chepc_min <- adj_chepc_output$calc_output
load('output/final_output/adj_chepc_full_alpha_max.rdata')
adj_chepc_max <- adj_chepc_output$calc_output
#load('output/final_output/adj_chepc_full_base.rdata')
#adj_chepc <- adj_chepc_output$calc_output



source('scripts/1_table_formatting_functions.R')
source('scripts/1_examine_psa_results.R')
source('scripts/1_calculation_function.R')

country_level_psa_summary <- get_country_level_psa_summary_all('full')
bilateral_country_level <- country_level_psa_summary$bilateral
relevant_rows <- which(sapply(rownames(bilateral_country_level),
               function(x){substr(x,start = 5,stop = nchar(x))})=='percent')
percent_share_df <- bilateral_country_level[relevant_rows,]

plot_compare_gdp_chepc_2_4('full')



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
get_plot_3af(weighting_mechanism = 'gdp_adj', affordability_measure = 'perdose_gdppc_percent')
get_plot_3af(weighting_mechanism = 'chepc_adj', affordability_measure = 'perdose_gdppc_percent')
get_plot_3af(weighting_mechanism = 'gdp_adj', affordability_measure = 'perdose_chepc_percent')
get_plot_3af(weighting_mechanism = 'chepc_adj', affordability_measure = 'perdose_chepc_percent')

get_plot_3af_bilateral(affordability_measure = 'perdose_gdppc_percent')
get_plot_3af_bilateral(affordability_measure = 'perdose_chepc_percent')

get_plot_3af_4cols('perdose_gdppc_percent','full')
get_plot_3af_4cols('perdose_chepc_percent','full')

get_plot_3af_fullpage('full')
