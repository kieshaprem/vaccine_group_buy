source('scripts/helper_calculation_function.R')
source('scripts/helper_examine_psa_results.R')
source('scripts/helper_format_table_all.R')
source('scripts/helper_format_table_scenario.R')
source('scripts/helper_run_scenarios.R')

options(scipen=999)

# ------------------------------------------------------------------------------
# Supplementary Table 4a & b: Tiered pricing adjusted by GDP per capita or CHE 
# per capita with alpha modified between 0,10 and 20, applied to base analysis 
# conditions, applying real coverage and Pmin = $4.00
# ------------------------------------------------------------------------------
load('output/final_output/bilateral_real_base.rdata')
bilateral_output <- bilateral_output$calc_output

run_scenario(coverage = 'real',mode = 'alpha_min')
# Outputs in output/final_output/
# adj_gdppc_real_alpha_min.rdata
# adj_chepc_real_alpha_min.rdata
run_scenario(coverage = 'real',mode = 'alpha_max')
# Outputs in output/final_output/
# adj_gdppc_real_alpha_max.rdata
# adj_chepc_real_alpha_max.rdata

load('output/final_output/adj_gdppc_real_alpha_min.rdata')
adj_gdppc_min <- adj_gdppc_output$calc_output
load('output/final_output/adj_gdppc_real_alpha_max.rdata')
adj_gdppc_max <- adj_gdppc_output$calc_output
load('output/final_output/adj_gdppc_real_base.rdata')
adj_gdppc <- adj_gdppc_output$calc_output

load('output/final_output/adj_chepc_real_alpha_min.rdata')
adj_chepc_min <- adj_chepc_output$calc_output
load('output/final_output/adj_chepc_real_alpha_max.rdata')
adj_chepc_max <- adj_chepc_output$calc_output
load('output/final_output/adj_chepc_real_base.rdata')
adj_chepc <- adj_chepc_output$calc_output

compile_sheet_alpha('real')
# produces output/final_output/base_alpha_real.xlsx
