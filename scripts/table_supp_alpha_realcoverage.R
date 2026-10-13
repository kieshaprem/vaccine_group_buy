source('scripts/1_helper_calculation_function.R')
source('scripts/1_helper_examine_psa_results.R')
source('scripts/1_table_formatting_functions.R')
source('scripts/1_helper_run_scenarios.R')

options(scipen=999)


#################################################################################
load('output/final_output/bilateral_real_base.rdata')
bilateral_output <- bilateral_output$calc_output

run_scenario(coverage = 'real',mode = 'alpha_min')
run_scenario(coverage = 'real',mode = 'alpha_max')

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
