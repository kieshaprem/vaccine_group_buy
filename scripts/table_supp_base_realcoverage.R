source('scripts/1_helper_calculation_function.R')
source('scripts/1_helper_examine_psa_results.R')
source('scripts/1_table_formatting_functions.R')
source('scripts/1_helper_run_scenarios.R')

options(scipen=999)

################################################################################
run_scenario(coverage = 'real',mode = 'base')
#run_scenario(coverage = 'real',mode = 'psa')

load('output/final_output/bilateral_real_base.rdata')
bilateral <- bilateral_output$calc_output
#load('output/final_output/bilateral_real_psa.rdata')

load('output/final_output/uniform_real_base.rdata')
uniform <- uniform_output$calc_output
#load('output/final_output/uniform_real_psa.rdata')

load('output/final_output/adj_gdppc_real_base.rdata')
adj_gdppc <- adj_gdppc_output$calc_output
#load('output/final_output/adj_gdppc_real_psa.rdata')

load('output/final_output/adj_chepc_real_base.rdata')
adj_chepc <- adj_chepc_output$calc_output
#load('output/final_output/adj_chepc_real_psa.rdata')

compile_base_results(coverage = 'real')

