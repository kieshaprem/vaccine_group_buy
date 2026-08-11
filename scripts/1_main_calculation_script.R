source('scripts/1_calculation_function.R')
source('scripts/1_table_formatting_functions.R')
source('scripts/1_examine_psa_results.R')

options(scipen=999)

run_scenario(coverage = 'full',mode = 'base')
#run_scenario(coverage = 'full',mode = 'psa')

load('output/final_output/bilateral_full_base.rdata')
bilateral <- bilateral_output$calc_output
#load('output/final_output/bilateral_full_psa.rdata')

load('output/final_output/uniform_full_base.rdata')
uniform <- uniform_output$calc_output
#load('output/final_output/uniform_full_psa.rdata')

load('output/final_output/adj_gdppc_full_base.rdata')
adj_gdppc <- adj_gdppc_output$calc_output
#load('output/final_output/adj_gdppc_full_psa.rdata')

load('output/final_output/adj_chepc_full_base.rdata')
adj_chepc <- adj_chepc_output$calc_output
#load('output/final_output/adj_chepc_full_psa.rdata')

compile_base_results(coverage = 'full')

# mode = 'psa'
# compile_sheet('full')

mode = 'output_psa'
compile_psa_results('full')

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

mode = 'output_psa'
coverage = 'real'
compile_psa_results('real')

#################################################################################
load('output/final_output/bilateral_full_base.rdata')
bilateral_output <- bilateral_output$calc_output

run_scenario(coverage = 'full',mode = 'alpha_min')
run_scenario(coverage = 'full',mode = 'alpha_max')

load('output/final_output/adj_gdppc_full_alpha_min.rdata')
adj_gdppc_min <- adj_gdppc_output$calc_output
load('output/final_output/adj_gdppc_full_alpha_max.rdata')
adj_gdppc_max <- adj_gdppc_output$calc_output
load('output/final_output/adj_gdppc_full_base.rdata')
adj_gdppc <- adj_gdppc_output$calc_output

load('output/final_output/adj_chepc_full_alpha_min.rdata')
adj_chepc_min <- adj_chepc_output$calc_output
load('output/final_output/adj_chepc_full_alpha_max.rdata')
adj_chepc_max <- adj_chepc_output$calc_output
load('output/final_output/adj_chepc_full_base.rdata')
adj_chepc <- adj_chepc_output$calc_output

compile_sheet_alpha('full')

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
