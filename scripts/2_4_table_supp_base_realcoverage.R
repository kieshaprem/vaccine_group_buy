source('scripts/helper_calculation_function.R')
source('scripts/helper_examine_psa_results.R')
source('scripts/helper_format_table_all.R')
source('scripts/helper_format_table_scenario.R')
source('scripts/helper_run_scenarios.R')

options(scipen=999)

################################################################################
run_scenario(coverage = 'real',mode = 'base')

load('output/tables/specific/bilateral_real_base.rdata')
bilateral <- bilateral_output$calc_output

load('output/tables/specific/uniform_real_base.rdata')
uniform <- uniform_output$calc_output

load('output/tables/specific/adj_gdppc_real_base.rdata')
adj_gdppc <- adj_gdppc_output$calc_output

load('output/tables/specific/adj_chepc_real_base.rdata')
adj_chepc <- adj_chepc_output$calc_output

compile_base_results(coverage = 'real')

