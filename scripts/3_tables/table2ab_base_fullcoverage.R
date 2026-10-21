source('scripts/0_helper_functions/helper_calculation_function.R')
source('scripts/0_helper_functions/helper_examine_psa_results.R')
source('scripts/0_helper_functions/helper_format_table_all.R')
source('scripts/0_helper_functions/helper_format_table_scenario.R')
source('scripts/0_helper_functions/helper_run_scenarios.R')

options(scipen=999)

# ------------------------------------------------------------------------------
# Table 2a & b: Expenditure of COVID-19 vaccines under bilateral, fixed pricing,
# and tiered pricing scenarios, assuming 100% vaccine coverage
# ------------------------------------------------------------------------------
run_scenario(coverage = 'full',mode = 'base')
# Outputs in output/final_output/:
# bilateral_full_base.rdata
# uniform_full_base.rdata
# adj_gdppc_full_base.rdata
# adj_chepc_full_base.rdata

load('output/tables/specific/bilateral_full_base.rdata')
bilateral <- bilateral_output$calc_output

load('output/tables/specific/uniform_full_base.rdata')
uniform <- uniform_output$calc_output

load('output/tables/specific/adj_gdppc_full_base.rdata')
adj_gdppc <- adj_gdppc_output$calc_output

load('output/tables/specific/adj_chepc_full_base.rdata')
adj_chepc <- adj_chepc_output$calc_output

compile_base_results(coverage = 'full')
# produces output/final_output/base_analysis_full.xlsx



