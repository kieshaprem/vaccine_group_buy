source('scripts/0_helper_functions/helper_calculation_function.R')
source('scripts/0_helper_functions/helper_examine_psa_results.R')
source('scripts/0_helper_functions/helper_format_table_all.R')
source('scripts/0_helper_functions/helper_format_table_scenario.R')
source('scripts/0_helper_functions/helper_run_scenarios.R')

options(scipen=999)

compile_psa_results('full')
