source('scripts/calculation_function.R')
source('scripts/table_formatting_functions.R')

options(scipen=999)

compile_sheet <- function(coverage){
  bilateral_sheet <- compile_base_sensitivity(df_median = bilateral$calc_output, df_min = bilateral_output$calc_output_min,
                                              df_max = bilateral_output$calc_output_max)
  uniform_sheet <- compile_base_sensitivity(df_median = uniform$calc_output, df_min = uniform_output$calc_output_min,
                                            df_max = uniform_output$calc_output_max,
                                            bilateral_median = bilateral$calc_output,
                                            bilateral_min = bilateral_output$calc_output_min,
                                            bilateral_max = bilateral_output$calc_output_max)
  adj_gdppc_sheet <- compile_base_sensitivity(df_median = adj_gdppc$calc_output, df_min = adj_gdppc_output$calc_output_min,
                                              df_max = adj_gdppc_output$calc_output_max,
                                              bilateral_median = bilateral$calc_output,
                                              bilateral_min = bilateral_output$calc_output_min,
                                              bilateral_max = bilateral_output$calc_output_max)
  adj_chepc_sheet <- compile_base_sensitivity(df_median = adj_chepc$calc_output, df_min = adj_chepc_output$calc_output_min,
                                              df_max = adj_chepc_output$calc_output_max,
                                              bilateral_median = bilateral$calc_output,
                                              bilateral_min = bilateral_output$calc_output_min,
                                              bilateral_max = bilateral_output$calc_output_max)
  
  write_xlsx(list('bilateral'=bilateral_sheet,'uniform'=uniform_sheet,'adj_gdppc'=adj_gdppc_sheet,
                  'adj_chepc'=adj_chepc_sheet),path = paste0('output/final_output/',coverage,'_base_psa_wsavings.xlsx'))
}


run_scenario(coverage = 'full',mode = 'base')
run_scenario(coverage = 'full',mode = 'psa')

load('output/final_output/bilateral_full_base.rdata')
bilateral <- bilateral_output
load('output/final_output/bilateral_full_psa.rdata')

load('output/final_output/uniform_full_base.rdata')
uniform <- uniform_output
load('output/final_output/uniform_full_psa.rdata')

load('output/final_output/adj_gdppc_full_base.rdata')
adj_gdppc <- adj_gdppc_output
load('output/final_output/adj_gdppc_full_psa.rdata')

load('output/final_output/adj_chepc_full_base.rdata')
adj_chepc <- adj_chepc_output
load('output/final_output/adj_chepc_full_psa.rdata')

compile_sheet('full')

#################################################################################
run_scenario(coverage = 'real',mode = 'base')
run_scenario(coverage = 'real',mode = 'psa')

load('output/final_output/bilateral_real_base.rdata')
bilateral <- bilateral_output
load('output/final_output/bilateral_real_psa.rdata')

load('output/final_output/uniform_real_base.rdata')
uniform <- uniform_output
load('output/final_output/uniform_real_psa.rdata')

load('output/final_output/adj_gdppc_real_base.rdata')
adj_gdppc <- adj_gdppc_output
load('output/final_output/adj_gdppc_real_psa.rdata')

load('output/final_output/adj_chepc_real_base.rdata')
adj_chepc <- adj_chepc_output
load('output/final_output/adj_chepc_real_psa.rdata')

compile_sheet('real')
