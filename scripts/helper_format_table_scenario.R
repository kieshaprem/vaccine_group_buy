library(countrycode)
source('scripts/helper_format_table_all.R')
source('scripts/helper_examine_psa_results.R')
# ------------------------------------------------------------------------------
# Function to compile results for base analysis, producing Table 2a-b 
# in the Main Paper
# ------------------------------------------------------------------------------
compile_base_results <- function(coverage){
  bilateral_sheet <- get_pretty_excel_table(bilateral,
                                            mechanism = 'bilateral')
  uniform_sheet <- get_pretty_excel_table(uniform,bilateral_dataset = bilateral,
                                          mechanism = 'uniform')
  adj_gdppc_sheet <- get_pretty_excel_table(adj_gdppc,bilateral_dataset = bilateral,
                                            mechanism = 'adj_gdppc')
  adj_chepc_sheet <- get_pretty_excel_table(adj_chepc,bilateral_dataset = bilateral,
                                            mechanism = 'adj_chepc')
  write_xlsx(list('bilateral'=bilateral_sheet, 'uniform' = uniform_sheet,
                  'adj_gdppc'=adj_gdppc_sheet, 'adj_chepc' = adj_chepc_sheet),
             path = paste0('output/tables/compiled/base_analysis_',coverage,'.xlsx'))
}

# ------------------------------------------------------------------------------
# Function to compile results for PSA analysis for all pricing mechanisms,  
# producing Table in the Supplement
# ------------------------------------------------------------------------------
compile_psa_results <- function(coverage){
  psa_summary <- get_psa_summary(coverage = coverage)
  country_level_summary <- get_country_level_psa_summary_all(coverage = coverage)
  bilateral_sheet <- table_mapping(mechanism = 'bilateral', psa_summary = psa_summary,
                                   country_level_summary = country_level_summary,
                                   coverage = coverage)
  uniform_sheet <- table_mapping(mechanism = 'uniform', psa_summary = psa_summary,
                                 country_level_summary = country_level_summary,
                                 coverage = coverage, bilateral_min = bilateral_sheet$first_q,
                                 bilateral_median = bilateral_sheet$mid_q,
                                 bilateral_max = bilateral_sheet$third_q)
  adj_gdppc_sheet <- table_mapping(mechanism = 'adj_gdppc', psa_summary = psa_summary,
                                   country_level_summary = country_level_summary,
                                   coverage = coverage, bilateral_min = bilateral_sheet$first_q,
                                   bilateral_median = bilateral_sheet$mid_q,
                                   bilateral_max = bilateral_sheet$third_q)
  adj_chepc_sheet <- table_mapping(mechanism = 'adj_chepc', psa_summary = psa_summary,
                                   country_level_summary = country_level_summary,
                                   coverage = coverage, bilateral_min = bilateral_sheet$first_q,
                                   bilateral_median = bilateral_sheet$mid_q,
                                   bilateral_max = bilateral_sheet$third_q)
  write_xlsx(list('bilateral'=bilateral_sheet$ranges_sheet,'uniform'=uniform_sheet$ranges_sheet,
                  'adj_gdppc'=adj_gdppc_sheet$ranges_sheet,'adj_chepc'=adj_chepc_sheet$ranges_sheet),
             path = paste0('output/tables/compiled/psa_analysis_',coverage,'.xlsx'))
  
}

# Helper function for compile_psa_results
# Returns median and 95% UI formatted for a particular pricing mechanism
table_mapping <- function(mechanism, psa_summary, country_level_summary, 
                          coverage,bilateral_min = NA, bilateral_median = NA,
                          bilateral_max = NA){
  first_q <- prices_calc(mechanism = mechanism, coverage = coverage, 
                         comparator = NA, psa_summary = psa_summary, 
                         psa_summary_index = '2.5%', mode = 'output_psa',
                         psa_country_level_summary = country_level_summary)$calc_output
  
  mid_q <- prices_calc(mechanism = mechanism, coverage = coverage, 
                       comparator = NA, psa_summary = psa_summary, 
                       psa_summary_index = '50%', mode = 'output_psa',
                       psa_country_level_summary = country_level_summary)$calc_output
  
  third_q <- prices_calc(mechanism = mechanism, coverage = coverage, 
                         comparator = NA, psa_summary = psa_summary, 
                         psa_summary_index = '97.5%', mode = 'output_psa',
                         psa_country_level_summary = country_level_summary)$calc_output
  sheet <- compile_base_sensitivity(df_median = mid_q, 
                                    df_min = first_q, 
                                    df_max = third_q,
                                    psa_summary = psa_summary, 
                                    name = mechanism,
                                    bilateral_median = bilateral_median,
                                    bilateral_min = bilateral_min, 
                                    bilateral_max = bilateral_max)
  return(list('ranges_sheet'=sheet,'first_q'=first_q,'mid_q'=mid_q,'third_q'=third_q))
}

# Helper function for table_mapping
# Given 3 dfs for median and 95% UI, returns formatted df 
compile_base_sensitivity <- function(df_median, df_min, df_max, name,
                                     bilateral_median=NA, bilateral_min=NA, 
                                     bilateral_max=NA, psa_summary = NA){
  df_min <- get_pretty_excel_table(df_min, bilateral_dataset = bilateral_max,
                                   psa_summary = psa_summary, 
                                   psa_summary_index = '2.5%',
                                   mechanism = name)
  df_max <- get_pretty_excel_table(df_max, 
                                   bilateral_dataset = bilateral_min,
                                   psa_summary = psa_summary, 
                                   psa_summary_index = '97.5%',
                                   mechanism = name)
  df_median <- get_pretty_excel_table(df_median, 
                                      bilateral_dataset = bilateral_median,
                                      psa_summary = psa_summary, 
                                      psa_summary_index = '50%',
                                      mechanism = name)
  compiled_df <- df_median
  compiled_df[,'Price per dose'] <- 
    paste0(df_median[,'Price per dose'], ' (',df_min[,'Price per dose'],' - ',
    df_max[,'Price per dose'],')')
  
  compiled_df[,'Total expenditure'] <- 
    paste0(df_median[,'Total expenditure'], ' (',df_min[,'Total expenditure'],' - ',
    df_max[,'Total expenditure'],')')
  if ('Total savings in expenditure' %in% names(compiled_df)){
    compiled_df[,'Total savings in expenditure'] <- 
      paste0(df_median[,'Total savings in expenditure'], ' (',
             df_min[,'Total savings in expenditure'],' - ',
      df_max[,'Total savings in expenditure'],')')
  }
  compiled_df[,'Percentage share'] <- 
    paste0(df_median[,'Percentage share'], ' (',df_min[,'Percentage share'],' - ',
      df_max[,'Percentage share'],')')
  
  return(compiled_df)
}

# ------------------------------------------------------------------------------
# Function to compile results for sensitivity analysis where alpha is varied 
# for all pricing mechanisms, producing Table in the Supplement
# ------------------------------------------------------------------------------

compile_sheet_alpha <- function(coverage){
  adj_gdppc_sheet <- compile_alpha_results(df_median = adj_gdppc, 
                                           df_min = adj_gdppc_min,
                                           df_max = adj_gdppc_max,
                                           bilateral_median = bilateral_output,
                                           bilateral_min = bilateral_output,
                                           bilateral_max = bilateral_output,
                                           name = 'adj_gdppc')
  adj_chepc_sheet <- compile_alpha_results(df_median = adj_chepc, 
                                           df_min = adj_chepc_min,
                                           df_max = adj_chepc_max,
                                           bilateral_median = bilateral_output,
                                           bilateral_min = bilateral_output,
                                           bilateral_max = bilateral_output,
                                           name = 'adj_chepc')
  
  write_xlsx(list('adj_gdppc'=adj_gdppc_sheet,'adj_chepc'=adj_chepc_sheet),
             path = paste0('output/tables/compiled/alpha_',coverage,'.xlsx'))
}



# Helper function for compile_sheet_alpha
# Given 3 dfs for median and 95% UI, returns formatted df 
compile_alpha_results <- function(df_median, df_min, df_max, name,
                                     bilateral_median=NA, bilateral_min=NA, 
                                     bilateral_max=NA){
  df_min <- get_pretty_excel_table(df_min, bilateral_dataset = bilateral_max,
                                    mechanism = name)
  df_max <- get_pretty_excel_table(df_max, 
                                   bilateral_dataset = bilateral_min,
                                   mechanism = name)
  df_median <- get_pretty_excel_table(df_median, 
                                      bilateral_dataset = bilateral_median,
                                      mechanism = name)
  outcome_cols <- which(names(df_min) %in% c('Price per dose','Total expenditure',
                                             'Total savings in expenditure',
                                             'Percentage share'))
  names(df_min)[outcome_cols] <- paste0(names(df_min)[outcome_cols], ' (alpha = 0)')
  compiled_df <- cbind(df_min, 'Price per dose (alpha = 10)'=df_median$`Price per dose`,
                       'Total expenditure (alpha = 10)'=df_median$`Total expenditure`,
                       'Total savings in expenditure (alpha = 10)'=df_median$`Total savings in expenditure`,
                       'Percentage share (alpha = 10)'=df_median$`Percentage share`,
                       'Price per dose (alpha = 20)'=df_max$`Price per dose`,
                       'Total expenditure (alpha = 20)'=df_max$`Total expenditure`,
                       'Total savings in expenditure (alpha = 20)'=df_max$`Total savings in expenditure`,
                       'Percentage share (alpha = 20)'=df_max$`Percentage share`)
  income_var <- paste0(toupper(substr(name,5,7)),' per capita')
  curr_income_vars <- colnames(compiled_df)[which(sapply(colnames(compiled_df),function(x){substr(x,5,nchar(x))})=='per capita')]
  to_rm <- curr_income_vars[curr_income_vars!=income_var]
  compiled_df <- compiled_df[,!colnames(compiled_df) %in% c(to_rm)]
  return(compiled_df)
}
# 
# compile_sheet <- function(coverage){
#   bilateral_sheet <- compile_base_sensitivity(df_median = bilateral$calc_output, 
#                                               df_min = prices_calc(mechanism = 'bilateral', coverage = coverage, 
#                                                                    mode = mode, comparator = NA, 
#                                                                    psa_number = ranges_bilateral_total$psa_number$min)$calc_output,
#                                               df_max = prices_calc(mechanism = 'bilateral', coverage = coverage, 
#                                                                    mode = mode, comparator = NA, 
#                                                                    psa_number = ranges_bilateral_total$psa_number$max)$calc_output)
#   uniform_sheet <- compile_base_sensitivity(df_median = uniform$calc_output, 
#                                             df_min = prices_calc(mechanism = 'uniform', coverage = coverage, 
#                                                                  mode = mode, comparator = NA, 
#                                                                  psa_number = ranges_uniform_total$psa_number$min)$calc_output,
#                                             df_max = prices_calc(mechanism = 'uniform', coverage = coverage, 
#                                                                  mode = mode, comparator = NA, 
#                                                                  psa_number = ranges_uniform_total$psa_number$max)$calc_output)
#   adj_gdppc_sheet <- compile_base_sensitivity(df_median = adj_gdppc$calc_output, 
#                                               df_min = prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
#                                                                    mode = mode, comparator = NA, 
#                                                                    psa_number = ranges_adj_gdppc_total$psa_number$min)$calc_output,
#                                               df_max = prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
#                                                                    mode = mode, comparator = NA, 
#                                                                    psa_number = ranges_adj_gdppc_total$psa_number$max)$calc_output)
#   adj_chepc_sheet <- compile_base_sensitivity(df_median = adj_chepc$calc_output, 
#                                               df_min = prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
#                                                                    mode = mode, comparator = NA, 
#                                                                    psa_number = ranges_adj_chepc_total$psa_number$min)$calc_output,
#                                               df_max = prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
#                                                                    mode = mode, comparator = NA, 
#                                                                    psa_number = ranges_adj_chepc_total$psa_number$max)$calc_output)
#   write_xlsx(list('bilateral'=bilateral_sheet,'uniform'=uniform_sheet,'adj_gdppc'=adj_gdppc_sheet,
#                   'adj_chepc'=adj_chepc_sheet),path = paste0('output/final_output/',coverage,'_base_psa_corrected.xlsx'))
# }
# 
# 
# 
# 
