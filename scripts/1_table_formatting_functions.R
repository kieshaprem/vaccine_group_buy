library(countrycode)

get_pretty_excel_table <- function(dataset,mechanism = NA, bilateral_dataset=NA, psa_summary=NA,
                                   psa_summary_index = NA){
  dataset <- get_overall_region_data(dataset,
                                     bilateral_dataset = bilateral_dataset,
                                     psa_summary = psa_summary,
                                     psa_summary_index = psa_summary_index,
                                     mechanism = mechanism)
  
  if (!is.data.frame(bilateral_dataset)){
    sheet_psa <- data.frame(cbind('iso3c'=dataset$iso3c,
                                  'Income Group' = dataset$income_grp,
                                  'Population'=dataset$pop_covered,
                                  'GDP per capita'=dataset$gdppc,
                                  'CHE per capita'=dataset$chepc,
                                  'perdose'=as.numeric(dataset$perdose),                                  
                                  'total_perdose'=as.numeric(dataset$total_perdose),
                                  'share'=as.numeric(dataset$percent_share)))
  } else{ sheet_psa <- data.frame(cbind('iso3c'=dataset$iso3c,
                                  'Income Group' = dataset$income_grp,
                                  'Population'=dataset$pop_covered,
                                  'GDP per capita'=dataset$gdppc,
                                  'CHE per capita'=dataset$chepc,
                                  'perdose'=as.numeric(dataset$perdose),
                                  'total_perdose'=as.numeric(dataset$total_perdose),
                                  'total_savings_perdose'=as.numeric(dataset$total_savings_perdose),
                                  'share'=as.numeric(dataset$percent_share)))
  }
  
  for (i in (3:ncol(sheet_psa))){
    sheet_psa[,i] <- as.numeric(sheet_psa[,i])
  }
  sheet_psa$share <- round(sheet_psa$share,2)
  sheet_psa$Country <- 
    countrycode(sheet_psa$iso3c,
                origin = 'iso3c', destination = 'country.name')
  sheet_psa[nrow(sheet_psa),]$Country <- 'SEA'
  sheet_psa <- sheet_psa %>%
    mutate(across(
      .cols = where(is.numeric),  # Only numeric columns
      .fns  = ~ prettyNum(., big.mark = ",", preserve.width = "none",nsmall=2)
    ))
  
  if (!is.data.frame(bilateral_dataset)){
    sheet_psa <- sheet_psa[,c('Country','Income.Group','Population','GDP.per.capita','CHE.per.capita','perdose','total_perdose',
                              'share')]
    names(sheet_psa) <- c('Country','Income Group','Population','GDP per capita','CHE per capita','Price per dose',
                          'Total expenditure','Percentage share')
    sheet_psa[nrow(sheet_psa),c('GDP per capita','CHE per capita', 'Price per dose')] <- c('','','')
  } else {
    sheet_psa[nrow(sheet_psa)-1,]$Country <- 'SEA'
    sheet_psa[(nrow(sheet_psa)-1):nrow(sheet_psa),]$perdose <- ""
    sheet_psa[nrow(sheet_psa),]$Country <- 'SEA (%)'
    sheet_psa[nrow(sheet_psa),]$total_perdose <- ""
    #sheet_psa[nrow(sheet_psa),]$share <- ""
    sheet_psa <- sheet_psa[,c('Country','Income.Group','Population','GDP.per.capita','CHE.per.capita','perdose','total_perdose',
                              'total_savings_perdose','share')]
    names(sheet_psa) <- c('Country','Income Group','Population','GDP per capita','CHE per capita','Price per dose',
                          'Total expenditure','Total savings in expenditure',
                          'Percentage share')
    sheet_psa[(nrow(sheet_psa)-1):nrow(sheet_psa),c('Population','GDP per capita', 'CHE per capita',
                                                    'Price per dose')] <- rep('',8)
    
  }
  
  return(sheet_psa)
}



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
  compiled_df[,'Price per dose'] <- paste0(df_median[,'Price per dose'], ' (',df_min[,'Price per dose'],' - ',
                                              df_max[,'Price per dose'],')')
  
  compiled_df[,'Total expenditure'] <- paste0(df_median[,'Total expenditure'], ' (',df_min[,'Total expenditure'],' - ',
                                              df_max[,'Total expenditure'],')')
  if ('Total savings in expenditure' %in% names(compiled_df)){
    compiled_df[,'Total savings in expenditure'] <- paste0(df_median[,'Total savings in expenditure'], ' (',df_min[,'Total savings in expenditure'],' - ',
                                                           df_max[,'Total savings in expenditure'],')')
  }
  compiled_df[,'Percentage share'] <- paste0(df_median[,'Percentage share'], ' (',df_min[,'Percentage share'],' - ',
                                             df_max[,'Percentage share'],')')
  
  return(compiled_df)
}



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

compile_sheet <- function(coverage){
  bilateral_sheet <- compile_base_sensitivity(df_median = bilateral$calc_output, 
                                              df_min = prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                                                   mode = mode, comparator = NA, 
                                                                   psa_number = ranges_bilateral_total$psa_number$min)$calc_output,
                                              df_max = prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                                                   mode = mode, comparator = NA, 
                                                                   psa_number = ranges_bilateral_total$psa_number$max)$calc_output)
  uniform_sheet <- compile_base_sensitivity(df_median = uniform$calc_output, 
                                            df_min = prices_calc(mechanism = 'uniform', coverage = coverage, 
                                                                 mode = mode, comparator = NA, 
                                                                 psa_number = ranges_uniform_total$psa_number$min)$calc_output,
                                            df_max = prices_calc(mechanism = 'uniform', coverage = coverage, 
                                                                 mode = mode, comparator = NA, 
                                                                 psa_number = ranges_uniform_total$psa_number$max)$calc_output)
  adj_gdppc_sheet <- compile_base_sensitivity(df_median = adj_gdppc$calc_output, 
                                              df_min = prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                                                   mode = mode, comparator = NA, 
                                                                   psa_number = ranges_adj_gdppc_total$psa_number$min)$calc_output,
                                              df_max = prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                                                   mode = mode, comparator = NA, 
                                                                   psa_number = ranges_adj_gdppc_total$psa_number$max)$calc_output)
  adj_chepc_sheet <- compile_base_sensitivity(df_median = adj_chepc$calc_output, 
                                              df_min = prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                                                   mode = mode, comparator = NA, 
                                                                   psa_number = ranges_adj_chepc_total$psa_number$min)$calc_output,
                                              df_max = prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                                                   mode = mode, comparator = NA, 
                                                                   psa_number = ranges_adj_chepc_total$psa_number$max)$calc_output)
  write_xlsx(list('bilateral'=bilateral_sheet,'uniform'=uniform_sheet,'adj_gdppc'=adj_gdppc_sheet,
                  'adj_chepc'=adj_chepc_sheet),path = paste0('output/final_output/',coverage,'_base_psa_corrected.xlsx'))
}

get_psa_range_text <- function(measure, psa_summary){
  return(paste0(psa_summary[measure,'50%'],' (',
                psa_summary[measure,'2.5%'],' - ',
                psa_summary[measure,'97.5%'],')'))
}

table_mapping <- function(mechanism, psa_summary, country_level_summary, coverage,
                          bilateral_min = NA, bilateral_median = NA,
                          bilateral_max = NA){
  first_q <- prices_calc(mechanism = mechanism, 
              coverage = coverage, 
              comparator = NA, 
              psa_summary = psa_summary, 
              psa_summary_index = '2.5%', mode = 'output_psa',
              psa_country_level_summary = country_level_summary)$calc_output
  
  mid_q <- prices_calc(mechanism = mechanism, 
                         coverage = coverage, 
                         comparator = NA, 
                         psa_summary = psa_summary, 
                         psa_summary_index = '50%', mode = 'output_psa',
                       psa_country_level_summary = country_level_summary)$calc_output
  
  third_q <- prices_calc(mechanism = mechanism, 
                       coverage = coverage, 
                       comparator = NA, 
                       psa_summary = psa_summary, 
                       psa_summary_index = '97.5%', mode = 'output_psa',
                       psa_country_level_summary = country_level_summary)$calc_output
  sheet <- compile_base_sensitivity(df_median = mid_q, df_min = first_q, df_max = third_q,
                           psa_summary = psa_summary, name = mechanism,bilateral_median = bilateral_median,
                           bilateral_min = bilateral_min, bilateral_max = bilateral_max)
  return(list('ranges_sheet'=sheet,'first_q'=first_q,'mid_q'=mid_q,'third_q'=third_q))
}


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
             path = paste0('output/final_output/',coverage,'_base_psa_empirical_quantiles_other_wassumption.xlsx'))
  
}

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
             path = paste0('output/final_output/base_analysis_empirical_other_',coverage,'.xlsx'))
}


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
             path = paste0('output/final_output/',coverage,'_others_base_alpha.xlsx'))
}

