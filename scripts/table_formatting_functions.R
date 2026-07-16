
get_pretty_excel_table <- function(dataset,bilateral_dataset=NA){
  dataset <- get_overall_region_data(dataset,bilateral_dataset = bilateral_dataset)
  
  if (!is.data.frame(bilateral_dataset)){
    sheet_psa <- data.frame(cbind('iso3c'=dataset$iso3c,
                                  'Population'=dataset$pop_covered,
                                  'GDP per capita'=dataset$gdppc,
                                  'CHE per capita'=dataset$chepc,
                                  'perdose'=as.numeric(dataset$perdose),                                  
                                  'total_perdose'=as.numeric(dataset$total_perdose),
                                  'share'=as.numeric(dataset$percent_share)))
  } else{
    sheet_psa <- data.frame(cbind('iso3c'=dataset$iso3c,
                                  'Population'=dataset$pop_covered,
                                  'GDP per capita'=dataset$gdppc,
                                  'CHE per capita'=dataset$chepc,
                                  'perdose'=as.numeric(dataset$perdose),
                                  'total_perdose'=as.numeric(dataset$total_perdose),
                                  'total_savings_perdose'=as.numeric(dataset$total_savings_perdose),
                                  'share'=as.numeric(dataset$percent_share)))
  }
  
  for (i in (2:ncol(sheet_psa))){
    sheet_psa[,i] <- as.numeric(sheet_psa[,i])
  }
  
  vars_to_round <- c('total_perdose','total_savings_perdose')
  for (var in vars_to_round){
    if (var %in% names(sheet_psa)){
      index = 1:nrow(sheet_psa)
      if (var=='total_savings_perdose'){
        index = 1:(nrow(sheet_psa)-1)
      }
      sheet_psa[index,var] <- round_up_thousands(sheet_psa[index,var])
    }
  }
  sheet_psa$perdose <- round(sheet_psa$perdose,2)
  sheet_psa$share <- round(sheet_psa$share,2)
  sheet_psa$Country <- ""
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
    sheet_psa <- sheet_psa[,c('Country','Population','GDP.per.capita','CHE.per.capita','perdose','total_perdose',
                              'share')]
    names(sheet_psa) <- c('Country','Population','GDP per capita','CHE per capita','Price per dose',
                          'Total expenditure',
                          'Percentage share')
    sheet_psa[nrow(sheet_psa),c('GDP per capita','CHE per capita', 'Price per dose')] <- c('','','')
  } else {
    sheet_psa[nrow(sheet_psa)-1,]$Country <- 'SEA'
    sheet_psa[(nrow(sheet_psa)-1):nrow(sheet_psa),]$perdose <- ""
    sheet_psa[nrow(sheet_psa),]$Country <- 'SEA (%)'
    sheet_psa[nrow(sheet_psa),]$total_perdose <- ""
    sheet_psa[nrow(sheet_psa),]$share <- ""
    sheet_psa <- sheet_psa[,c('Country','Population','GDP.per.capita','CHE.per.capita','perdose','total_perdose',
                              'total_savings_perdose','share')]
    names(sheet_psa) <- c('Country','Population','GDP per capita','CHE per capita','Price per dose',
                          'Total expenditure','Total savings in expenditure',
                          'Percentage share')
    sheet_psa[(nrow(sheet_psa)-1):nrow(sheet_psa),c('Population','GDP per capita', 'CHE per capita',
                                                    'Price per dose')] <- rep('',8)
    
  }
  
  return(sheet_psa)
}



compile_base_sensitivity <- function(df_median, df_min, df_max, name,
                                     bilateral_median=NA, bilateral_min=NA, bilateral_max=NA){
  df_min <- get_pretty_excel_table(df_min, bilateral_dataset = bilateral_max)
  df_max <- get_pretty_excel_table(df_max, bilateral_dataset = bilateral_min)
  df_median <- get_pretty_excel_table(df_median, bilateral_dataset = bilateral_median)
  
  compiled_df <- df_median
  compiled_df[,'Total expenditure'] <- paste0(df_median[,'Total expenditure'], ' (',df_min[,'Total expenditure'],' - ',
                                              df_max[,'Total expenditure'],')')
  if ('Total savings in expenditure' %in% names(compiled_df)){
    compiled_df[,'Total savings in expenditure'] <- paste0(df_median[,'Total savings in expenditure'], ' (',df_min[,'Total savings in expenditure'],' - ',
                                                           df_max[,'Total savings in expenditure'],')')
    compiled_df[,'Percentage share'] <- paste0(df_median[,'Percentage share'], ' (',df_min[,'Percentage share'],' - ',
                                               df_max[,'Percentage share'],')')
  }
  return(compiled_df)
}

