
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

