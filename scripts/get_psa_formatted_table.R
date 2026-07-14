library(countrycode)
library(dplyr)

get_overall_region_data <- function(dataset,bilateral_dataset=NA,name){
  col_id_pop <- which(names(dataset)=='pop_covered')
  col_id_total <- which(names(dataset)=='total_perdose')
  col_id_share <- which(names(dataset)=='percent_share')
  col_id <- 0
  if (substr(name,start=1,stop=9)!='bilateral'){
    col_id <- which(names(dataset)=='total_savings_perdose')
  }
  new_row <- c("SEA",character(ncol(dataset)-1))
  new_row[col_id_pop] <- sum(dataset$pop_covered)
  new_row[col_id_total] <- sum(dataset$total_perdose)
  if (col_id!=0){
    new_row[col_id] <- sum(dataset$total_savings_perdose)
    new_row_2 <- c("SEA (%)",character(ncol(dataset)-1))
    new_row_2[col_id] <- sum(dataset$total_savings_perdose)*100/sum(bilateral_dataset$total_perdose)
  }
  new_row[col_id_share] <- sum(dataset$percent_share)
  dataset[nrow(dataset)+1,] <- new_row
  if (col_id!=0){
    dataset[nrow(dataset)+1,] <- new_row_2
  }
  return(dataset)
}



get_pretty_excel_table <- function(dataset,dataset_min,dataset_max,
                                   bilateral_dataset=NA,
                                   bilateral_dataset_min=NA,
                                   bilateral_dataset_max=NA,
                                   name){
  dataset <- get_overall_region_data(dataset,bilateral_dataset = bilateral_dataset, name=name)
  dataset_min <- get_overall_region_data(dataset_min,bilateral_dataset = bilateral_dataset_min, name=name)
  dataset_max <- get_overall_region_data(dataset_max, bilateral_dataset = bilateral_dataset_max, name=name)
  
  if (substr(name,start=1,stop=9)=='bilateral'){
    sheet_psa <- data.frame(cbind('iso3c'=dataset$iso3c,
                                  'Population'=dataset$pop_covered,
                                  'GDP per capita'=dataset$gdp_percapita,
                                  'perdose_min'=as.numeric(dataset_min$perdose),
                                  'perdose'=as.numeric(dataset$perdose),
                                  'perdose_max'=as.numeric(dataset_max$perdose),
                                 'total_perdose_min'=as.numeric(dataset_min$total_perdose),
                                  'total_perdose'=as.numeric(dataset$total_perdose),
                                  'total_perdose_max'=as.numeric(dataset_max$total_perdose),
                                   'share_min'=as.numeric(dataset_min$percent_share),
                                  'share'=as.numeric(dataset$percent_share),
                                  'share_max'=as.numeric(dataset_max$percent_share)))
  } else{
    sheet_psa <- data.frame(cbind('iso3c'=dataset$iso3c,
                                  'Population'=dataset$pop_covered,
                                  'GDP per capita'=dataset$gdp_percapita,
                                  'perdose_min'=as.numeric(dataset_min$perdose),
                                  'perdose'=as.numeric(dataset$perdose),
                                  'perdose_max'=as.numeric(dataset_max$perdose),
                                  'savings_perdose_min'=as.numeric(dataset_min$savings_perdose),
                                  'savings_perdose'=as.numeric(dataset$savings_perdose),
                                  'savings_perdose_max'=as.numeric(dataset_max$savings_perdose),
                                  'total_perdose_min'=as.numeric(dataset_min$total_perdose),
                                  'total_perdose'=as.numeric(dataset$total_perdose),
                                  'total_perdose_max'=as.numeric(dataset_max$total_perdose),
                                  'total_savings_perdose_min'=as.numeric(dataset_min$total_savings_perdose),
                                  'total_savings_perdose'=as.numeric(dataset$total_savings_perdose),
                                  'total_savings_perdose_max'=as.numeric(dataset_max$total_savings_perdose),
                                  'share_min'=as.numeric(dataset_min$percent_share),
                                  'share'=as.numeric(dataset$percent_share),
                                  'share_max'=as.numeric(dataset_max$percent_share)))
  }
  if (substr(name,start=1,stop=9)=='adj_chepc'){
    sheet_psa <- data.frame(cbind(sheet_psa,'CHE per capita'=dataset$chepc))
  }
  
  for (i in (2:ncol(sheet_psa))){
    sheet_psa[,i] <- as.numeric(sheet_psa[,i])
  }
  sheet_psa$share_min <- round(sheet_psa$share_min,2)
  sheet_psa$share <- round(sheet_psa$share,2)
  sheet_psa$share_max <- round(sheet_psa$share_max,2)
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
  if (sum(sheet_psa$perdose!=sheet_psa$perdose_min)+
      sum(sheet_psa$perdose!=sheet_psa$perdose_max)>0){
    sheet_psa$perdose <- paste0(sheet_psa$perdose, " (",
                                sheet_psa$perdose_min," - ",
                                sheet_psa$perdose_max,")")
    sheet_psa[nrow(sheet_psa),]$perdose <- ""
  }
  if (sum(sheet_psa$total_perdose!=sheet_psa$total_perdose_min)+
      sum(sheet_psa$total_perdose!=sheet_psa$total_perdose_max)>0){
  sheet_psa$total_perdose <- paste0(sheet_psa$total_perdose, " (",
                                    sheet_psa$total_perdose_min," - ",
                                    sheet_psa$total_perdose_max,")")}
  if (sum(sheet_psa$share!=sheet_psa$share_min)+ 
      sum(sheet_psa$share!=sheet_psa$share_max)>0){
  sheet_psa$share <- paste0(sheet_psa$share, " (",
                            sheet_psa$share_min," - ",
                            sheet_psa$share_max,")")}
  if (substr(name,start=1,stop=9)=='bilateral'){
    sheet_psa <- sheet_psa[,c('Country','Population','GDP.per.capita','perdose','total_perdose',
                              'share')]
    names(sheet_psa) <- c('Country','Population','GDP per capita','Price per dose',
                          'Total expenditure',
                          'Percentage share')
  }

  if (substr(name,start=1,stop=9)!='bilateral'){
    sheet_psa[nrow(sheet_psa)-1,]$Country <- 'SEA'
    sheet_psa[(nrow(sheet_psa)-1):nrow(sheet_psa),]$perdose <- ""
    sheet_psa[nrow(sheet_psa),]$Country <- 'SEA (%)'
    sheet_psa[nrow(sheet_psa),]$total_perdose <- ""
    sheet_psa[nrow(sheet_psa),]$share <- ""
    
    if (sum(sheet_psa$savings_perdose!=sheet_psa$savings_perdose_min)+
        sum(sheet_psa$savings_perdose!=sheet_psa$savings_perdose_max)>0){
      sheet_psa$savings_perdose <- paste0(sheet_psa$savings_perdose, " (",
                                          sheet_psa$savings_perdose_min," - ",
                                          sheet_psa$savings_perdose_max,")")
      sheet_psa[(nrow(sheet_psa)-1):nrow(sheet_psa),]$savings_perdose <- ""
    }
    if (sum(sheet_psa$total_savings_perdose!=sheet_psa$total_savings_perdose_min)+
        sum(sheet_psa$total_savings_perdose!=sheet_psa$total_savings_perdose_max)>0){
      sheet_psa$total_savings_perdose <- paste0(sheet_psa$total_savings_perdose, " (",
                                                sheet_psa$total_savings_perdose_min," - ",
                                                sheet_psa$total_savings_perdose_max,")")
    }

    if (substr(name,start=1,stop=9)!='adj_chepc'){
      
    sheet_psa <- sheet_psa[,c('Country','Population','GDP.per.capita','perdose','savings_perdose','total_perdose',
                              'total_savings_perdose','share')]
    names(sheet_psa) <- c('Country','Population','GDP per capita','Price per dose','Savings per dose',
                          'Total expenditure','Total savings in expenditure',
                          'Percentage share')
    }
    if (substr(name,start=1,stop=9)=='adj_chepc'){
      sheet_psa <- sheet_psa[,c('Country','Population','GDP.per.capita','CHE.per.capita','perdose','savings_perdose','total_perdose',
                                'total_savings_perdose','share')]
      names(sheet_psa) <- c('Country','Population','GDP per capita','CHE per capita','Price per dose','Savings per dose',
                            'Total expenditure','Total savings in expenditure',
                            'Percentage share')
    }
    
  }

  return(sheet_psa)
}
