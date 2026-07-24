load('output/intermediate/main_data.rdata')
source('scripts/0_process_calculations.R')
source('scripts/0_load_mi4a_data.R')
source('scripts/1_get_prices.R')
#source('scripts/1_examine_psa_results.R')

library(dplyr)
library(countrycode)
library(writexl)

# objective:
# 0. select mechanism (bilateral, uniform, or tiered), coverage (100% or real) & mode (base or PSA)
# 1. get df for mechanism with all calculated values
# 2. output table in pretty format

get_prices <- function(mechanism, mode, psa_number = NA, psa_summary = NA,
                       psa_summary_index = NA, alpha = NA){
  df <- main_data
  if (mechanism == 'bilateral'){
    # input bilateral price
    prices <- get_bilateral(df = df, mode = mode, psa_number = psa_number,
                            psa_summary = psa_summary, psa_summary_index = psa_summary_index)
  } else if (mechanism == 'uniform'){
    # input uniform price
    prices <- get_uniform(df = df, mode = mode, psa_number = psa_number,
                          psa_summary = psa_summary, psa_summary_index = psa_summary_index)
  } else if (mechanism == 'adj_gdppc'){
    # input adj_gdppc price
    prices <- get_tiered(df = df, adj = 'gdppc', 
                         mode = mode, psa_number = psa_number,
                         psa_summary = psa_summary, psa_summary_index = psa_summary_index,
                         alpha = alpha)
  } else {
    # input adj_chepc price
    prices <- get_tiered(df = df, adj = 'chepc', 
                         mode = mode, psa_number = psa_number,
                         psa_summary = psa_summary, psa_summary_index = psa_summary_index,
                         alpha = alpha)
  }
  return(prices)
}

calc <- function(df,coverage,comparator,mechanism){
  if (coverage == 'full'){
    # input pop_covered = pop_size
    df$pop_covered <- df$population_size
  } else {
    # input pop_covered = covid-19 coverage
    df$pop_covered <- as.numeric(df$coverage_covid_1dose_percent)/100*df$population_size
  }
  
  # process mechanism using function
  df <- process_mechanism(df = df, mechanism = mechanism, bilateral_df = comparator)
  
  # output table
  return (df)
}

prices_calc <- function(mechanism, coverage, mode, comparator = NA, psa_number=NA, psa_summary = NA,
                        psa_summary_index = NA, psa_country_level_summary=NA,alpha=10){
    prices <- get_prices(mechanism = mechanism, mode = mode, psa_number = psa_number,
                         psa_summary = psa_summary, psa_summary_index = psa_summary_index,
                         alpha = alpha)
    output <- list()
    price <- prices[['df']]
    output_df <- calc(df = price, coverage = coverage, comparator = comparator, mechanism= mechanism)
    if (mode == 'output_psa'){
      output_df$percent_share <- get_data(summary_df = psa_country_level_summary[[mechanism]],
                                                  psa_index = psa_summary_index,data_type = 'percent')
      output_df$total_perdose <- get_data(summary_df = psa_country_level_summary[[mechanism]],
                                                  psa_index = psa_summary_index, data_type = 'total')
      output_df$total_savings_perdose <- get_data(summary_df = psa_country_level_summary[[mechanism]],
                                                  psa_index = psa_summary_index, data_type = 'savings')
      
    }
    output[['calc_output']] <- output_df
    return (output)
}

# to extract % share ranges from country_level_psa_summary
get_data <- function(summary_df, psa_index, data_type){
  row_id <- which(sapply(rownames(summary_df),function(x){substr(x,5,nchar(x))})==data_type)
  return(summary_df[row_id,psa_index])
}


run_scenario <- function(coverage,mode){
  outcome <- data.frame(array(dim = c(26,10000)))
  rownames(outcome) <- c('bilateral_total','uniform_total','adj_gdppc_total',
                         'adj_chepc_total',
                         'savings_uniform_percent','savings_adj_gdppc_percent',
                         'savings_adj_chepc_percent',
                         'savings_uniform','savings_adj_gdppc',
                         'savings_adj_chepc',
                         'bilateral_gdppc_wci','uniform_gdppc_wci',
                         'adj_gdppc_gdppc_wci','adj_chepc_gdppc_wci',
                         'bilateral_chepc_wci','uniform_chepc_wci',
                         'adj_gdppc_chepc_wci','adj_chepc_chepc_wci',
                         'bilateral_gdppc_ci','uniform_gdppc_ci',
                         'adj_gdppc_gdppc_ci','adj_chepc_gdppc_ci',
                         'bilateral_chepc_ci','uniform_chepc_ci',
                         'adj_gdppc_chepc_ci','adj_chepc_chepc_ci')
  if (mode == 'base'){
    bilateral_output <- prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                    mode = mode, comparator = NA)
    uniform_output <- prices_calc(mechanism = 'uniform', coverage = coverage, 
                                  mode = mode, comparator = bilateral_output$calc_output)
    adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                    mode = mode, comparator = bilateral_output$calc_output)
    adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                    mode = mode, comparator = bilateral_output$calc_output)
    save(bilateral_output,file = paste0('output/final_output/bilateral_',coverage,'_',mode,'.rdata'))
    save(uniform_output,file = paste0('output/final_output/uniform_',coverage,'_',mode,'.rdata'))
    save(adj_gdppc_output,file = paste0('output/final_output/adj_gdppc_',coverage,'_',mode,'.rdata'))
    save(adj_chepc_output,file = paste0('output/final_output/adj_chepc_',coverage,'_',mode,'.rdata'))
    
  } else if (mode == 'alpha_min'){
    adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output,alpha = 0)
    adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output, alpha = 0)
    save(adj_gdppc_output,file = paste0('output/final_output/adj_gdppc_',coverage,'_',mode,'.rdata'))
    save(adj_chepc_output,file = paste0('output/final_output/adj_chepc_',coverage,'_',mode,'.rdata'))
  } else if (mode =='alpha_max'){
    adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output,alpha = 20)
    adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output, alpha = 20)
    save(adj_gdppc_output,file = paste0('output/final_output/adj_gdppc_',coverage,'_',mode,'.rdata'))
    save(adj_chepc_output,file = paste0('output/final_output/adj_chepc_',coverage,'_',mode,'.rdata'))
  } else {
    for (i in 1:10000){
      print(i)
      bilateral_output <- prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                      mode = mode, comparator = NA, psa_number = i)
      uniform_output <- prices_calc(mechanism = 'uniform', coverage = coverage, 
                                    mode = mode, comparator = bilateral_output$calc_output, psa_number = i)
      adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                      mode = mode, comparator = bilateral_output$calc_output, psa_number = i)
      adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                      mode = mode, comparator = bilateral_output$calc_output, psa_number = i)
      outcome['bilateral_total',paste0("X",i)] <- sum(bilateral_output$calc_output$total_perdose)
      outcome['uniform_total',paste0("X",i)] <- sum(uniform_output$calc_output$total_perdose)
      outcome['adj_gdppc_total',paste0("X",i)] <- sum(adj_gdppc_output$calc_output$total_perdose)
      outcome['adj_chepc_total',paste0("X",i)] <- sum(adj_chepc_output$calc_output$total_perdose)
      
      outcome['savings_uniform',paste0("X",i)] <- sum(uniform_output$calc_output$total_savings_perdose)
      outcome['savings_adj_gdppc',paste0("X",i)] <- sum(adj_gdppc_output$calc_output$total_savings_perdose)
      outcome['savings_adj_chepc',paste0("X",i)] <- sum(adj_chepc_output$calc_output$total_savings_perdose)
      
      outcome['savings_uniform_percent',paste0("X",i)] <- outcome['savings_uniform',paste0("X",i)]*100/
        sum(bilateral_output$calc_output$total_perdose)
      outcome['savings_adj_gdppc_percent',paste0("X",i)] <- outcome['savings_adj_gdppc',paste0("X",i)]*100/
        sum(bilateral_output$calc_output$total_perdose)
      outcome['savings_adj_chepc_percent',paste0("X",i)] <- outcome['savings_adj_chepc',paste0("X",i)]*100/
        sum(bilateral_output$calc_output$total_perdose)
      
      # outcome['bilateral_gdppc_wci',paste0("X",i)] <- get_wci(bilateral_output$calc_output,
      #                                                   perspective = 'gdppc')$concentration_index
      # outcome['uniform_gdppc_wci',paste0("X",i)] <- get_wci(uniform_output$calc_output,
      #                                                         perspective = 'gdppc')$concentration_index
      # outcome['adj_gdppc_gdppc_wci',paste0("X",i)] <- get_wci(adj_gdppc_output$calc_output,
      #                                                       perspective = 'gdppc')$concentration_index
      # outcome['adj_chepc_gdppc_wci',paste0("X",i)] <- get_wci(adj_chepc_output$calc_output,
      #                                                       perspective = 'gdppc')$concentration_index
      # outcome['bilateral_chepc_wci',paste0("X",i)] <- get_wci(bilateral_output$calc_output,
      #                                                         perspective = 'chepc')$concentration_index
      # outcome['uniform_chepc_wci',paste0("X",i)] <- get_wci(uniform_output$calc_output,
      #                                                       perspective = 'chepc')$concentration_index
      # outcome['adj_gdppc_chepc_wci',paste0("X",i)] <- get_wci(adj_gdppc_output$calc_output,
      #                                                         perspective = 'chepc')$concentration_index
      # outcome['adj_chepc_chepc_wci',paste0("X",i)] <- get_wci(adj_chepc_output$calc_output,
      #                                                         perspective = 'chepc')$concentration_index
      # 
      # outcome['bilateral_gdppc_ci',paste0("X",i)] <- get_ci(bilateral_output$calc_output,
      #                                                         perspective = 'gdppc')
      # outcome['uniform_gdppc_ci',paste0("X",i)] <- get_ci(uniform_output$calc_output,
      #                                                       perspective = 'gdppc')
      # outcome['adj_gdppc_gdppc_ci',paste0("X",i)] <- get_ci(adj_gdppc_output$calc_output,
      #                                                         perspective = 'gdppc')
      # outcome['adj_chepc_gdppc_ci',paste0("X",i)] <- get_ci(adj_chepc_output$calc_output,
      #                                                         perspective = 'gdppc')
      # 
      # outcome['bilateral_chepc_ci',paste0("X",i)] <- get_ci(bilateral_output$calc_output,
      #                                                         perspective = 'chepc')
      # outcome['uniform_chepc_ci',paste0("X",i)] <- get_ci(uniform_output$calc_output,
      #                                                       perspective = 'chepc')
      # outcome['adj_gdppc_chepc_ci',paste0("X",i)] <- get_ci(adj_gdppc_output$calc_output,
      #                                                         perspective = 'chepc')
      # outcome['adj_chepc_chepc_ci',paste0("X",i)] <- get_ci(adj_chepc_output$calc_output,
      #                                                         perspective = 'chepc')
      
    }
    return(outcome)
  }
}

run_country_level_psa <- function(psa_mat, mechanism, coverage, comparator_df = NA){
  outcome <- data.frame(array(dim = c(33,10000)))
  rownames(outcome) <- c(sapply(main_data$iso3c,function(x){paste0(x,'_total')}),
                         sapply(main_data$iso3c,function(x){paste0(x,'_savings')}),
                         sapply(main_data$iso3c,function(x){paste0(x,'_percent')}))
  for (i in 1:10000){
    if (mechanism!='bilateral'){
      comparator_df <- prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                   mode = 'psa', comparator = NA, psa_number = i)$calc_output
    }
    df <- prices_calc(mechanism = mechanism, coverage = coverage, 
                      mode = 'psa', comparator = comparator_df, psa_number = i)$calc_output
    outcome[1:11,paste0("X",i)] <- df$total_perdose
    outcome[23:33,paste0("X",i)] <- df$percent_share
    if (mechanism!='bilateral'){
      outcome[12:22,paste0("X",i)] <- df$total_savings_perdose
    }
  }
  return(outcome)
}

# get regional data
get_overall_region_data <- function(dataset,bilateral_dataset=NA,
                                    psa_summary=NA, psa_summary_index = NA,
                                    mechanism = NA){
  col_id_pop <- which(names(dataset)=='pop_covered')
  col_id_total <- which(names(dataset)=='total_perdose')
  col_id_share <- which(names(dataset)=='percent_share')
  col_id <- 0
  
  if (is.data.frame(bilateral_dataset)){
    col_id <- which(names(dataset)=='total_savings_perdose')
  }
  new_row <- character(ncol(dataset))
  new_row[1] <- "SEA"
  new_row[col_id_pop] <- sum(dataset$pop_covered)
  #new_row[col_id_share] <- sum(dataset$percent_share)
  if (!is.data.frame(psa_summary)){
    new_row[col_id_total] <- sum(dataset$total_perdose)
  } else {
    mechanism_name <- paste0(mechanism,'_total')
    new_row[col_id_total] <- psa_summary[mechanism_name,psa_summary_index]
  }
  
  if (col_id==0){
    dataset[nrow(dataset)+1,] <- new_row
  } else{
    new_row_2 <- character(ncol(dataset))
    new_row_2[1] <- "SEA (%)"
    
    if (!is.data.frame(psa_summary)){
      new_row[col_id] <- sum(as.numeric(dataset$total_savings_perdose))
      new_row_2[col_id] <- round(sum(as.numeric(dataset$total_savings_perdose))*100/
                                   sum(as.numeric(bilateral_dataset$total_perdose)),2)
    } else {
      # flipping the order so makes numerical sense
      # if (psa_summary_index=='2.5%'){
      #   psa_summary_index <- '97.5%'
      # } else if (psa_summary_index=='97.5%'){
      #   psa_summary_index <- '2.5%'
      # } else {
      #   psa_summary_index <- '50%'
      # }
      
      mechanism_name <- paste0('savings_',mechanism)
      new_row[col_id] <- psa_summary[mechanism_name,psa_summary_index]
      mechanism_name <- paste0('savings_',mechanism,'_percent')
      new_row_2[col_id] <-  psa_summary[mechanism_name,psa_summary_index]
    }
    dataset[nrow(dataset)+1,] <- new_row
    dataset[nrow(dataset)+1,] <- new_row_2
  }
  return(dataset)
}
