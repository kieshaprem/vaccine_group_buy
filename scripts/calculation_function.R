load('output/intermediate/main_data.rdata')
source('scripts/0_process_calculations.R')
source('scripts/0_load_mi4a_data.R')
source('scripts/get_prices.R')
library(dplyr)
library(countrycode)
library(writexl)

# objective:
# 0. select mechanism (bilateral, uniform, or tiered), coverage (100% or real) & mode (base or PSA)
# 1. get df for mechanism with all calculated values
# 2. output table in pretty format

# do PSA mode later -> PSA mode will affect price

get_prices <- function(mechanism, mode){
  df <- main_data
  base_alpha <- 10
  if (mechanism == 'bilateral'){
    # input bilateral price
    prices <- get_bilateral(df = df, mode = mode)
  } else if (mechanism == 'uniform'){
    # input uniform price
    prices <- get_uniform(df = df, mode = mode)
  } else if (mechanism == 'adj_gdppc'){
    # input adj_gdppc price
    prices <- get_tiered(df = df, adj = 'gdppc', alpha = base_alpha, mode = mode)
  } else {
    # input adj_chepc price
    prices <- get_tiered(df = df, adj = 'chepc', alpha = base_alpha, mode = mode)
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

prices_calc <- function(mechanism, coverage, mode, comparator){
  prices <- get_prices(mechanism = mechanism, mode = mode)
  output <- list()

  if (mode == 'base'){
    price <- prices[['df']]
    if (mechanism != 'bilateral'){
      comparator <- comparator[['calc_output']]
    }
    output[['calc_output']] <- calc(df = price, coverage = coverage, comparator = comparator, mechanism= mechanism)
  } else {
    price <- prices[['df']]
    if (mechanism != 'bilateral'){
      comparator_min <- comparator[['calc_output_min']]
      comparator_max <- comparator[['calc_output_max']]
    }
    price_min <- prices[['df_min']]
    output[['calc_output_min']] <- calc(df = price_min, coverage = coverage, comparator = comparator_max, mechanism = mechanism)
    price_max <- prices[['df_max']]
    output[['calc_output_max']] <- calc(df = price_max, coverage = coverage, comparator = comparator_min, mechanism = mechanism)
  }
  return (output)
}
  


# get regional data
get_overall_region_data <- function(dataset,bilateral_dataset=NA){
  col_id_pop <- which(names(dataset)=='pop_covered')
  col_id_total <- which(names(dataset)=='total_perdose')
  col_id_share <- which(names(dataset)=='percent_share')
  col_id <- 0
  
  if (is.data.frame(bilateral_dataset)){
    col_id <- which(names(dataset)=='total_savings_perdose')
    print(col_id)
  }
  new_row <- character(ncol(dataset))
  new_row[1] <- "SEA"
  new_row[col_id_pop] <- sum(dataset$pop_covered)
  new_row[col_id_total] <- sum(dataset$total_perdose)
  new_row[col_id_share] <- sum(dataset$percent_share)
  
  if (col_id==0){
    dataset[nrow(dataset)+1,] <- new_row
  } else{
    new_row[col_id] <- sum(as.numeric(dataset$total_savings_perdose))
    print(ncol(dataset)-1)
    new_row_2 <- character(ncol(dataset))
    new_row_2[1] <- "SEA (%)"
    new_row_2[col_id] <- round(sum(as.numeric(dataset$total_savings_perdose))*100/
                                 sum(as.numeric(bilateral_dataset$total_perdose)),2)
    dataset[nrow(dataset)+1,] <- new_row
    dataset[nrow(dataset)+1,] <- new_row_2
  }
  return(dataset)
}

run_scenario <- function(coverage,mode){
  bilateral_output <- prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                  mode = mode, comparator = NA)
  uniform_output <- prices_calc(mechanism = 'uniform', coverage = coverage, 
                                mode = mode, comparator = bilateral_output)
  adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                  mode = mode, comparator = bilateral_output)
  adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                  mode = mode, comparator = bilateral_output)
  
  save(bilateral_output,file = paste0('output/final_output/bilateral_',coverage,'_',mode,'.rdata'))
  save(uniform_output,file = paste0('output/final_output/uniform_',coverage,'_',mode,'.rdata'))
  save(adj_gdppc_output,file = paste0('output/final_output/adj_gdppc_',coverage,'_',mode,'.rdata'))
  save(adj_chepc_output,file = paste0('output/final_output/adj_chepc_',coverage,'_',mode,'.rdata'))
}
