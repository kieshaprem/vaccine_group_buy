load('output/intermediate/main_data.rdata')
source('scripts/0_process_calculations.R')
source('scripts/0_load_mi4a_data.R')
source('scripts/helper_get_prices.R')
#source('scripts/1_examine_psa_results.R')

library(dplyr)
library(countrycode)
library(writexl)

# ------------------------------------------------------------------------------
# Function returns df with price values according to specific mechanism.
# If in PSA mode (psa_summary_index!=NA), reads in price values from psa_mat
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Function is a wrapper for functions to run calculations on expenditure, savings,
# and percent share based on selected coverage and pricing mechanism
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Function retrieves values from country_level_psa_summary for 95% UI ranges on
# expenditure, savings and % share
# ------------------------------------------------------------------------------

# to extract % share ranges from country_level_psa_summary
get_data <- function(summary_df, psa_index, data_type){
  row_id <- which(sapply(rownames(summary_df),function(x){substr(x,5,nchar(x))})==data_type)
  return(summary_df[row_id,psa_index])
}


# ------------------------------------------------------------------------------
# Function is a wrapper for functions to select price and run calculations on 
# expenditure, savings,and percent share based on selected coverage and 
# pricing mechanism
# ------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------
# Function calculates total expenditure, total savings in expenditure and 
# % savings in expenditure to be displayed in table
# ------------------------------------------------------------------------------

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
