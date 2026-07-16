source('scripts/0_load_mi4a_data.R')
load('output/intermediate/hic_psa.rdata')
load('output/intermediate/umic_psa.rdata')
load('output/intermediate/lmic_psa.rdata')
load('output/intermediate/alternate_psa.rdata')


# functions to retrieve vaccine price by mechanism
get_bilateral <- function(df, mode){
  df$perdose <- 0
  output <- list()
  if (mode == 'base'){
    df[df$income_grp=="HIC",]$perdose <- median(hic_prices)
    df[df$income_grp=="UMIC",]$perdose <- median(umic_prices)
    df[df$income_grp=="LMIC",]$perdose <- median(lmic_prices)
    output[['df']] <- df
  } else {
    df_min <- df
    df_min[df_min$income_grp=="HIC",]$perdose <- min(hic_psa)
    df_min[df_min$income_grp=="UMIC",]$perdose <- min(umic_psa)
    df_min[df_min$income_grp=="LMIC",]$perdose <- min(lmic_psa)
    output[['df_min']] <- df_min
    
    df_max <- df
    df_max[df_max$income_grp=="HIC",]$perdose <- max(hic_psa)
    df_max[df_max$income_grp=="UMIC",]$perdose <- max(umic_psa)
    df_max[df_max$income_grp=="LMIC",]$perdose <- max(lmic_psa)
    output[['df_max']] <- df_max
  }
  return(output)
}


get_uniform <- function(df, mode){
  df$perdose <- 0
  output <- list()
  if (mode == 'base'){
    df$perdose <- median(wpro_lmic_prices)
    output[['df']] <- df
  } else {
    df_min <- df
    df_min$perdose <- min(alternate_psa)
    output[['df_min']] <- df_min
    
    df_max <- df
    df_max$perdose <- max(alternate_psa)
    output[['df_max']] <- df_max
  }
  return(output)
}

get_tiered <- function(df, adj, alpha, mode){
  df$perdose <- 0
  varname <- paste0('relative_',adj)
  df[,varname] <- get_relative_var(df=df,var=adj)
  output <- list()
  if (mode == 'base'){
    pmin <- quantile(wpro_lmic_prices,probs = 0.25)
    df$perdose <- get_adjusted_price(relative_country_income = df[,varname],p_min = pmin, alpha = alpha)
    output[['df']] <- df
  } else {
    df_min <- df
    pmin <- min(alternate_psa)
    df_min$perdose <- get_adjusted_price(relative_country_income = df[,varname],p_min = pmin, alpha = alpha)
    output[['df_min']] <- df_min
    
    df_max <- df
    pmin <- max(alternate_psa)
    df_max$perdose <- get_adjusted_price(relative_country_income = df[,varname],p_min = pmin, alpha = alpha)
    output[['df_max']] <- df_max
  }
  return(output)
}
