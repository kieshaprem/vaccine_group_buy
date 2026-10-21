source('scripts/1_data_preparation/get_mi4a_data.R')
load( 'output/intermediate/psa_price_base.rdata')


# functions to retrieve vaccine price by mechanism
get_bilateral <- function(df, mode, psa_number=NA, psa_summary = NA, psa_summary_index = NA){
  df$perdose <- numeric(nrow(df))
  output <- list()
  if (mode == 'base'){
    # Base analysis
    df[df$income_grp=="HIC",]$perdose <- round(median(hic_prices_bilateral),2)
    df[df$income_grp=="UMIC",]$perdose <- round(median(umic_prices_bilateral),2)
    df[df$income_grp=="LMIC",]$perdose <- round(median(lmic_prices_bilateral),2)
    output[['df']] <- df
  } else if (mode =='output_psa'){
    # Running outcomes for PSA summary tables
    df[df$income_grp=="HIC",]$perdose <- psa_summary['HIC',psa_summary_index]
    df[df$income_grp=="UMIC",]$perdose <- psa_summary['UMIC',psa_summary_index]
    df[df$income_grp=="LMIC",]$perdose <- psa_summary['LMIC',psa_summary_index]
    output[['df']] <- df
  } else {
    # Running outcomes on prices selected by PSA
    df[which(df$income_grp=="HIC"),]$perdose <- round(psa_mat['HIC', paste0("X",psa_number)],2)
    df[which(df$income_grp=="UMIC"),]$perdose <- round(psa_mat['UMIC', paste0("X",psa_number)],2)
    df[which(df$income_grp=="LMIC"),]$perdose <- round(psa_mat['LMIC', paste0("X",psa_number)],2)
    output[['df']] <- df
  }
  return(output)
}


get_uniform <- function(df, mode, psa_number = NA, psa_summary = NA, psa_summary_index = NA){
  df$perdose <- 0
  output <- list()
  if (mode == 'base'){
    df$perdose <- round(median(wpro_lmic_prices),2)
    output[['df']] <- df
  } else if (mode =='output_psa'){
    df$perdose <- psa_summary['Uniform',psa_summary_index]
    output[['df']] <- df
  } else {
    df$perdose <- round(psa_mat['Uniform', paste0("X",psa_number)],2)
    output[['df']] <- df
  }
  return(output)
}

get_tiered <- function(df, adj, alpha = 10, mode, psa_number = NA, psa_summary = NA, psa_summary_index = NA){
  df$perdose <- 0
  varname <- paste0('relative_',adj)
  df[,varname] <- get_relative_var(df=df,var=adj)
  output <- list()
  if (mode == 'base'){
    pmin <- quantile(wpro_lmic_prices,probs = 0.25)
    df$perdose <- get_adjusted_price(relative_country_income = df[,varname],p_min = pmin, alpha = alpha)
    output[['df']] <- df
  } else if (mode == 'output_psa'){
    pmin <- psa_summary['Pmin',psa_summary_index]
    df$perdose <- get_adjusted_price(relative_country_income = df[,varname],p_min = pmin, alpha = alpha)
    output[['df']] <- df
  } else {
     pmin <- psa_mat['Pmin', paste0("X",psa_number)]
    if ('alpha' %in% rownames(psa_mat)){
      alpha <- psa_mat['alpha',paste0('X',psa_number)]
    }
    df$perdose <- get_adjusted_price(relative_country_income = df[,varname],p_min = pmin, alpha = alpha)
    output[['df']] <- df
  }
  return(output)
}
