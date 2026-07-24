source('scripts/0_load_mi4a_data.R')
#load('output/intermediate/psa_mat_alpha_empirical_full.rdata')
load('output/intermediate/psa_mat_alpha_empirical_others.rdata')
#load('output/intermediate/psa_mat_pmin.rdata')
psa_mat <- psa_mat[1:4,]

# functions to retrieve vaccine price by mechanism
get_bilateral <- function(df, mode, psa_number=NA, psa_summary = NA, psa_summary_index = NA){
  df$perdose <- numeric(nrow(df))
  output <- list()
  if (mode == 'base'){
    df[df$income_grp=="HIC",]$perdose <- round(median(hic_prices),2)
    df[df$income_grp=="UMIC",]$perdose <- round(median(umic_prices),2)
    df[df$income_grp=="LMIC",]$perdose <- round(median(lmic_prices),2)
    output[['df']] <- df
  } else if (mode =='output_psa'){
    df[df$income_grp=="HIC",]$perdose <- psa_summary['HIC',psa_summary_index]
    df[df$income_grp=="UMIC",]$perdose <- psa_summary['UMIC',psa_summary_index]
    df[df$income_grp=="LMIC",]$perdose <- psa_summary['LMIC',psa_summary_index]
    output[['df']] <- df
  } else {
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
    df$perdose <- psa_summary['Alternate',psa_summary_index]
    output[['df']] <- df
  } else {
    df$perdose <- round(psa_mat['Alternate', paste0("X",psa_number)],2)
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
    pmin <- psa_summary['Alternate',psa_summary_index]
    df$perdose <- get_adjusted_price(relative_country_income = df[,varname],p_min = pmin, alpha = alpha)
    output[['df']] <- df
  } else {
     pmin <- psa_mat['Alternate', paste0("X",psa_number)]
    if ('alpha' %in% rownames(psa_mat)){
      alpha <- psa_mat['alpha',paste0('X',psa_number)]
    }
    df$perdose <- get_adjusted_price(relative_country_income = df[,varname],p_min = pmin, alpha = alpha)
    output[['df']] <- df
  }
  return(output)
}
