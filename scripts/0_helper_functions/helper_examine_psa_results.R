# ------------------------------------------------------------------------------
# Function combines summaries for prices sampled by PSA and outcomes on expenditure, 
# savings, percent share so we can get median (95% UI)
# ------------------------------------------------------------------------------
get_psa_summary <- function(coverage){
  price_filename <- 'output/intermediate/psa_price_base.rdata'
  load(price_filename) # object name is psa_mat
  
  if (coverage == 'full'){
    outcomes_filename <- 'output/intermediate/psa_outcomes_base.rdata'
    load(outcomes_filename) #object name is full_outcome_base
    psa_outcomes_matrix <- full_outcome_base
  } else {
    outcomes_filename <- 'output/intermediate/psa_outcomes_real.rdata'
    load(outcomes_filename) #object name is real_outcome_base
    psa_outcomes_matrix <- real_outcome_base
  }
  psa_summary <- data.frame(array(dim=c(nrow(psa_mat)+nrow(psa_outcomes_matrix),3)))
  rownames(psa_summary) <- c(rownames(psa_mat),rownames(psa_outcomes_matrix))
  colnames(psa_summary) <- c('2.5%','50%','97.5%')
  
  for (var in 1:length(rownames(psa_summary))){
    varname <-  rownames(psa_summary)[var]
    if (var<=5){
      psa_summary[varname,] <- get_prices_val(psa_mat, measure = varname)
    } else {
      psa_summary[varname,] <- get_prices_val(psa_outcomes_matrix, measure = varname)
    }
  }
  return(psa_summary)
}
# ------------------------------------------------------------------------------
# Function runs country-level calculations on expenditure, savings and percent share 
# using values generated in PSA to get country-level median (95% UI)
# for all pricing mechanisms
# ------------------------------------------------------------------------------
get_country_level_psa_all <- function(psa_mat, coverage){
  bilateral_country_level_psa <- run_country_level_psa(psa_mat = psa_mat, 
                                                       mechanism = 'bilateral', 
                                                       coverage = coverage)
  uniform_country_level_psa <- run_country_level_psa(psa_mat = psa_mat, 
                                                     mechanism = 'uniform', 
                                                     coverage = coverage)
  adj_gdppc_country_level_psa <- run_country_level_psa(psa_mat = psa_mat, 
                                                       mechanism = 'adj_gdppc', 
                                                       coverage = coverage)
  adj_chepc_country_level_psa <- run_country_level_psa(psa_mat = psa_mat, 
                                                       mechanism = 'adj_chepc', 
                                                       coverage = coverage)
  
  outcome_list <- list('bilateral'=bilateral_country_level_psa,
                       'uniform'=uniform_country_level_psa,
                       'adj_gdppc'=adj_gdppc_country_level_psa,
                       'adj_chepc'=adj_chepc_country_level_psa)
  
  return(outcome_list)
}

# ------------------------------------------------------------------------------
# Function summarises country-level calculations on expenditure, savings and 
# percent share using values generated in PSA to get country-level median 
# (95% UI) for a specific pricing scenario
# ------------------------------------------------------------------------------
get_psa_summary_country_level <- function(country_level_psa){
  psa_summary <- data.frame(array(dim=c(nrow(country_level_psa),3)))
  rownames(psa_summary) <- rownames(country_level_psa)
  colnames(psa_summary) <- c('2.5%','50%','97.5%')
  
  for (var in 1:nrow(psa_summary)){
    varname <-  rownames(psa_summary)[var]
    psa_summary[varname,] <- get_prices_val(country_level_psa, measure = varname)
  }
  return(psa_summary)
}

# ------------------------------------------------------------------------------
# summarise country-level calculations on expenditure, savings and percent share 
# using values generated in PSA to get country-level median (95% UI) for all
# pricing scenarios
# ------------------------------------------------------------------------------
get_country_level_psa_summary_all <- function(coverage){
  if (coverage == 'full'){
    load('output/intermediate/psa_countrylevel_base.rdata')
  }
  if (coverage == 'real'){
    load('output/intermediate/psa_countrylevel_real.rdata')
  }
  bilateral_country_summary <- get_psa_summary_country_level(country_level_psa$bilateral)
  uniform_country_summary <- get_psa_summary_country_level(country_level_psa$uniform)
  adj_gdppc_country_summary <- get_psa_summary_country_level(country_level_psa$adj_gdppc)
  adj_chepc_country_summary <- get_psa_summary_country_level(country_level_psa$adj_chepc)
  summary <- list('bilateral'=bilateral_country_summary, 
                  'uniform'=uniform_country_summary,
                  'adj_gdppc'=adj_gdppc_country_summary,
                  'adj_chepc'=adj_chepc_country_summary)
  return(summary)
}

# ------------------------------------------------------------------------------
# Function displays values in median (95% UI) format
# ------------------------------------------------------------------------------
get_psa_range_text <- function(measure, psa_summary){
  return(paste0(psa_summary[measure,'50%'],' (',
                psa_summary[measure,'2.5%'],' - ',
                psa_summary[measure,'97.5%'],')'))
}

# ------------------------------------------------------------------------------
# Function rounds up median (95% UI) prices to 1000s
# ------------------------------------------------------------------------------
get_prices_val <- function(df,measure){
  if (sum(is.na(df[measure,]))==0){
    vals <- as.vector(unlist(df[measure,]))
    quant_vals <- quantile(vals, probs = c(0.025,0.5,0.975))
    quant_vals <- round(quant_vals,2)
    if (quant_vals['97.5%']>1000){
      quant_vals <- round_up_thousands(quant_vals)
    }
    return(quant_vals)
  } else return (NA)
}
