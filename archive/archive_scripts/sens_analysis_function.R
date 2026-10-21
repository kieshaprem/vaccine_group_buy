library(countrycode)

# Tiered pricing formula
adjusted_price <- function(P_min, alpha,relative_country_wealth){
  return (P_min*(1+alpha*relative_country_wealth))
}
P_min <- 5

# Scenarios only for tiered pricing
alphas_tested <- seq(0,20)

# Scenarios for bilateral, uniform and tiered pricing
base_price_tested <- c(0.5,1,1.5)



# Sensitivity analysis for uniform and bilateral pricing
general_sens_analysis <- function(df,price_range=base_price_tested){
  # df should be either bilateral_full, bilateral_real, uniform_full, or uniform_real
  sens_analysis_df <- list()
  for (co in df$iso3c){
    temp <- data.frame(array(NA,dim=c(length(price_range),1)))
    for (i in 1:length(price_range)){
      temp[i,1] <- price_range[i]*df[df$iso3c==co,]$perdose
    }
    sens_analysis_df[[co]] <- temp
  }
  return(sens_analysis_df)
}

# Sensitivity analysis for tiered pricing
tiered_pricing_sens_analysis <- function(df,price_range,alphas_range,P_min,country_wealth_var){
  sens_analysis_df <- list()
  for (co in df$iso3c){
    temp <- data.frame(array(NA,dim=c(length(price_range),
                                      length(alphas_range))))
    for (i in 1:length(price_range)){
      for (j in 1:length(alphas_range)){
        temp[i,j] <- adjusted_price(P_min*price_range[i], 
                                    alphas_range[j], 
                                    df[df$iso3c==co,country_wealth_var])
      }
    }
    sens_analysis_df[[co]] <- temp
  }
  return(sens_analysis_df)
}

tiered_pricing_selection <- function(P_min,price_range,alphas_range,country_wealth_var){
  if (country_wealth_var=='GDPPC'){
    return(tiered_pricing_sens_analysis(df = adj_gdppc_full, P_min = P_min, 
                                        price_range = base_price_tested,
                                        alphas_range = alphas_tested,
                                        country_wealth_var='relative_gdppc'))
  }

  if (country_wealth_var=='gov_exp'){
    return(tiered_pricing_sens_analysis(df = adj_gov_exp_full, P_min = P_min, 
                                        price_range = base_price_tested,
                                        alphas_range = alphas_tested,
                                        country_wealth_var='relative_gov_exp'))
  }
  if (country_wealth_var=='CHEPC'){
    return(tiered_pricing_sens_analysis(df = adj_chepc_full, P_min = P_min, 
                                        price_range = base_price_tested,
                                        alphas_range = alphas_tested,
                                        country_wealth_var='relative_chepc'))
  }
}

## Functions to get percent share ###############################
get_percent_share_df <- function(base_df,sens_analysis_df,constant_col,alpha){
  analysis <- data.frame(array(NA,dim=c(length(base_df$iso3c),5)))
  colnames(analysis) <- c("Country","Population","percent_share_min",
                          "percent_share_median",
                          "percent_share_max")
  analysis$Country <- base_df$iso3c
  analysis$Population <- base_df$pop_covered
  analysis$gdp_percapita <- base_df$gdp_percapita
  
  for (co in analysis$Country){
    if (!alpha){
      analysis[analysis$Country==co,]$percent_share_min <- sens_analysis_df[[co]][1,constant_col]*
        analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
      analysis[analysis$Country==co,]$percent_share_median <- sens_analysis_df[[co]][2,constant_col]*
        analysis[analysis$Country==co,]$Population
      analysis[analysis$Country==co,]$percent_share_max <- sens_analysis_df[[co]][3,constant_col]*
        analysis[analysis$Country==co,]$Population
    }
    else{
      if (ncol(sens_analysis_df[[1]])>1){
        analysis[analysis$Country==co,]$percent_share_min <- sens_analysis_df[[co]][constant_col,1]*
          analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
        analysis[analysis$Country==co,]$percent_share_median <- sens_analysis_df[[co]][constant_col,11]*
          analysis[analysis$Country==co,]$Population
        analysis[analysis$Country==co,]$percent_share_max <- sens_analysis_df[[co]][constant_col,21]*
          analysis[analysis$Country==co,]$Population
      }
      else{
        analysis[analysis$Country==co,]$percent_share_min <- sens_analysis_df[[co]][constant_col,1]*
          analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
        analysis[analysis$Country==co,]$percent_share_median <- sens_analysis_df[[co]][constant_col,1]*
          analysis[analysis$Country==co,]$Population
        analysis[analysis$Country==co,]$percent_share_max <- sens_analysis_df[[co]][constant_col,1]*
          analysis[analysis$Country==co,]$Population
      }
    }
  }
  analysis$percent_share_min <- analysis$percent_share_min*100/sum(analysis$percent_share_min)
  analysis$percent_share_median <- analysis$percent_share_median*100/sum(analysis$percent_share_median)
  analysis$percent_share_max <- analysis$percent_share_max*100/sum(analysis$percent_share_max)
  return(analysis)
}

# for CHEC adjusted and GDPPC adjusted only
# get WCI range
get_wci_range <- function(base_df,sens_analysis_df,constant_col, 
                          wealth_metric,affordability_metric){
  base_df <- base_df[order(base_df[,wealth_metric]),]
  analysis <- data.frame(array(NA,dim=c(length(base_df$iso3c),6)))
  colnames(analysis) <- c("Country","gdp_percapita","gov_exp",
                          "wci_min",
                          "wci_median",
                          "wci_max")
  analysis$Country <- base_df$iso3c
  analysis$gdp_percapita <- base_df$gdp_percapita
  analysis$gov_exp <- base_df$gov_exp
  output_df_list <- list()
  for (i in c(1,11,21)){
    temp_df <- analysis
    temp_df$perdose <- sapply(analysis$Country,
                              function(x){sens_analysis_df[[x]][constant_col,i]})
    temp_df[,affordability_metric] <- temp_df$perdose*100/temp_df[,wealth_metric]
    if (i==1){
      col <- 'wci_min'
    }
    if (i==11){
      col <- 'wci_median'
    }
    if (i==21){
      col <- 'wci_max'
    }
    analysis[,col] <- ci(ineqvar = log(temp_df[,wealth_metric],base=10),
                         outcome = temp_df[,affordability_metric],
                         type = "CIw")$concentration_index
    output_df_list[[col]] <- temp_df
    }
  return(list(analysis,output_df_list))
}

