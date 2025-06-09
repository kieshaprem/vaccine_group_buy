adjusted_price <- function(P_min, alpha, gdppc_i){
  return (P_min*(1+alpha*((gdppc_i-gdppc_min)/(gdppc_max-gdppc_min))))
}

alphas_tested <- seq(0,20)
base_price_tested <- c(0.5,1,1.5)
P_min <- 5

bilateral_sens_analysis <- list()
uniform_sens_analysis <- list()
gdp_adj_analysis <- list()

library(countrycode)

for (co in bilateral$Country){
  bilateral_sens_analysis[[co]] <- data.frame(array(dim=c(length(base_price_tested),length(alphas_tested))))
  uniform_sens_analysis[[co]] <- data.frame(array(dim=c(length(base_price_tested),length(alphas_tested))))
  gdp_adj_analysis[[co]] <- data.frame(array(dim=c(length(base_price_tested),length(alphas_tested))))
}

for (co in bilateral$Country){
  temp <- data.frame(array(NA,dim=c(length(base_price_tested),
                                 length(alphas_tested))))
  for (i in 1:length(base_price_tested)){
    for (j in 1:length(alphas_tested)){
      temp[i,j] <- adjusted_price(P_min*base_price_tested[i], 
                                          alphas_tested[j], 
                                          dataset[dataset$Country==co,]$`GDP per capita (2022)`)
    }
  }
  gdp_adj_analysis[[co]] <- temp
}

gdp_adj_sens_analysis <- gdp_adj_analysis

for (co in bilateral$Country){
  temp <- data.frame(array(NA,dim=c(length(base_price_tested),
                                    1)))
  for (i in 1:length(base_price_tested)){
      temp[i,1] <- base_price_tested[i]*uniform[uniform$Country==co,]$perdose
  }
  uniform_sens_analysis[[co]] <- temp
}

for (co in bilateral$Country){
  temp <- data.frame(array(NA,dim=c(length(base_price_tested),
                                    1)))
  for (i in 1:length(base_price_tested)){
    temp[i,1] <- base_price_tested[i]*bilateral[bilateral$Country==co,]$perdose
  }
  bilateral_sens_analysis[[co]] <- temp
}

## Function to get dataframe needed for plotting ###############################
get_percent_share_df <- function(sens_analysis_df,constant_col,alpha){
  analysis <- data.frame(array(NA,dim=c(length(bilateral$Country),5)))
  colnames(analysis) <- c("Country","Population","percent_share_min",
                                    "percent_share_median",
                                    "percent_share_max")
  analysis$Country <- bilateral$Country
  analysis$Population <- dataset$`Population (2022)`
  for (co in analysis$Country){
    if (!alpha){
      analysis[analysis$Country==co,]$percent_share_min <- sens_analysis_df[[co]][1,constant_col]*analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
      analysis[analysis$Country==co,]$percent_share_median <- sens_analysis_df[[co]][2,constant_col]*analysis[analysis$Country==co,]$Population
      analysis[analysis$Country==co,]$percent_share_max <- sens_analysis_df[[co]][3,constant_col]*analysis[analysis$Country==co,]$Population
    }
    else{
      if (ncol(sens_analysis_df[[1]])>1){
        analysis[analysis$Country==co,]$percent_share_min <- sens_analysis_df[[co]][constant_col,1]*analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
        analysis[analysis$Country==co,]$percent_share_median <- sens_analysis_df[[co]][constant_col,11]*analysis[analysis$Country==co,]$Population
        analysis[analysis$Country==co,]$percent_share_max <- sens_analysis_df[[co]][constant_col,21]*analysis[analysis$Country==co,]$Population
      }
      else{
        analysis[analysis$Country==co,]$percent_share_min <- sens_analysis_df[[co]][constant_col,1]*analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
        analysis[analysis$Country==co,]$percent_share_median <- sens_analysis_df[[co]][constant_col,1]*analysis[analysis$Country==co,]$Population
        analysis[analysis$Country==co,]$percent_share_max <- sens_analysis_df[[co]][constant_col,1]*analysis[analysis$Country==co,]$Population
        
      }
    }
  }
  analysis$percent_share_min <- analysis$percent_share_min*100/sum(analysis$percent_share_min)
  analysis$percent_share_median <- analysis$percent_share_median*100/sum(analysis$percent_share_median)
  analysis$percent_share_max <- analysis$percent_share_max*100/sum(analysis$percent_share_max)
  return(analysis)
}

bilateral_analysis_price <- get_percent_share_df(bilateral_sens_analysis,constant_col=1,
                                           alpha=FALSE)
uniform_analysis_price <- get_percent_share_df(uniform_sens_analysis,constant_col=1,
                                               alpha = FALSE)
gdp_adj_analysis_price <- get_percent_share_df(gdp_adj_sens_analysis, constant_col = 11,
                                               alpha = FALSE)


gdp_adj_analysis_alpha <- get_percent_share_df(gdp_adj_sens_analysis, constant_col=2,
                                                 alpha=TRUE)





