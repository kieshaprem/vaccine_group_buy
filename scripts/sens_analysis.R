library(countrycode)

# Tiered pricing formula
adjusted_price <- function(P_min, alpha, gdppc_i){
  return (P_min*(1+alpha*((gdppc_i-gdppc_min)/(gdppc_max-gdppc_min))))
}
P_min <- 5
gdppc_min <- min(dataset[1:10,]$`GDP per capita (2022)`)
gdppc_max <- max(dataset[1:10,]$`GDP per capita (2022)`)

# Scenarios only for tiered pricing
alphas_tested <- seq(0,20)

# Scenarios for bilateral, uniform and tiered pricing
base_price_tested <- c(0.5,1,1.5)

# Setting up objects to store sensitivity analysis results
bilateral_sens_analysis <- list()
uniform_sens_analysis <- list()
gdp_adj_sens_analysis <- list()

for (co in bilateral$Country){
  bilateral_sens_analysis[[co]] <- data.frame(array(dim=c(length(base_price_tested),length(alphas_tested))))
  uniform_sens_analysis[[co]] <- data.frame(array(dim=c(length(base_price_tested),length(alphas_tested))))
  gdp_adj_sens_analysis[[co]] <- data.frame(array(dim=c(length(base_price_tested),length(alphas_tested))))
}


# Sensitivity analysis for uniform pricing
for (co in bilateral$Country){
  temp <- data.frame(array(NA,dim=c(length(base_price_tested),
                                    1)))
  for (i in 1:length(base_price_tested)){
      temp[i,1] <- base_price_tested[i]*uniform[uniform$Country==co,]$perdose
  }
  uniform_sens_analysis[[co]] <- temp
}

# Sensitivity analysis for bilateral pricing
for (co in bilateral$Country){
  temp <- data.frame(array(NA,dim=c(length(base_price_tested),
                                    1)))
  for (i in 1:length(base_price_tested)){
    temp[i,1] <- base_price_tested[i]*bilateral[bilateral$Country==co,]$perdose
  }
  bilateral_sens_analysis[[co]] <- temp
}

# Sensitivity analysis for tiered pricing
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
  gdp_adj_sens_analysis[[co]] <- temp
}


## Functions to get dataframe needed for plotting ###############################

# Percent share ranges #########################################################
get_percent_share_df <- function(sens_analysis_df,constant_col,alpha){
  analysis <- data.frame(array(NA,dim=c(length(bilateral$Country),5)))
  colnames(analysis) <- c("Country","Population","percent_share_min",
                                    "percent_share_median",
                                    "percent_share_max")
  analysis$Country <- bilateral$Country
  analysis$Population <- dataset[1:10,]$`Population (2022)`
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

# Total expenditure ranges #####################################################
get_total_expenditure <- function(sens_analysis_df,constant_col,alpha){
  analysis <- data.frame(array(NA,dim=c(length(bilateral$Country),5)))
  colnames(analysis) <- c("Country","Population","min_total_expenditure",
                          "median_total_expenditure",
                          "max_total_expenditure")
  analysis$Country <- bilateral$Country
  analysis$Population <- dataset[1:10,]$`Population (2022)`
  for (co in analysis$Country){
    if (!alpha){
      analysis[analysis$Country==co,]$min_total_expenditure <- sens_analysis_df[[co]][1,constant_col]*analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
      analysis[analysis$Country==co,]$median_total_expenditure <- sens_analysis_df[[co]][2,constant_col]*analysis[analysis$Country==co,]$Population
      analysis[analysis$Country==co,]$max_total_expenditure <- sens_analysis_df[[co]][3,constant_col]*analysis[analysis$Country==co,]$Population
    }
    else{
      if (ncol(sens_analysis_df[[1]])>1){
        analysis[analysis$Country==co,]$min_total_expenditure <- sens_analysis_df[[co]][constant_col,1]*analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
        analysis[analysis$Country==co,]$median_total_expenditure <- sens_analysis_df[[co]][constant_col,11]*analysis[analysis$Country==co,]$Population
        analysis[analysis$Country==co,]$max_total_expenditure <- sens_analysis_df[[co]][constant_col,21]*analysis[analysis$Country==co,]$Population
      }
      else{
        analysis[analysis$Country==co,]$min_total_expenditure <- sens_analysis_df[[co]][constant_col,1]*analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
        analysis[analysis$Country==co,]$median_total_expenditure <- sens_analysis_df[[co]][constant_col,1]*analysis[analysis$Country==co,]$Population
        analysis[analysis$Country==co,]$max_total_expenditure <- sens_analysis_df[[co]][constant_col,1]*analysis[analysis$Country==co,]$Population
        
      }
    }
  }
  
  return(analysis)
}


bilateral_analysis_expenditure <- get_total_expenditure(bilateral_sens_analysis,constant_col=1,
                                                 alpha=FALSE)
uniform_analysis_expenditure <- get_total_expenditure(uniform_sens_analysis,constant_col=1,
                                               alpha = FALSE)
gdp_adj_analysis_price_expenditure <- get_total_expenditure(gdp_adj_sens_analysis, constant_col = 11,
                                               alpha = FALSE)


gdp_adj_analysis_alpha_expenditure <- get_total_expenditure(gdp_adj_sens_analysis, constant_col=2,
                                               alpha=TRUE)

# gdp_adj_alphabetical <- gdp_adj_analysis_price_expenditure[order(gdp_adj_analysis_price_expenditure$Country),]
# gdp_adj_alphabetical$price_perdose <- gdp_adj[order(gdp_adj$Country),]$perdose
# gdp_adj_alphabetical <- gdp_adj_alphabetical[,c('Country','Population',
#                                                 'price_perdose','min_total_expenditure',
#                                                 'median_total_expenditure','max_total_expenditure')]
# bilateral_analysis_expenditure <- bilateral_analysis_expenditure[order(bilateral_analysis_expenditure$Country),]

# Getting table for supplementary
gdp_adj_analysis_price_expenditure$min_total_savings <- bilateral_analysis_expenditure$min_total_expenditure-gdp_adj_analysis_price_expenditure$min_total_expenditure
gdp_adj_analysis_price_expenditure$median_total_savings <- bilateral_analysis_expenditure$median_total_expenditure-gdp_adj_analysis_price_expenditure$median_total_expenditure
gdp_adj_analysis_price_expenditure$max_total_savings <- bilateral_analysis_expenditure$max_total_expenditure-gdp_adj_analysis_price_expenditure$max_total_expenditure
gdp_adj_analysis_price_expenditure$price_perdose_max <- NA
gdp_adj_analysis_price_expenditure$price_perdose_min <- NA
gdp_adj_analysis_price_expenditure$price_perdose <- gdp_adj$perdose

for (co in gdp_adj_analysis_price_expenditure$Country){
    gdp_adj_analysis_price_expenditure[gdp_adj_analysis_price_expenditure$Country==co,]$price_perdose_min <- gdp_adj_sens_analysis[[co]][1,11]
    gdp_adj_analysis_price_expenditure[gdp_adj_analysis_price_expenditure$Country==co,]$price_perdose_max <- gdp_adj_sens_analysis[[co]][3,11]
}

gdp_adj_supp_table <- gdp_adj_analysis_price_expenditure[,c('Country','Population','price_perdose',
                                                'price_perdose_min','price_perdose_max',
                                                colnames(gdp_adj_analysis_price_expenditure)[3:8])]

gdp_adj_supp_table$perdose_final <- paste0(round(gdp_adj_supp_table$price_perdose,2)," (",
                                             round(gdp_adj_supp_table$price_perdose_min,2), " - ",
                                             round(gdp_adj_supp_table$price_perdose_max,2),")")

gdp_adj_supp_table$expenditure_final <- paste0(round(gdp_adj_supp_table$median_total_expenditure,2)," (",
                                             round(gdp_adj_supp_table$min_total_expenditure,2), " - ",
                                             round(gdp_adj_supp_table$max_total_expenditure,2),")")

gdp_adj_supp_table$savings_final <- paste0(round(gdp_adj_supp_table$median_total_savings,2)," (",
                                                 round(gdp_adj_supp_table$min_total_savings,2), " - ",
                                                 round(gdp_adj_supp_table$max_total_savings,2),")")

to_save <- gdp_adj_supp_table[,c('Country','perdose_final','expenditure_final','savings_final')]
library(writexl)
write_xlsx(to_save,path='output/gdp_adj_price_table.xlsx')
