# P_min <- 5
# alpha <- 10
# gdppc_min <- min(dataset$`GDP per capita (2022)`)
# gdppc_max <- max(dataset$`GDP per capita (2022)`)

adjusted_price <- function(P_min, alpha, gdppc_i){
  return (P_min*(1+alpha*((gdppc_i-gdppc_min)/(gdppc_max-gdppc_min))))
}

#sapply(dataset$`GDP per capita (2022)`, function(x){adjusted_price(P_min, alpha, x)})

################################################################################
sens_analysis_data <- list()

p_min <- 5

alphas_tested <- c(5,10,15,20)

for (i in (1:length(alphas_tested))){
  temp_gdp_adj <- gdp_adj
  temp_gdp_adj$perdose<- sapply(dataset$`GDP per capita (2022)`, 
                                function(x){adjusted_price(p_min, alphas_tested[i], x)})
  temp_gdp_adj$total_perdose <- temp_gdp_adj$perdose*dataset$`Population (2022)`
  temp_gdp_adj$total_percourse <- temp_gdp_adj$total_perdose*2
  temp_gdp_adj$savings_perdose <- bilateral$perdose-temp_gdp_adj$perdose
  temp_gdp_adj$total_savings_perdose <- bilateral$total_perdose-temp_gdp_adj$total_perdose
  temp_gdp_adj$total_savings_percourse <- bilateral$total_percourse-temp_gdp_adj$total_percourse
  temp_gdp_adj$percourse_total_savings_percent <- temp_gdp_adj$total_savings_percourse/bilateral$total_percourse
  temp_gdp_adj$perdose_gdppc_percent <- temp_gdp_adj$perdose*100/dataset$`GDP per capita (2022)`
  
  spline_points <- spline(x=log(x=dataset[1:10,4],base=10),
                          y=temp_gdp_adj$perdose_gdppc_percent, n = 1000) 
  data_points <- list()
  data_points[["alpha"]] <- alphas_tested[i]
  data_points[["p_min"]] <- p_min
  data_points[["df"]] <- temp_gdp_adj
  data_points[["spline_points"]] <- spline_points
  
  sens_analysis_data[[i]] <- data_points
}

