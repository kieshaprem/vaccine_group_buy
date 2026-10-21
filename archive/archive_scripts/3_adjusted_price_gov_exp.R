adj_updated <- main_sheet[,c('iso3c','population_size','gdp_percapita','relative_gov_exp','gov_exp',
                             'relative_gov_exp','coverage_covid_1dose_percent')]
adj_updated$coverage_covid_1dose_percent <- as.numeric(adj_updated$coverage_covid_1dose_percent)

get_adjusted_price <- function(relative_country_income,p_min,alpha){
  return(p_min*(1+alpha*relative_country_income))
}
p_min <- 5
alpha <- 10

# adjusted by gov_exp
adj_gov_exp <- adj_updated
adj_gov_exp$perdose <- get_adjusted_price(relative_country_income = adj_gov_exp$relative_gov_exp,
                                        p_min=p_min, alpha=alpha)
adj_gov_exp$savings_perdose <- bilateral_updated$perdose-adj_gov_exp$perdose
adj_gov_exp$perdose_gdppc_percent <- adj_gov_exp$perdose*100/adj_gov_exp$gdp_percapita
adj_gov_exp$perdose_gov_exp_percent <- adj_gov_exp$perdose*100/adj_gov_exp$gov_exp

# adjusted by gov_exp, full coverage
adj_gov_exp_full <- adj_gov_exp[,!colnames(adj_gov_exp) %in% c('coverage_covid_1dose_percent')]
adj_gov_exp_real <- adj_gov_exp
adj_gov_exp_real$pop_covered <-round(adj_gov_exp_real$coverage_covid_1dose_percent/100*adj_gov_exp_real$population_size)
adj_gov_exp_real <- adj_gov_exp_real[,c('iso3c','population_size',
                                    'coverage_covid_1dose_percent','pop_covered',
                                    colnames(adj_gov_exp_real)[!colnames(adj_gov_exp_real) %in% 
                                                               c('iso3c','population_size',
                                                                'coverage_covid_1dose_percent','pop_covered')])]

# total per dose
adj_gov_exp_full$total_perdose <- adj_gov_exp_full$population_size*adj_gov_exp_full$perdose
adj_gov_exp_real$total_perdose <- adj_gov_exp_real$pop_covered*adj_gov_exp_real$perdose

# total savings per dose
adj_gov_exp_full$total_savings_perdose <- bilateral_full$total_perdose-adj_gov_exp_full$total_perdose
adj_gov_exp_real$total_savings_perdose <- bilateral_real$total_perdose-adj_gov_exp_real$total_perdose

# percent share
adj_gov_exp_full$percent_share <- adj_gov_exp_full$total_perdose*100/sum(adj_gov_exp_full$total_perdose)
adj_gov_exp_real$percent_share <- adj_gov_exp_real$total_perdose*100/sum(adj_gov_exp_real$total_perdose)

names(adj_gov_exp_full)[which(names(adj_gov_exp_full)=='population_size')] <- 'pop_covered'


save(adj_gov_exp_full,file='output/adj_gov_exp_full.rdata')
save(adj_gov_exp_real,file='output/adj_gov_exp_real.rdata')