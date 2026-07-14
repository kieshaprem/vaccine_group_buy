uniform_updated <- main_sheet[,c('iso3c','population_size','gdp_percapita','gov_exp',
                                 'coverage_covid_1dose_percent','uniform_price')]
names(uniform_updated)[which(names(uniform_updated)=='uniform_price')] <- 'perdose'
uniform_updated$coverage_covid_1dose_percent <- as.numeric(uniform_updated$coverage_covid_1dose_percent)
uniform_updated$savings_perdose <- bilateral_updated$perdose-uniform_updated$perdose
uniform_updated$perdose_gdppc_percent <- uniform_updated$perdose*100/uniform_updated$gdp_percapita
uniform_updated$perdose_gov_exp_percent <- uniform_updated$perdose*100/uniform_updated$gov_exp

# population
uniform_full <- uniform_updated[,c('iso3c','population_size','gdp_percapita','gov_exp','perdose',
                                   'savings_perdose','perdose_gdppc_percent','perdose_gov_exp_percent')]
uniform_real <- uniform_updated[,c('iso3c','population_size','gdp_percapita','gov_exp','perdose',
                                   'savings_perdose','perdose_gdppc_percent','perdose_gov_exp_percent',
                                   'coverage_covid_1dose_percent')]
uniform_real$pop_covered <-round(uniform_real$coverage_covid_1dose_percent/100*uniform_real$population_size)

# total price
uniform_full$total_perdose <- uniform_full$population_size*uniform_full$perdose
uniform_real$total_perdose <- uniform_real$pop_covered*uniform_updated$perdose

# total savings
uniform_full$total_savings_perdose <- bilateral_full$total_perdose-uniform_full$total_perdose
uniform_real$total_savings_perdose <- bilateral_real$total_perdose-uniform_real$total_perdose

# percent share
uniform_full$percent_share <- uniform_full$total_perdose*100/sum(uniform_full$total_perdose)
uniform_real$percent_share <- uniform_real$total_perdose*100/sum(uniform_real$total_perdose)

names(uniform_full)[which(names(uniform_full)=='population_size')] <- 'pop_covered'

uniform_full <- merge(uniform_full,main_sheet[,c('iso3c','chepc')],by.x = 'iso3c',
                        by.y = 'iso3c',all.x = T)
uniform_full$perdose_chepc_percent <- uniform_full$perdose*100/adj_gdppc_full_updated$chepc
uniform_full <- uniform_full[order(uniform_full$gdp_percapita),]


save(uniform_full,file = 'output/uniform_full.rdata')
save(uniform_real,file = 'output/uniform_real.rdata')

