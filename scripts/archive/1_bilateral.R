load('output/main_sheet.rdata')
# Bilateral
bilateral_updated <- main_sheet[,c('iso3c','population_size','gdp_percapita','gov_exp',
                                   'coverage_covid_1dose_percent','bilateral_price')]
bilateral_updated$coverage_covid_1dose_percent <- as.numeric(bilateral_updated$coverage_covid_1dose_percent)
names(bilateral_updated)[which(names(bilateral_updated)=='bilateral_price')] <- 'perdose'
bilateral_updated$perdose_gdppc_percent <- bilateral_updated$perdose*100/bilateral_updated$gdp_percapita
bilateral_updated$perdose_gov_exp_percent <- bilateral_updated$perdose*100/bilateral_updated$gov_exp


# population
bilateral_full <- bilateral_updated[,c('iso3c','population_size','gdp_percapita','gov_exp','perdose',
                                       'perdose_gdppc_percent','perdose_gov_exp_percent')]
bilateral_real <- bilateral_updated[,c('iso3c','population_size','gdp_percapita','gov_exp','perdose',
                                          'perdose_gdppc_percent','perdose_gov_exp_percent',
                                          'coverage_covid_1dose_percent')]
bilateral_real$pop_covered <- round(bilateral_real$coverage_covid_1dose_percent/100*bilateral_real$population_size)

# total price
bilateral_full$total_perdose <- bilateral_full$population_size*bilateral_full$perdose
bilateral_real$total_perdose <- bilateral_real$pop_covered*bilateral_real$perdose

# percent share
bilateral_full$percent_share <- bilateral_full$total_perdose*100/sum(bilateral_full$total_perdose)
bilateral_real$percent_share <- bilateral_real$total_perdose*100/sum(bilateral_real$total_perdose)

names(bilateral_full)[which(names(bilateral_full)=='population_size')] <- 'pop_covered'

load('output/bilateral_full.rdata')
bilateral_full <- merge(bilateral_full,main_sheet[,c('iso3c','chepc')],by.x = 'iso3c',
                              by.y = 'iso3c',all.x = T)
bilateral_full$perdose_chepc_percent <- bilateral_full$perdose*100/bilateral_full$chepc
bilateral_full <- bilateral_full[order(bilateral_full$gdp_percapita),]

save(bilateral_full,file = 'output/bilateral_full.rdata')
save(bilateral_real,file = 'output/bilateral_real.rdata')
