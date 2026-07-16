source('scripts/0_process_calculations.R')
adj_updated <- main_sheet[,c('iso3c','population_size','gdp_percapita',
                             'relative_gdppc','gov_exp',
                                 'relative_gov_exp','coverage_covid_1dose_percent')]
adj_updated$coverage_covid_1dose_percent <- as.numeric(adj_updated$coverage_covid_1dose_percent)
p_min <- 5
alpha <- 10

# adjusted by GDPPC
adj_gdppc <- adj_updated
adj_gdppc$perdose <- get_adjusted_price(relative_country_income = adj_gdppc$relative_gdppc,
                                        p_min=p_min, alpha=alpha)
adj_gdppc$savings_perdose <- bilateral_updated$perdose-adj_gdppc$perdose
adj_gdppc$perdose_gdppc_percent <- adj_gdppc$perdose*100/adj_gdppc$gdp_percapita
adj_gdppc$perdose_gov_exp_percent <- adj_gdppc$perdose*100/adj_gdppc$gov_exp

# adjusted by GDPPC, full coverage
adj_gdppc_full <- adj_gdppc[,!colnames(adj_gdppc) %in% c('coverage_covid_1dose_percent')]
adj_gdppc_real <- adj_gdppc
adj_gdppc_real$pop_covered <-round(adj_gdppc_real$coverage_covid_1dose_percent/100*adj_gdppc_real$population_size)
adj_gdppc_real <- adj_gdppc_real[,c('iso3c','population_size',
                                    'coverage_covid_1dose_percent','pop_covered',
                                    colnames(adj_gdppc_real)[!colnames(adj_gdppc_real) %in% c('iso3c','population_size',
                                                                  'coverage_covid_1dose_percent','pop_covered')])]

# total per dose
adj_gdppc_full$total_perdose <- adj_gdppc_full$population_size*adj_gdppc_full$perdose
adj_gdppc_real$total_perdose <- adj_gdppc_real$pop_covered*adj_gdppc_real$perdose

# total savings per dose
adj_gdppc_full$total_savings_perdose <- bilateral_full$total_perdose-adj_gdppc_full$total_perdose
adj_gdppc_real$total_savings_perdose <- bilateral_real$total_perdose-adj_gdppc_real$total_perdose

# percent share
adj_gdppc_full$percent_share <- adj_gdppc_full$total_perdose*100/sum(adj_gdppc_full$total_perdose)
adj_gdppc_real$percent_share <- adj_gdppc_real$total_perdose*100/sum(adj_gdppc_real$total_perdose)

names(adj_gdppc_full)[which(names(adj_gdppc_full)=='population_size')] <- 'pop_covered'

adj_gdppc_full <- merge(adj_gdppc_full,main_sheet[,c('iso3c','chepc')],by.x = 'iso3c',
                      by.y = 'iso3c',all.x = T)
adj_gdppc_full$perdose_chepc_percent <- adj_gdppc_full$perdose*100/adj_gdppc_full$chepc
adj_gdppc_full <- adj_gdppc_full[order(adj_gdppc_full$gdp_percapita),]

save(adj_gdppc_full,file='output/adj_gdppc_full.rdata')
save(adj_gdppc_real,file='output/adj_gdppc_real.rdata')