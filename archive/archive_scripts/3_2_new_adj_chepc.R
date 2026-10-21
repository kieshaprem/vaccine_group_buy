adj_chepc_updated <- main_sheet[,c('iso3c','population_size','gdp_percapita',
                                   'relative_gdppc','chepc',
                                   'relative_chepc','coverage_covid_1dose_percent')]
adj_chepc_updated$coverage_covid_1dose_percent <- as.numeric(adj_chepc_updated$coverage_covid_1dose_percent)
adj_chepc_updated$perdose <- get_adjusted_price(relative_country_income = adj_chepc_updated$relative_chepc,
                                                p_min = quantile(wpro_lmic_prices,probs = 0.25),
                                                alpha = 10)
adj_chepc_updated$savings_perdose <- round(bilateral_full_updated$perdose-adj_chepc_updated$perdose,2)
names(adj_chepc_updated)[which(names(adj_chepc_updated)=='population_size')] <- 'pop_covered'

adj_chepc_full_updated <- process_mechanism(adj_chepc_updated,mechanism = 'adj_chepc',
                                            bilateral_df = bilateral_full_updated)

adj_chepc_real_updated <- adj_chepc_updated
adj_chepc_real_updated <- merge(adj_chepc_real_updated[,!colnames(adj_chepc_real_updated) %in% c('pop_covered')],
                                bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                                  'pop_covered')],by.x = 'iso3c',
                                by.y='iso3c',all.x = T)
adj_chepc_real_updated <- adj_chepc_real_updated[order(adj_chepc_real_updated$gdp_percapita),]
adj_chepc_real_updated <- process_mechanism(df = adj_chepc_real_updated,mechanism = 'adj_chepc',
                                            bilateral_df = bilateral_real_updated)

# PSA (100% coverage)
adj_chepc_full_min <- adj_chepc_updated
adj_chepc_full_min$perdose <- get_adjusted_price(relative_country_income = adj_chepc_updated$relative_chepc,
                                                 p_min = min(wpro_lmic_prices),
                                                 alpha = 10)
adj_chepc_full_min$savings_perdose <- round(bilateral_min_full$perdose-adj_chepc_full_min$perdose,2)
adj_chepc_full_min <- process_mechanism(adj_chepc_full_min,mechanism='adj_chepc',
                                        bilateral_df = bilateral_min_full)

adj_chepc_full_max <- adj_chepc_updated
adj_chepc_full_max$perdose <- get_adjusted_price(relative_country_income = adj_chepc_updated$relative_chepc,
                                               p_min = max(wpro_lmic_prices),
                                                 alpha = 10)
adj_chepc_full_max$savings_perdose <- round(bilateral_max_full$perdose-adj_chepc_full_max$perdose,2)
adj_chepc_full_max <- process_mechanism(adj_chepc_full_max,mechanism='adj_chepc',
                                        bilateral_df = bilateral_max_full)

# PSA (COVID-19 2023 coverage)
adj_chepc_real_min <- adj_chepc_real_updated
adj_chepc_real_min$perdose <- adj_chepc_full_min$perdose
adj_chepc_real_min$savings_perdose <- adj_chepc_full_min$savings_perdose
adj_chepc_real_min <- process_mechanism(adj_chepc_real_min,mechanism='adj_chepc',
                                        bilateral_df = bilateral_min_real)

adj_chepc_real_max <- adj_chepc_real_updated
adj_chepc_real_max$perdose <- adj_chepc_full_max$perdose
adj_chepc_real_max$savings_perdose <- adj_chepc_full_max$savings_perdose
adj_chepc_real_max <- process_mechanism(adj_chepc_real_max,mechanism='adj_chepc',
                                        bilateral_df = bilateral_max_real)

save(adj_chepc_full_min,file = 'output/minadjchepc_compared_wminbilateral_full.rdata')
save(adj_chepc_full_max,file = 'output/maxadjchepc_compared_wmaxbilateral_full.rdata')
save(adj_chepc_real_min,file = 'output/minadjchepc_compared_wminbilateral_real.rdata')
save(adj_chepc_real_max,file = 'output/maxadjchepc_compared_wmaxbilateral_real.rdata')

load('output/adjchepc_compared_wminbilateral_full.rdata')
load('output/adjchepc_compared_wmaxbilateral_full.rdata')
load('output/adjchepc_compared_wminbilateral_real.rdata')
load('output/adjchepc_compared_wmaxbilateral_real.rdata')


load('output/minadjchepc_compared_wbilateral_full.rdata')
#load('output/maxadjchepc_compared_wbilateral_full.rdata')
load('output/minadjchepc_compared_wbilateral_real.rdata')
#load('output/maxadjchepc_compared_wbilateral_real.rdata')