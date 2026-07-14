adj_gdppc_updated <- main_sheet[,c('iso3c','population_size','gdp_percapita',
                                   'relative_gdppc','chepc',
                                   'relative_chepc','coverage_covid_1dose_percent')]
adj_gdppc_updated$coverage_covid_1dose_percent <- as.numeric(adj_gdppc_updated$coverage_covid_1dose_percent)
adj_gdppc_updated$perdose <- get_adjusted_price(relative_country_income = adj_gdppc_updated$relative_gdppc,
                                                p_min = quantile(wpro_lmic_prices,probs = 0.25),
                                                alpha = 10)
adj_gdppc_updated$savings_perdose <- round(bilateral_full_updated$perdose-adj_gdppc_updated$perdose,2)
names(adj_gdppc_updated)[which(names(adj_gdppc_updated)=='population_size')] <- 'pop_covered'

adj_gdppc_full_updated <- process_mechanism(adj_gdppc_updated,mechanism = 'adj_gdppc',
                                            bilateral_df = bilateral_full_updated)

adj_gdppc_real_updated <- adj_gdppc_updated
adj_gdppc_real_updated <- merge(adj_gdppc_real_updated[,!colnames(adj_gdppc_real_updated) %in% c('pop_covered')],
                              bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                                'pop_covered')],by.x = 'iso3c',
                              by.y='iso3c',all.x = T)
adj_gdppc_real_updated <- adj_gdppc_real_updated[order(adj_gdppc_real_updated$gdp_percapita),]
adj_gdppc_real_updated <- process_mechanism(adj_gdppc_real_updated,mechanism = 'adj_gdppc',
                                            bilateral_df = bilateral_real_updated)

# PSA (100% coverage)
adj_gdppc_full_min <- adj_gdppc_updated
adj_gdppc_full_min$perdose <- get_adjusted_price(relative_country_income = adj_gdppc_updated$relative_gdppc,
                                         p_min = min(wpro_lmic_prices),
                                         alpha = 10)
adj_gdppc_full_min$savings_perdose <- round(bilateral_min_full$perdose-adj_gdppc_full_min$perdose,2)
adj_gdppc_full_min <- process_mechanism(adj_gdppc_full_min,mechanism='adj_gdppc',
                                        bilateral_df = bilateral_min_full)

adj_gdppc_full_max <- adj_gdppc_updated
adj_gdppc_full_max$perdose <- get_adjusted_price(relative_country_income = adj_gdppc_updated$relative_gdppc,
                                               p_min = max(wpro_lmic_prices),
                                              alpha = 10)
adj_gdppc_full_max$savings_perdose <- round(bilateral_max_full$perdose-adj_gdppc_full_max$perdose,2)
adj_gdppc_full_max <- process_mechanism(adj_gdppc_full_max,mechanism='adj_gdppc',
                                        bilateral_df = bilateral_max_full)

# PSA (COVID-19 2023 coverage)
adj_gdppc_real_min <- adj_gdppc_real_updated
adj_gdppc_real_min$perdose <- adj_gdppc_full_min$perdose
adj_gdppc_real_min$savings_perdose <- adj_gdppc_full_min$savings_perdose
adj_gdppc_real_min <- process_mechanism(adj_gdppc_real_min,mechanism='adj_gdppc',
                                        bilateral_df = bilateral_min_real)
adj_gdppc_real_max <- adj_gdppc_real_updated
adj_gdppc_real_max$perdose <- adj_gdppc_full_max$perdose
adj_gdppc_real_max$savings_perdose <- adj_gdppc_full_max$savings_perdose
adj_gdppc_real_max <- process_mechanism(adj_gdppc_real_max,mechanism='adj_gdppc',
                                        bilateral_df = bilateral_max_real)

save(adj_gdppc_full_min,file = 'output/minadjgdppc_compared_wminbilateral_full.rdata')
save(adj_gdppc_full_max,file = 'output/maxadjgdppc_compared_wmaxbilateral_full.rdata')
save(adj_gdppc_real_min,file = 'output/minadjgdppc_compared_wminbilateral_real.rdata')
save(adj_gdppc_real_max,file = 'output/maxadjgdppc_compared_wmaxbilateral_real.rdata')

load('output/adjgdppc_compared_wminbilateral_full.rdata')
load('output/adjgdppc_compared_wmaxbilateral_full.rdata')
load('output/adjgdppc_compared_wminbilateral_real.rdata')
load('output/adjgdppc_compared_wmaxbilateral_real.rdata')

load('output/minadjgdppc_compared_wbilateral_full.rdata')
#load('output/maxadjgdppc_compared_wbilateral_full.rdata')
load('output/minadjgdppc_compared_wbilateral_real.rdata')
#load('output/maxadjgdppc_compared_wbilateral_real.rdata')