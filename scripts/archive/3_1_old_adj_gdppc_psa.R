# PSA (100% coverage)
adj_gdppc_full_min_old <- adj_gdppc_full
adj_gdppc_full_min_old$perdose <- get_adjusted_price(relative_country_income = 
                                                       adj_gdppc_full_min_old$relative_gdppc,
                                                 p_min = min(vaccine_prices[vaccine_prices$Region=='WPRO'&
                                                vaccine_prices$`Income Group`=='LMIC',]$`Price per Dose in USD`),
                                                 alpha = 10)
adj_gdppc_full_min_old$savings_perdose <- bilateral_min_full$perdose-adj_gdppc_full_min_old$perdose
adj_gdppc_full_min_old <- process_mechanism(adj_gdppc_full_min_old,mechanism='adj_gdppc')

adj_gdppc_full_max_old <- adj_gdppc_full
adj_gdppc_full_max_old$perdose <- get_adjusted_price(relative_country_income = 
                                                       adj_gdppc_full_max_old$relative_gdppc,
                                                 p_min = max(vaccine_prices[vaccine_prices$Region=='WPRO'&
                                                vaccine_prices$`Income Group`=='LMIC',]$`Price per Dose in USD`),
                                                 alpha = 10)
adj_gdppc_full_max_old$savings_perdose <- bilateral_max_full$perdose-adj_gdppc_full_max_old$perdose
adj_gdppc_full_max_old <- process_mechanism(adj_gdppc_full_max_old,mechanism='adj_gdppc')

# PSA (COVID-19 2023 coverage)
adj_gdppc_real_min_old <- merge(adj_gdppc_real_min_old[,!colnames(adj_gdppc_real_min_old) %in% c('pop_covered')],
                                bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                                  'pop_covered')],by.x = 'iso3c',
                                by.y='iso3c',all.x = T)

adj_gdppc_real_min_old$perdose <- adj_gdppc_full_min_old$perdose
adj_gdppc_real_min_old$savings_perdose <- adj_gdppc_full_min_old$savings_perdose
adj_gdppc_real_min_old <- process_mechanism(adj_gdppc_real_min_old,mechanism='adj_gdppc')

adj_gdppc_real_max_old <- adj_gdppc_real_min_old
adj_gdppc_real_max_old$perdose <- adj_gdppc_full_max_old$perdose
adj_gdppc_real_max_old$savings_perdose <- adj_gdppc_full_max_old$savings_perdose
adj_gdppc_real_max_old <- process_mechanism(adj_gdppc_real_max_old,mechanism='adj_gdppc')
