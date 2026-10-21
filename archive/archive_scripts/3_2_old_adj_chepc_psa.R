# PSA (100% coverage)
adj_chepc_full_min_old <- adj_chepc_full
adj_chepc_full_min_old$perdose <- get_adjusted_price(relative_country_income = 
                                                       adj_chepc_full_min_old$relative_chepc,
                                                     p_min = min(vaccine_prices[vaccine_prices$Region=='WPRO'&
                                                                                  vaccine_prices$`Income Group`=='LMIC',]$`Price per Dose in USD`),
                                                     alpha = 10)
adj_chepc_full_min_old$savings_perdose <- bilateral_min_full$perdose-adj_chepc_full_min_old$perdose
adj_chepc_full_min_old <- process_mechanism(adj_chepc_full_min_old,mechanism='adj_chepc')

adj_chepc_full_max_old <- adj_chepc_full
adj_chepc_full_max_old$perdose <- get_adjusted_price(relative_country_income = 
                                                       adj_chepc_full_max_old$relative_chepc,
                                                     p_min = max(vaccine_prices[vaccine_prices$Region=='WPRO'&
                                                                                  vaccine_prices$`Income Group`=='LMIC',]$`Price per Dose in USD`),
                                                     alpha = 10)
adj_chepc_full_max_old$savings_perdose <- bilateral_max_full$perdose-adj_chepc_full_max_old$perdose
adj_chepc_full_max_old <- process_mechanism(adj_chepc_full_max_old,mechanism='adj_chepc')

# PSA (COVID-19 2023 coverage)
adj_chepc_real_min_old <- adj_chepc_real_updated
adj_chepc_real_min_old$perdose <- adj_chepc_full_min_old$perdose
adj_chepc_real_min_old$savings_perdose <- adj_chepc_full_min_old$savings_perdose
adj_chepc_real_min_old <- process_mechanism(adj_chepc_real_min_old,mechanism='adj_chepc')

adj_chepc_real_max_old <- adj_chepc_real_updated
adj_chepc_real_max_old$perdose <- adj_chepc_full_max_old$perdose
adj_chepc_real_max_old$savings_perdose <- adj_chepc_full_max_old$savings_perdose
adj_chepc_real_max_old <- process_mechanism(adj_chepc_real_max_old,mechanism='adj_chepc')
