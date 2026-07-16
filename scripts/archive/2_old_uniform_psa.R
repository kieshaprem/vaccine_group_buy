
vaccine_prices <- read_excel('data/SEARO & WPRO data points.xlsx',skip = 8)
summary(vaccine_prices[vaccine_prices$Region=='WPRO'&
                         vaccine_prices$`Income Group`=="LMIC",]$`Price per Dose in USD`)
vaccine_prices_lmic_wpro_old <- vaccine_prices[vaccine_prices$Region=='WPRO'&
                                                 vaccine_prices$`Income Group`=="LMIC",]$`Price per Dose in USD`

uniform_min_old <- uniform_full
uniform_min_old$perdose <- min(vaccine_prices_lmic_wpro_old)
uniform_min_old$savings_perdose <- bilateral_min$perdose-uniform_min_old$perdose

uniform_max_old <- uniform_full
uniform_max_old$perdose <- max(vaccine_prices_lmic_wpro_old)
uniform_max_old$savings_perdose <- bilateral_max$perdose-uniform_max_old$perdose

uniform_min_full_old <- uniform_min_old
uniform_max_full_old <- uniform_max_old

uniform_min_full_old <- process_mechanism(uniform_min_full_old,mechanism = 'uniform')
uniform_max_full_old <- process_mechanism(uniform_max_full_old,mechanism = 'uniform')

uniform_min_real_old <- uniform_min_old
uniform_max_real_old <- uniform_max_old

uniform_min_real_old <- uniform_min_real
uniform_max_real_old <- merge(uniform_max_real_old[,!colnames(uniform_max_real_old) %in% 
                                                 c('pop_covered')],
                          bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                            'pop_covered')],by.x = 'iso3c',
                          by.y='iso3c',all.x = T)

uniform_min_real_old <- process_mechanism(uniform_min_real_old,mechanism = 'uniform')
uniform_max_real_old <- process_mechanism(uniform_max_real_old,mechanism = 'uniform')

uniform_min_real_old <- uniform_min_real_old[order(uniform_min_real_old$gdp_percapita),]
uniform_max_real_old <- uniform_max_real_old[order(uniform_max_real_old$gdp_percapita),]

