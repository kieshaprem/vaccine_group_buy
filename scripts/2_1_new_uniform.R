source('scripts/0_process_calculations.R')
source('scripts/0_load_mi4a_data.R')
source('scripts/1_1_new_bilateral.R')

uniform_start <- bilateral_start
uniform_start$perdose <- median(wpro_lmic_prices)

uniform_full_updated <- uniform_start
uniform_full_updated$savings_perdose <- bilateral_full_updated$perdose-uniform_full_updated$perdose
uniform_full_updated <- process_mechanism(uniform_full_updated,mechanism = 'uniform',
                                          bilateral_df = bilateral_full_updated)

uniform_real_updated <- uniform_full_updated
uniform_real_updated <- merge(uniform_real_updated[,!colnames(uniform_real_updated) %in% c('pop_covered')],
                                bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                                  'pop_covered')],by.x = 'iso3c',
                                by.y='iso3c',all.x = T)
uniform_real_updated <- uniform_real_updated[order(uniform_real_updated$gdp_percapita),]
uniform_real_updated <- process_mechanism(uniform_real_updated,mechanism = 'uniform',
                                          bilateral_df = bilateral_real_updated)

# PSA ##########################################################################

uniform_min <- uniform_start
uniform_min$perdose <- min(wpro_lmic_prices)
uniform_min$savings_perdose <- bilateral_min_full$perdose-uniform_min$perdose

uniform_max <- uniform_start
uniform_max$perdose <- max(wpro_lmic_prices)
uniform_max$savings_perdose <- bilateral_max_full$perdose-uniform_max$perdose

uniform_min_full <- uniform_min
uniform_max_full <- uniform_max

uniform_min_full <- process_mechanism(uniform_min_full,mechanism = 'uniform',
                                      bilateral_df = bilateral_min_full)
uniform_max_full <- process_mechanism(uniform_max_full,mechanism = 'uniform',
                                      bilateral_df = bilateral_max_full)

uniform_min_real <- uniform_min
uniform_max_real <- uniform_max

uniform_min_real <- merge(uniform_min_real[,!colnames(uniform_min_real) %in% c('pop_covered')],
                          bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                            'pop_covered')],by.x = 'iso3c',
                          by.y='iso3c',all.x = T)
uniform_max_real <- merge(uniform_max_real[,!colnames(uniform_max_real) %in% c('pop_covered')],
                          bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                            'pop_covered')],by.x = 'iso3c',
                          by.y='iso3c',all.x = T)

uniform_min_real <- process_mechanism(uniform_min_real,mechanism = 'uniform',
                                      bilateral_df = bilateral_min_real)
uniform_max_real <- process_mechanism(uniform_max_real,mechanism = 'uniform',
                                      bilateral_df = bilateral_max_real)

uniform_min_real <- uniform_min_real[order(uniform_min_real$gdp_percapita),]
uniform_max_real <- uniform_max_real[order(uniform_max_real$gdp_percapita),]

save(uniform_min_full,file = 'output/minuniform_compared_wminbilateral_full.rdata')
save(uniform_max_full,file = 'output/maxuniform_compared_wmaxbilateral_full.rdata')
save(uniform_min_real,file = 'output/minuniform_compared_wminbilateral_real.rdata')
save(uniform_max_real,file = 'output/minuniform_compared_wmaxbilateral_real.rdata')

load('output/uniform_compared_wminbilateral_full2.rdata')
load('output/uniform_compared_wmaxbilateral_full2.rdata')
load('output/uniform_compared_wminbilateral_real2.rdata')
load('output/uniform_compared_wmaxbilateral_real2.rdata')

load('output/minuniform_compared_wbilateral_full2.rdata')
#load('output/maxuniform_compared_wbilateral_full2.rdata')
load('output/minuniform_compared_wbilateral_real2.rdata')
#load('output/maxuniform_compared_wbilateral_real2.rdata')