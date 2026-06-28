load('output/main_sheet.rdata')
# Bilateral
bilateral_updated <- main_sheet[,c('iso3c','population_size','gdp_percapita','ooppc',
                                   'coverage_covid_1dose_percent','bilateral_price')]
bilateral_updated$coverage_covid_1dose_percent <- as.numeric(bilateral_updated$coverage_covid_1dose_percent)
bilateral_updated$pop_covered <- round(bilateral_updated$coverage_covid_1dose_percent/100*bilateral_updated$population_size)

bilateral_updated$total_price <- bilateral_updated$pop_covered*bilateral_updated$bilateral_price
bilateral_updated$total_price_in_millions <- bilateral_updated$total_price/10^6

bilateral_updated$total_price_full_coverage <- bilateral_updated$population_size*bilateral_updated$bilateral_price
bilateral_updated$total_price_full_coverage_in_millions <- bilateral_updated$total_price_full_coverage/10^6

bilateral_updated$perdose_gdppc_percent <- bilateral_updated$bilateral_price*100/bilateral_updated$gdp_percapita
bilateral_updated$perdose_ooppc_percent <- bilateral_updated$bilateral_price*100/bilateral_updated$ooppc

bilateral_updated$percent_share_partial <- bilateral_updated$total_price*100/sum(bilateral_updated$total_price)
bilateral_updated$percent_share_full <- bilateral_updated$total_price_full_coverage*100/sum(bilateral_updated$total_price_full_coverage)

names(bilateral_updated)[which(names(bilateral_updated)=='bilateral_price')] <- 'perdose'
bilateral_updated$iso2c <- countrycode(bilateral_updated$iso3c,origin='iso3c',destination = 'iso2c')
#bilateral_updated <- bilateral_updated[order(-main_sheet$gdp_percapita),]

save(bilateral_updated,file = 'output/bilateral_updated.rdata')
load('output/bilateral_updated.rdata')

bilateral_partial <- bilateral_updated[,c('iso3c','perdose','coverage_covid_1dose_percent','pop_covered',
                                          'total_price','total_price_in_millions','gdp_percapita',
                                          'ooppc')]
names(bilateral_partial)[which(names(bilateral_partial)=='total_price')] <- 'total_perdose'

bilateral_full <- bilateral_updated[,c('iso3c','perdose','population_size',
                                       'total_price_full_coverage','total_price_full_coverage_in_millions',
                                       'gdp_percapita','ooppc')]
names(bilateral_full)[which(names(bilateral_full)=='total_price_full_coverage')] <- 'total_perdose'
library(countrycode)
bilateral_full$iso2c <- countrycode(bilateral_full$iso3c,origin = 'iso3c',destination = 'iso2c')
names(bilateral_full)[which(names(bilateral_full)=='population_size')] <- 'pop_covered'
