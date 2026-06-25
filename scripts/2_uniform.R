uniform_updated <- main_sheet[,c('iso3c','pop_size_rounded','gdp_percapita','ooppc',
                                 'coverage_covid_1dose_percent','uniform_price')]
uniform_updated$coverage_covid_1dose_percent <- as.numeric(uniform_updated$coverage_covid_1dose_percent)
uniform_updated$pop_covered <- round(uniform_updated$coverage_covid_1dose_percent/100*uniform_updated$pop_size_rounded)
uniform_updated$total_price <- uniform_updated$pop_covered*uniform_updated$uniform_price
uniform_updated$total_price_in_millions <- uniform_updated$total_price/10^6
uniform_updated$total_price_full_coverage <- uniform_updated$pop_size_rounded*uniform_updated$uniform_price
uniform_updated$total_price_full_coverage_in_millions <- uniform_updated$total_price_full_coverage/10^6
uniform_updated$perdose_gdppc_percent <- uniform_updated$uniform_price*100/uniform_updated$gdp_percapita
uniform_updated$perdose_ooppc_percent <- uniform_updated$uniform_price*100/uniform_updated$ooppc
uniform_updated$percent_share_partial <- uniform_updated$total_price*100/sum(uniform_updated$total_price)
uniform_updated$percent_share_full <- uniform_updated$total_price_full_coverage*100/sum(uniform_updated$total_price_full_coverage)
uniform_updated$total_savings_partial <- bilateral_updated$total_price-uniform_updated$total_price
uniform_updated$total_savings_full <- bilateral_updated$total_price_full_coverage-uniform_updated$total_price_full_coverage
uniform_updated$total_savings_partial_in_millions <- uniform_updated$total_savings_partial/10^6
uniform_updated$total_savings_full_in_millions <- uniform_updated$total_savings_full/10^6
names(uniform_updated)[which(names(uniform_updated)=='uniform_price')] <- 'perdose'
uniform_updated$savings_perdose <- bilateral_updated$perdose-uniform_updated$perdose

save(uniform_updated,file='output/uniform_updated.rdata')


uniform_partial <- uniform_updated[,c('iso3c','perdose','pop_covered',
                                          'total_price','total_price_in_millions','total_savings_full')]
names(uniform_partial)[which(names(uniform_partial)=='total_price')] <- 'total_perdose'
uniform_partial$savings_perdose <- bilateral_updated$perdose-uniform_partial$perdose

uniform_full <- uniform_updated[,c('iso3c','perdose','pop_size_rounded',
                                       'total_price_full_coverage','total_price_full_coverage_in_millions','total_savings_full')]
names(uniform_full)[which(names(uniform_full)=='total_price_full_coverage')] <- 'total_perdose'
uniform_full$savings_perdose <- bilateral_updated$perdose-uniform_full$perdose
names(uniform_full)[which(names(uniform_full)=='total_savings_full')] <- 'total_savings_perdose'

uniform_full <- uniform_full[order(-main_sheet$gdp_percapita),]
