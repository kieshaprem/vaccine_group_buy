p_min <- 5
alpha <- 10
# set alpha as 10.5 if we want total adjusted full coverage = total uniform full coverage

adj_updated <- bilateral_updated[,c('iso3c','population_size','gdp_percapita','ooppc',
                                    'coverage_covid_1dose_percent','pop_covered')]
adj_updated <- cbind(adj_updated,relative_gdppc=main_sheet$relative_gdppc,
                     relative_ooppc=main_sheet$relative_ooppc)

adj_updated$perdose <- p_min*(1+alpha*adj_updated$relative_ooppc)

adj_updated$total_price <- adj_updated$pop_covered*adj_updated$perdose
adj_updated$total_price_in_millions <- adj_updated$total_price/10^6
adj_updated$total_price_full_coverage <- adj_updated$population_size*adj_updated$perdose
adj_updated$total_price_full_coverage_in_millions <- adj_updated$total_price_full_coverage/10^6

adj_updated$perdose_gdppc_percent <- adj_updated$perdose*100/adj_updated$gdp_percapita
adj_updated$perdose_ooppc_percent <- adj_updated$perdose*100/adj_updated$ooppc
adj_updated$percent_share_partial <- adj_updated$total_price*100/sum(adj_updated$total_price)
adj_updated$percent_share_full <- adj_updated$total_price_full_coverage*100/sum(adj_updated$total_price_full_coverage)

adj_updated$total_savings_partial <- bilateral_updated$total_price-adj_updated$total_price
adj_updated$total_savings_full <- bilateral_updated$total_price_full_coverage-adj_updated$total_price_full_coverage

adj_updated$total_savings_partial_in_millions <- adj_updated$total_savings_partial/10^6
adj_updated$total_savings_full_in_millions <- adj_updated$total_savings_full/10^6

adj_updated$savings_perdose <- bilateral_updated$perdose-adj_updated$perdose

save(adj_updated,file='output/adj_updated.rdata')

load('output/adj_updated.rdata')

adj_partial <- adj_updated[,c('iso3c','perdose','population_size',
                              'total_price','total_price_in_millions',
                              'savings_perdose','gdp_percapita','ooppc',
                              'relative_gdppc','relative_ooppc','total_savings_partial')]
names(adj_partial)[which(names(adj_partial)=='total_price')] <- 'total_perdose'
names(adj_partial)[which(names(adj_partial)=='total_savings_partial')] <- 'total_savings_perdose'

adj_full <- adj_updated[,c('iso3c','perdose','population_size',
                           'total_price_full_coverage',
                           'total_price_full_coverage_in_millions','savings_perdose',
                           'gdp_percapita','ooppc','relative_gdppc','relative_ooppc',
                           'total_savings_full')]
names(adj_full)[which(names(adj_full)=='total_price_full_coverage')] <- 'total_perdose'
names(adj_full)[which(names(adj_full)=='total_savings_full')] <- 'total_savings_perdose'
names(adj_full)[which(names(adj_full)=='population_size')] <- 'pop_covered'


