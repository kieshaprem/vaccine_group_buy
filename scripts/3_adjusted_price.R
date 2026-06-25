p_min <- 5
alpha <- 10.5
# set alpha as 10.5 if we want total adjusted full coverage = total uniform full coverage

adj_updated <- main_sheet[,c('iso3c','pop_size_rounded','gdp_percapita','relative_gdppc','ooppc','relative_ooppc',
                                 'coverage_covid_1dose_percent')]
adj_updated$coverage_covid_1dose_percent <- as.numeric(adj_updated$coverage_covid_1dose_percent)
adj_updated$pop_covered <- round(adj_updated$coverage_covid_1dose_percent/100*adj_updated$pop_size_rounded)

adj_updated$adjusted_price <- p_min*(1+alpha*adj_updated$relative_ooppc)
                                       
adj_updated$total_price <- adj_updated$pop_covered*adj_updated$adjusted_price
adj_updated$total_price_in_millions <- adj_updated$total_price/10^6
adj_updated$total_price_full_coverage <- adj_updated$pop_size_rounded*adj_updated$adjusted_price
adj_updated$total_price_full_coverage_in_millions <- adj_updated$total_price_full_coverage/10^6
adj_updated$perdose_gdppc_percent <- adj_updated$adjusted_price*100/adj_updated$gdp_percapita
adj_updated$perdose_ooppc_percent <- adj_updated$adjusted_price*100/adj_updated$ooppc
adj_updated$percent_share_partial <- adj_updated$total_price*100/sum(adj_updated$total_price)
adj_updated$percent_share_full <- adj_updated$total_price_full_coverage*100/sum(adj_updated$total_price_full_coverage)

adj_updated$total_savings_partial <- bilateral_updated$total_price-adj_updated$total_price
adj_updated$total_savings_full <- bilateral_updated$total_price_full_coverage-adj_updated$total_price_full_coverage

adj_updated$total_savings_partial_in_millions <- adj_updated$total_savings_partial/10^6
adj_updated$total_savings_full_in_millions <- adj_updated$total_savings_full/10^6
names(adj_updated)[which(names(adj_updated)=='adjusted_price')] <- 'perdose'
adj_updated$savings_perdose <- bilateral_updated$perdose-adj_updated$perdose
  
save(adj_updated,file='output/adj_updated.rdata')

adj_updated_gdppc <- adj_updated
adj_updated_gdppc$adjusted_price <- p_min*(1+alpha*adj_updated_gdppc$relative_gdppc)

adj_updated_gdppc$total_price <- adj_updated_gdppc$pop_covered*adj_updated_gdppc$adjusted_price
adj_updated_gdppc$total_price_in_millions <- adj_updated_gdppc$total_price/10^6
adj_updated_gdppc$total_price_full_coverage <- adj_updated_gdppc$pop_size_rounded*adj_updated_gdppc$adjusted_price
adj_updated_gdppc$total_price_full_coverage_in_millions <- adj_updated_gdppc$total_price_full_coverage/10^6
adj_updated_gdppc$perdose_gdppc_percent <- adj_updated_gdppc$adjusted_price*100/adj_updated_gdppc$gdp_percapita
adj_updated_gdppc$perdose_ooppc_percent <- adj_updated_gdppc$adjusted_price*100/adj_updated_gdppc$ooppc
adj_updated_gdppc$percent_share_partial <- adj_updated_gdppc$total_price*100/sum(adj_updated_gdppc$total_price)
adj_updated_gdppc$percent_share_full <- adj_updated_gdppc$total_price_full_coverage*100/sum(adj_updated_gdppc$total_price_full_coverage)

adj_updated_gdppc$total_savings_partial <- bilateral_updated$total_price-adj_updated_gdppc$total_price
adj_updated_gdppc$total_savings_full <- bilateral_updated$total_price_full_coverage-adj_updated_gdppc$total_price_full_coverage

adj_updated_gdppc$total_savings_partial_in_millions <- adj_updated_gdppc$total_savings_partial/10^6
adj_updated_gdppc$total_savings_full_in_millions <- adj_updated_gdppc$total_savings_full/10^6
names(adj_updated_gdppc)[which(names(adj_updated_gdppc)=='adjusted_price')] <- 'perdose'
adj_updated_gdppc$savings_perdose <- bilateral_updated$perdose-adj_updated_gdppc$perdose

save(adj_updated_gdppc,file='output/adj_updated_gdppc.rdata')

library(writexl)
write_xlsx(list(bilateral_updated,uniform_updated,adj_updated_gdppc,adj_updated),path = 'output/modelling_data.xlsx')


adj_partial <- adj_updated[,c('iso3c','perdose','pop_covered',
                                      'total_price','total_price_in_millions','savings_perdose')]
names(adj_partial)[which(names(adj_partial)=='total_price')] <- 'total_perdose'

adj_full <- adj_updated[,c('iso3c','perdose','pop_size_rounded',
                                   'total_price_full_coverage','total_price_full_coverage_in_millions','savings_perdose')]
names(adj_full)[which(names(adj_full)=='total_price_full_coverage')] <- 'total_perdose'



adj_partial_gdppc <- adj_updated_gdppc[,c('iso3c','perdose','pop_covered',
                              'total_price','total_price_in_millions','savings_perdose')]
names(adj_partial_gdppc)[which(names(adj_partial_gdppc)=='total_price')] <- 'total_perdose'

adj_full_gdppc <- adj_updated_gdppc[,c('iso3c','perdose','pop_size_rounded',
                           'total_price_full_coverage','total_price_full_coverage_in_millions','savings_perdose','total_savings_full')]
names(adj_full_gdppc)[which(names(adj_full_gdppc)=='total_price_full_coverage')] <- 'total_perdose'
names(adj_full_gdppc)[which(names(adj_full_gdppc)=='total_savings_full')] <- 'total_savings_perdose'

adj_full_gdppc <- adj_full_gdppc[order(-main_sheet$gdp_percapita),]
