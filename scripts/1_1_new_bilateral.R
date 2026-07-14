# starting table
bilateral_start <- bilateral_full[,c(1:3,5)]
bilateral_start$income_group <- "LMIC"
bilateral_start[which(bilateral_start$perdose==10.00),]$income_group <- "UMIC"
bilateral_start[which(bilateral_start$perdose==15.00),]$income_group <- "HIC"

bilateral_full_updated <- bilateral_start
bilateral_full_updated[bilateral_full_updated$income_group=='LMIC',]$perdose <- median(lmic_prices)
bilateral_full_updated[bilateral_full_updated$income_group=='UMIC',]$perdose <- median(umic_prices)
bilateral_full_updated[bilateral_full_updated$income_group=='HIC',]$perdose <- median(hic_prices)
bilateral_full_updated <- process_mechanism(bilateral_full_updated,mechanism = 'bilateral')

bilateral_real_updated <- bilateral_full_updated
bilateral_real_updated <- merge(bilateral_real_updated[,!colnames(bilateral_real_updated) %in% c('pop_covered')],
                            bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                              'pop_covered')],by.x = 'iso3c',
                            by.y='iso3c',all.x = T)
bilateral_real_updated <- bilateral_real_updated[order(bilateral_real_updated$gdp_percapita),]
bilateral_real_updated <- process_mechanism(bilateral_real_updated,mechanism = 'bilateral',
                                            bilateral_df = NA)

# Min price
bilateral_min <- bilateral_start
bilateral_min[which(bilateral_min$income_group=="LMIC"),]$perdose <- min(lmic_prices)
bilateral_min[which(bilateral_min$income_group=="UMIC"),]$perdose <- min(umic_prices)
bilateral_min[which(bilateral_min$income_group=="HIC"),]$perdose <- min(hic_prices)

# Max price
bilateral_max <- bilateral_start
bilateral_max[which(bilateral_max$income_group=="LMIC"),]$perdose <- max(lmic_prices)
bilateral_max[which(bilateral_max$income_group=="UMIC"),]$perdose <- max(umic_prices)
bilateral_max[which(bilateral_max$income_group=="HIC"),]$perdose <- max(hic_prices)


# Getting data for real coverage
bilateral_min_full <- process_mechanism(bilateral_min,mechanism = 'bilateral',bilateral_df = NA)
bilateral_max_full <- process_mechanism(bilateral_max,mechanism = 'bilateral',bilateral_df = NA)

bilateral_min_real <- bilateral_min
names(bilateral_min_real)[which(names(bilateral_min_real)=='pop_covered')] <- 'population_size'
bilateral_min_real <- merge(bilateral_min_real[,!colnames(bilateral_min_real) %in% c('pop_covered')],
                            bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                              'pop_covered')],by.x = 'iso3c',
                            by.y='iso3c',all.x = T)
bilateral_min_real <- bilateral_min_real[order(bilateral_min_real$gdp_percapita),]
bilateral_min_real <- process_mechanism(bilateral_min_real, mechanism = 'bilateral',bilateral_df = NA)

bilateral_max_real <- bilateral_max
names(bilateral_max_real)[which(names(bilateral_max_real)=='pop_covered')] <- 'population_size'
bilateral_max_real <- merge(bilateral_max_real[,!colnames(bilateral_max_real) %in% c('pop_covered')],
                            bilateral_real[,c('iso3c','coverage_covid_1dose_percent',
                                              'pop_covered')],by.x = 'iso3c',
                            by.y='iso3c',all.x = T)
bilateral_max_real <- bilateral_max_real[order(bilateral_max_real$gdp_percapita),]
bilateral_max_real <- process_mechanism(bilateral_max_real, mechanism = 'bilateral',bilateral_df = NA)
