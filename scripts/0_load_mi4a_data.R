source('scripts/0_process_calculations.R')
vaccine_prices_updated <- read.csv('data/who-mi4a-dataset-final-september-2025(Vaccine purchase database).csv')
vaccine_prices_updated[vaccine_prices_updated$Manufacturer==
                         'Beijing Institute of Biological Products Co., Ltd. (CNBG Subsidiary)',]$Manufacturer <- 'BIBP'
vaccine_prices_updated[vaccine_prices_updated$Manufacturer=='Serum Institute of India Pvt. Ltd.',]$Manufacturer <- 'SII'
vaccine_prices_updated[vaccine_prices_updated$Manufacturer=='Janssen Vaccines Corp.',]$Manufacturer <- 'Janssen'
vaccine_prices_updated[vaccine_prices_updated$Manufacturer=='Gamaleya Research Institute',]$Manufacturer <- 'Gamaleya'
vaccine_prices_updated[vaccine_prices_updated$Manufacturer=='Sinovac Biotech Co. Ltd',]$Manufacturer <- 'Sinovac'
vaccine_prices_updated[vaccine_prices_updated$Manufacturer=='',]$Manufacturer <- 'Unknown'
vaccine_prices_updated[vaccine_prices_updated$Manufacturer=='Unknown',]$Manufacturer <- 'Blank'
vaccine_prices_updated[vaccine_prices_updated$Manufacturer=='Center for Genetic Engineering and Biotechnology',]$Manufacturer <- 'ICGEB'

vaccine_prices_updated[vaccine_prices_updated$Procurement.Mechanism=='COVAX',]$Procurement.Mechanism <- 'Covax'
vaccine_prices_updated[vaccine_prices_updated$Procurement.Mechanism=='',]$Procurement.Mechanism <- 'Blank'

# vax prices under bilateral mechanism (base analysis)
vaccine_prices_bilateral_base <- vaccine_prices_updated[vaccine_prices_updated$Vaccine=='COVID-19'&
                                                     vaccine_prices_updated$Procurement.Mechanism %in% c('Bilateral') &
                                                     vaccine_prices_updated$Region %in% c('WPRO','SEARO') &
                                                     vaccine_prices_updated$Price.per.Dose.in.USD!="",]
hic_prices_bilateral <- vaccine_prices_bilateral_base[vaccine_prices_bilateral_base$Income.Group=="HIC",]$Price.per.Dose.in.USD
hic_prices_bilateral <- get_vaccine_price_value(hic_prices_bilateral)

lmic_prices_bilateral <- vaccine_prices_bilateral_base[vaccine_prices_bilateral_base$Income.Group=="LMIC",]$Price.per.Dose.in.USD
lmic_prices_bilateral <- get_vaccine_price_value(lmic_prices_bilateral)

umic_prices_bilateral <- vaccine_prices_bilateral_base[vaccine_prices_bilateral_base$Income.Group=="UMIC",]$Price.per.Dose.in.USD
umic_prices_bilateral <- get_vaccine_price_value(umic_prices_bilateral)
# since there is only 1 umic price, combine the ranges of hic and lmic for umic instead
umic_prices_bilateral <- c(hic_prices_bilateral, lmic_prices_bilateral)


# vax prices under bilateral mechanism (for PSA)
vaccine_prices_bilateral <- vaccine_prices_updated[vaccine_prices_updated$Vaccine=='COVID-19'&
                                                     vaccine_prices_updated$Procurement.Mechanism %in% c('Bilateral','Other') &
                                                     vaccine_prices_updated$Region %in% c('WPRO','SEARO') &
                                                     vaccine_prices_updated$Price.per.Dose.in.USD!="",]

hic_prices <- vaccine_prices_bilateral[vaccine_prices_bilateral$Income.Group=="HIC",]$Price.per.Dose.in.USD
hic_prices <- get_vaccine_price_value(hic_prices)

lmic_prices <- vaccine_prices_bilateral[vaccine_prices_bilateral$Income.Group=="LMIC",]$Price.per.Dose.in.USD
lmic_prices <- get_vaccine_price_value(lmic_prices)

umic_prices <- vaccine_prices_bilateral[vaccine_prices_bilateral$Income.Group=="UMIC",]$Price.per.Dose.in.USD
umic_prices <- get_vaccine_price_value(umic_prices)
# since there is only 1 umic price, combine the ranges of hic and lmic for umic instead
umic_prices <- c(hic_prices, lmic_prices)

# assume that if lmics were to buy AstraZeneca, they would be priced similarly
lmic_prices[3] <- 5

# vax prices under uniform mechanism
wpro_lmic <- vaccine_prices_updated[vaccine_prices_updated$Region=='WPRO' & 
                                      vaccine_prices_updated$Income.Group=="LMIC" &
                                      vaccine_prices_updated$Vaccine=='COVID-19' &
                                      vaccine_prices_updated$Price.per.Dose.in.USD!="",]$Price.per.Dose.in.USD
wpro_lmic_prices <- get_vaccine_price_value(wpro_lmic)

wpro_searo_lmic <- vaccine_prices_updated[vaccine_prices_updated$Region %in% c("WPRO","SEARO") & 
                                            vaccine_prices_updated$Income.Group=="LMIC" &
                                            vaccine_prices_updated$Vaccine=='COVID-19' &
                                            vaccine_prices_updated$Price.per.Dose.in.USD!="",]$Price.per.Dose.in.USD
wpro_searo_lmic_prices <- get_vaccine_price_value(wpro_searo_lmic)

rm(vaccine_prices_bilateral,wpro_lmic,wpro_searo_lmic)
