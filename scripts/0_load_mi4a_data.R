source('scripts/0_process_calculations.R')
vaccine_prices_updated <- read.csv('data/who-mi4a-dataset-final-september-2025(Vaccine purchase database).csv')

# vax prices under bilateral mechanism
vaccine_prices_bilateral <- vaccine_prices_updated[vaccine_prices_updated$Vaccine=='COVID-19'&
                                                     vaccine_prices_updated$Procurement.Mechanism=='Bilateral' &
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

rm(vaccine_prices_bilateral,vaccine_prices_updated,wpro_lmic,wpro_searo_lmic)
