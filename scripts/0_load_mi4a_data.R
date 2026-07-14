
vaccine_prices_updated <- read.csv('data/who-mi4a-dataset-final-september-2025(Vaccine purchase database).csv')
get_vaccine_price_value <- function(vaccine_prices){
  return(as.numeric(sapply(vaccine_prices,function(x){substr(x,3,nchar(x))})))
}

# getting data for uniform mechanism
wpro_lmic <- vaccine_prices_updated[vaccine_prices_updated$Region=='WPRO' & 
                                         vaccine_prices_updated$Income.Group=="LMIC" &
                                         vaccine_prices_updated$Vaccine=='COVID-19' &
                                      vaccine_prices_updated$Price.per.Dose.in.USD!="",]
wpro_lmic_prices <- wpro_updated_covid$Price.per.Dose.in.USD

wpro_searo_lmic <- vaccine_prices_updated[vaccine_prices_updated$Region %in% c("WPRO","SEARO") & 
                                      vaccine_prices_updated$Income.Group=="LMIC" &
                                      vaccine_prices_updated$Vaccine=='COVID-19' &
                                      vaccine_prices_updated$Price.per.Dose.in.USD!="",]
wpro_searo_lmic_prices <- wpro_updated_covid$Price.per.Dose.in.USD

wpro_lmic_prices <- get_vaccine_price_value(wpro_lmic_prices)
summary(wpro_lmic_prices)
wpro_searo_lmic_prices <- get_vaccine_price_value(wpro_searo_lmic_prices)
summary(wpro_searo_lmic_prices)

# bilateral vax prices
vaccine_prices_bilateral <- vaccine_prices_updated[vaccine_prices_updated$Vaccine=='COVID-19'&
                                                     vaccine_prices_updated$Procurement.Mechanism=='Bilateral' &
                                                     vaccine_prices_updated$Region %in% c('WPRO','SEARO') &
                                                     vaccine_prices_updated$Price.per.Dose.in.USD!="",]

hic_prices <- vaccine_prices_bilateral[vaccine_prices_bilateral$Income.Group=="HIC",]$Price.per.Dose.in.USD
hic_prices <- get_vaccine_price_value(hic_prices)

umic_prices <- vaccine_prices_bilateral[vaccine_prices_bilateral$Income.Group=="UMIC",]$Price.per.Dose.in.USD
umic_prices <- get_vaccine_price_value(umic_prices)
# combine the ranges of hic and lmic for umic
umic_prices <- c(hic_prices, lmic_prices)

lmic_prices <- vaccine_prices_bilateral[vaccine_prices_bilateral$Income.Group=="LMIC",]$Price.per.Dose.in.USD
lmic_prices <- get_vaccine_price_value(lmic_prices)

