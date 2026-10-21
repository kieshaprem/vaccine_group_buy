# ------------------------------------------------------------------------------
# The script reads prices from MI4A database and extracts relevant price for
# COVID-19 vaccination under each required category:
# HIC, UMIC, LMIC prices for WPRO & SEARO under bilateral mechanism
# LMIC prices for WPRO (for uniform pricing)
# ------------------------------------------------------------------------------
# Input: 
# data/who-mi4a-dataset-final-september-2025(Vaccine purchase database).csv
# ------------------------------------------------------------------------------
# Output: 
## Used in base analysis for bilateral mechanism: 
### hic_prices_bilateral, umic_prices_bilateral, lmic_prices_bilateral
## Used in PSA for bilateral mechanism: 
### hic_prices, umic_prices, lmic_prices
## Used in  both base analysis and PSA for uniform and tiered pricing:
### wpro_lmic_prices
# ------------------------------------------------------------------------------

source('scripts/0_helper_functions/helper_process_calculations.R')

# recoding manufacturer names to short form
vax_prices <- read.csv('data/who-mi4a-dataset-final-september-2025(Vaccine purchase database).csv')
vax_prices[vax_prices$Procurement.Mechanism=='COVAX',]$Procurement.Mechanism <- 'Covax'
vax_prices[vax_prices$Procurement.Mechanism=='',]$Procurement.Mechanism <- 'Blank'

# vax prices under bilateral mechanism (base analysis)
bilateral_base <- vax_prices[vax_prices$Vaccine=='COVID-19'&
                        vax_prices$Procurement.Mechanism %in% c('Bilateral') &
                        vax_prices$Region %in% c('WPRO','SEARO') &
                        vax_prices$Price.per.Dose.in.USD!="",]
hic_prices_bilateral <- bilateral_base[bilateral_base$Income.Group=="HIC",]$Price.per.Dose.in.USD
hic_prices_bilateral <- get_vaccine_price_value(hic_prices_bilateral)

lmic_prices_bilateral <- bilateral_base[bilateral_base$Income.Group=="LMIC",]$Price.per.Dose.in.USD
lmic_prices_bilateral <- get_vaccine_price_value(lmic_prices_bilateral)

umic_prices_bilateral <- bilateral_base[bilateral_base$Income.Group=="UMIC",]$Price.per.Dose.in.USD
umic_prices_bilateral <- get_vaccine_price_value(umic_prices_bilateral)
# since there is only 1 umic price, combine the ranges of hic and lmic for umic instead
umic_prices_bilateral <- c(hic_prices_bilateral, lmic_prices_bilateral)


# vax prices under bilateral mechanism (for PSA)
bilateral_all <- vax_prices[vax_prices$Vaccine=='COVID-19'&
                            vax_prices$Procurement.Mechanism %in% 
                              c('Bilateral','Other') &
                            vax_prices$Region %in% c('WPRO','SEARO') &
                            vax_prices$Price.per.Dose.in.USD!="",]

hic_prices <- bilateral_all[bilateral_all$Income.Group=="HIC",]$Price.per.Dose.in.USD
hic_prices <- get_vaccine_price_value(hic_prices)

lmic_prices <- bilateral_all[bilateral_all$Income.Group=="LMIC",]$Price.per.Dose.in.USD
lmic_prices <- get_vaccine_price_value(lmic_prices)

umic_prices <- bilateral_all[bilateral_all$Income.Group=="UMIC",]$Price.per.Dose.in.USD
umic_prices <- get_vaccine_price_value(umic_prices)
# since there is only 1 umic price, combine the ranges of hic and lmic for umic instead
umic_prices <- c(hic_prices, lmic_prices)
# assume that if lmics were to buy AstraZeneca, they would be priced similarly
lmic_prices[3] <- 5


# vax prices under uniform mechanism
wpro_lmic <- vax_prices[vax_prices$Region=='WPRO' & 
                        vax_prices$Income.Group=="LMIC" &
                        vax_prices$Vaccine=='COVID-19' &
                        vax_prices$Price.per.Dose.in.USD!="",]$Price.per.Dose.in.USD
wpro_lmic_prices <- get_vaccine_price_value(wpro_lmic)
rm(bilateral_all,bilateral_base,wpro_lmic)
