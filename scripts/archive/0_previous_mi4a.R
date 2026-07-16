library(readxl)

vaccine_prices <- read_excel('data/SEARO & WPRO data points.xlsx',skip = 8)
summary(vaccine_prices[vaccine_prices$`Procurement Mechanism`=="Bilateral"&
                         vaccine_prices$`Income Group`=="HIC",]$`Price per Dose in USD`)
summary(vaccine_prices[vaccine_prices$`Procurement Mechanism`=="Bilateral"&
                         vaccine_prices$`Income Group`=="UMIC",]$`Price per Dose in USD`)
summary(vaccine_prices[vaccine_prices$`Procurement Mechanism`=="Bilateral"&
                         vaccine_prices$`Income Group`=="LMIC",]$`Price per Dose in USD`)
# ranges are the same as updated data, don't need to update ranges

summary(vaccine_prices[vaccine_prices$Region %in% c('WPRO','SEARO') &
                         vaccine_prices$`Income Group`=="LMIC",]$`Price per Dose in USD`)
