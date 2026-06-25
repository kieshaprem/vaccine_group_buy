library(readxl)
library(countrycode)

oop <- read_excel('data/SH.XPD.CHEX.PC.CD_SEAcountries + % vaccine coverage.xlsx',sheet = 'SH.XPD.CHEX.PC.CD_SEAcountries')
oop <- oop[,c('Country Name','Country Code','2023')]
names(oop)[2:3] <- c('iso3c','ooppc')

vax_coverage <- read_excel('data/SH.XPD.CHEX.PC.CD_SEAcountries + % vaccine coverage.xlsx',sheet = 'Vaccine coverage data (%)')
names(vax_coverage)[2:5] <- c('coverage_covid_1dose_percent',
'coverage_covid_complete_primary_percent','hpv_coverage_percent','uhc_service_utilisation_percent')
vax_coverage <- vax_coverage[1:11,]

pop_2023 <- read.csv('data/unpopulation_dataportal_20260623100853.csv')
pop_2023 <- pop_2023[,c('Iso3','Time','Value')]
names(pop_2023) <- c('iso3c','year','population_size')
pop_2023$pop_size_rounded <- round(pop_2023$population_size)
pop_2023 <- pop_2023[pop_2023$year==2023,]

gdp_2023 <- read.csv('data/GDP/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_428094.csv')
gdp_2023 <- gdp_2023[gdp_2023$Country.Code %in% pop_2023$iso3c,]
gdp_2023 <- gdp_2023[,c('Country.Name','Country.Code','X2023')]
names(gdp_2023)[3] <- 'GDP_2023'

main_sheet <- merge(pop_2023[,c('iso3c','pop_size_rounded')],gdp_2023,by.x = 'iso3c',by.y = 'Country.Code' )
main_sheet <- main_sheet[,c(3,1,2,4)]
main_sheet$gdp_percapita <- main_sheet$GDP_2023/main_sheet$pop_size_rounded
main_sheet$relative_gdppc <- (main_sheet$gdp_percapita-min(main_sheet$gdp_percapita))/
  (max(main_sheet$gdp_percapita)-min(main_sheet$gdp_percapita))
main_sheet <- merge(main_sheet,oop[,2:3],
                    by.x = 'iso3c',by.y='iso3c')
main_sheet$relative_ooppc <- (main_sheet$ooppc-min(main_sheet$ooppc))/
  (max(main_sheet$ooppc)-min(main_sheet$ooppc))

main_sheet <- merge(main_sheet,vax_coverage,by.x = 'Country.Name',by.y='Country Name')
main_sheet[main_sheet$hpv_coverage_percent=='Not available',]$hpv_coverage_percent <- NA
main_sheet$hpv_coverage_percent <- as.numeric(main_sheet$hpv_coverage_percent)

load('data/scenarios.RData')
dataset <- dataset[,c('Country','Bilateral mechanisms*',
                      'Pooled procurement via ASEAN')]
dataset <- dataset[1:10,]
dataset$iso3c <- countrycode(dataset$Country,origin='country.name',destination = 'iso3c')
dataset <- dataset[,c(1,4,2,3)]
main_sheet <- merge(main_sheet,dataset[,colnames(dataset)!='Country'],by.x='iso3c',by.y='iso3c',all.x = T)
names(main_sheet)[13:14] <- c('bilateral_price','uniform_price')
main_sheet[main_sheet$iso3c=='TLS',]$bilateral_price <- 9.95
main_sheet[main_sheet$iso3c=='TLS',]$uniform_price <- 7.5
save(main_sheet,file = 'output/main_sheet.rdata')
