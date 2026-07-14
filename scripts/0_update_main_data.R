library(readxl)
library(countrycode)
library(writexl)

che <- read_excel('data/SH.XPD.CHEX.PC.CD_SEAcountries + % vaccine coverage.xlsx',sheet = 'SH.XPD.CHEX.PC.CD_SEAcountries')
che <- che[,c('Country Name','Country Code','2023')]
names(che)[2:3] <- c('iso3c','chepc')

vax_coverage <- read_excel('data/SH.XPD.CHEX.PC.CD_SEAcountries + % vaccine coverage.xlsx',sheet = 'Vaccine coverage data (%)')
names(vax_coverage)[2:5] <- c('coverage_covid_1dose_percent',
'coverage_covid_complete_primary_percent','hpv_coverage_percent','uhc_service_utilisation_percent')
vax_coverage <- vax_coverage[1:11,]

pop_2023 <- read.csv('data/API_SP.POP.TOTL_DS2_en_csv_v2_430662/API_SP.POP.TOTL_DS2_en_csv_v2_430662.csv',skip = 4)
pop_2023 <- pop_2023[,c('Country.Name','Country.Code','X2023')]
names(pop_2023) <- c('Country.Name','iso3c','population_size')
pop_2023 <- pop_2023[pop_2023$Country.Name %in% vax_coverage$`Country Name`,]

gdp_2023 <- read.csv('data/GDP/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_428094.csv')
gdp_2023 <- gdp_2023[gdp_2023$Country.Code %in% pop_2023$iso3c,]
gdp_2023 <- gdp_2023[,c('Country.Name','Country.Code','X2023')]
names(gdp_2023)[3] <- 'GDP_2023'

main_sheet <- merge(pop_2023[,c('iso3c','population_size')],
                    gdp_2023,by.x = 'iso3c',by.y = 'Country.Code' )
main_sheet <- main_sheet[,c(3,1,2,4)]
main_sheet$gdp_percapita <- main_sheet$GDP_2023/main_sheet$population_size
main_sheet$relative_gdppc <- (main_sheet$gdp_percapita-min(main_sheet$gdp_percapita))/
  (max(main_sheet$gdp_percapita)-min(main_sheet$gdp_percapita))
main_sheet <- merge(main_sheet,che[,2:3],
                    by.x = 'iso3c',by.y='iso3c')

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
main_sheet <- main_sheet[order(main_sheet$gdp_percapita),]

save(main_sheet,file = 'output/main_sheet.rdata')
write_xlsx(main_sheet,path = 'output/main_sheet.xlsx')

library(readxl)
main_sheet <- read_xlsx('output/main_sheet.xlsx')
gov_expenditure_pc <- read.csv('data/API_SH.XPD.GHED.PC.CD_DS2_en_csv_v2_8655/API_SH.XPD.GHED.PC.CD_DS2_en_csv_v2_8655.csv',skip=4)
gov_expenditure_pc <- gov_expenditure_pc[gov_expenditure_pc$Country.Code %in% bilateral_full$iso3c,]
gov_expenditure_pc <- gov_expenditure_pc[,c('Country.Code','X2023')]
names(gov_expenditure_pc)[2] <- 'gov_exp'

main_sheet <- merge(main_sheet,gov_expenditure_pc,by.x = 'iso3c',by.y='Country.Code')
main_sheet <- main_sheet[order(main_sheet$gdp_percapita),]
main_sheet$relative_gov_exp <- (main_sheet$gov_exp-min(main_sheet$gov_exp))/
  (max(main_sheet$gov_exp)-min(main_sheet$gov_exp))

save(main_sheet,file = 'output/main_sheet.rdata')
library(writexl)
write_xlsx(main_sheet,path = 'output/main_sheet.xlsx')

rm(che,dataset,gdp_2023,pop_2023,vax_coverage)
