# ------------------------------------------------------------------------------
# The script reads in relevant indicators of the ASEAN countries: 
# iso3code
# Income group according to World Bank Income Group classification,
# Population size in 2023 according to World Bank
# Gross Domestic Product in 2023 according to World Bank
# Current Health Expenditure in 2023 according to World Bank
# % of total population vaccinated with >=1 dose of COVID-19 vaccine as of Dec 31, 2023
# ------------------------------------------------------------------------------
# Input: 
# data/historical_income_grp.xlsx
# data/API_SP.POP.TOTL_DS2_en_csv_v2_430662.csv
# 
# ------------------------------------------------------------------------------
# Output: output/final_output/base_analysis_full.xlsx
# ------------------------------------------------------------------------------

library(readxl)
options(scipen=999)

sea_countries <- c('MMR','TLS','LAO','KHM','PHL','VNM','IDN','THA','MYS','BRN','SGP')
main_data <- data.frame(iso3c=sea_countries)

# countries and their income groups in 2023 according to World Bank income classification
income_grp_database <- read_excel('data/historical_income_grp.xlsx',
                                  sheet = 'Country Analytical History', skip = 5)
names(income_grp_database)[1] <- 'iso3c'
sea_income_grp <- income_grp_database[income_grp_database$iso3c %in% sea_countries,c('iso3c','2023')]
names(sea_income_grp)[2] <- 'income_grp'
sea_income_grp$income_grp <- paste0(sea_income_grp$income_grp,'IC')
main_data <- merge(main_data,sea_income_grp,by.x = 'iso3c',by.y = 'iso3c')

# Population size
pop_2023 <- read.csv('data/API_SP.POP.TOTL_DS2_en_csv_v2_430662.csv',skip = 4)
pop_2023 <- pop_2023[,c('Country.Name','Country.Code','X2023')]
names(pop_2023) <- c('Country.Name','iso3c','population_size')
sea_pop_2023 <- pop_2023[pop_2023$iso3c %in% main_data$iso3c,]
main_data <- merge(main_data,sea_pop_2023,by.x = 'iso3c',by.y = 'iso3c')

# GDP
gdp_2023 <- read.csv('data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_428094.csv')
sea_gdp_2023 <- gdp_2023[gdp_2023$Country.Code %in% main_data$iso3c,]
sea_gdp_2023 <- sea_gdp_2023[,c('Country.Code','X2023')]
names(sea_gdp_2023)[2] <- 'GDP_2023'
main_data <- merge(main_data,sea_gdp_2023,by.x = 'iso3c',by.y = 'Country.Code')

# GDP per capita
main_data$gdppc <- main_data$GDP_2023/main_data$population_size

# CHE per capita
chepc <- read_excel('data/SH.XPD.CHEX.PC.CD_SEAcountries + % vaccine coverage.xlsx',sheet = 'SH.XPD.CHEX.PC.CD_SEAcountries')
chepc <- chepc[,c('Country Code','2023')]
names(chepc)[1:2] <- c('iso3c','chepc')
main_data <- merge(main_data,chepc,by.x = 'iso3c',by.y = 'iso3c')

# real vaccination coverage
vax_coverage <- read_excel('data/SH.XPD.CHEX.PC.CD_SEAcountries + % vaccine coverage.xlsx',sheet = 'Vaccine coverage data (%)')
names(vax_coverage)[2:5] <- c('coverage_covid_1dose_percent',
                              'coverage_covid_complete_primary_percent','hpv_coverage_percent','uhc_service_utilisation_percent')
vax_coverage <- vax_coverage[1:11,]
main_data <- merge(main_data, vax_coverage, by.x = 'Country.Name', by.y='Country Name')
main_data <- main_data[,-1]
main_data <- main_data[order(main_data$gdppc),]

# 
# # GHE per capita
# # ghepc <- read.csv('data/API_SH.XPD.GHED.PC.CD_DS2_en_csv_v2_8655/API_SH.XPD.GHED.PC.CD_DS2_en_csv_v2_8655.csv',skip=4)
# # sea_ghepc <- ghepc[ghepc$Country.Code %in% main_data$iso3c,]
# # sea_ghepc <- ghepc[,c('Country.Code','X2023')]
# # names(sea_ghepc)[2] <- 'ghepc'
# # main_data <- merge(main_data,sea_ghepc,by.x = 'iso3c',by.y = 'Country.Code')
# 
# # order so it looks nice
# main_data <- main_data[,c(1,6,2:5,12,7:(ncol(main_data)-1))]

save(main_data, file = 'output/intermediate/main_data.rdata')
rm(chepc,gdp_2023,income_grp_database,pop_2023,sea_gdp_2023,
   sea_income_grp,sea_pop_2023,vax_coverage)
