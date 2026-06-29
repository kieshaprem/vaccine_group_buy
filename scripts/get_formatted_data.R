bilateral_sheet <- bilateral_updated
bilateral_sheet <- bilateral_sheet[,c(1:5,7,6,8:16)]
bilateral_sheet <- bilateral_sheet[,c(1:7,10,15,8,14)]
names(bilateral_sheet)[3:4] <- c('GDP per capita','CHE per capita')
names(bilateral_sheet)[5] <- 'COVID 1-dose coverage'
names(bilateral_sheet)[6] <- 'Population covered (COVID 1-dose coverage)'
names(bilateral_sheet)[7] <- 'Price per dose'
names(bilateral_sheet)[8] <- 'Total Expenditure (100% coverage)'
names(bilateral_sheet)[9] <- 'Percent Share (100% coverage)'
names(bilateral_sheet)[10] <- 'Total Expenditure (COVID 1-dose coverage)'
names(bilateral_sheet)[11] <- 'Percent Share (COVID 1-dose coverage)'

uniform_sheet <- uniform_updated
uniform_sheet <- uniform_sheet[,c(1:7,20,10,17,15,8,18,14)]
names(uniform_sheet)[3:14] <- c('GDP per capita','CHE per capita',
                                'COVID 1-dose coverage',
                                'Population covered (COVID 1-dose coverage)',
                                'Price per dose',
                                'Savings per dose',
                                'Total Expenditure (100% coverage)',
                                'Total Savings in Expenditure (100% coverage)',
                                'Percent Share (100% coverage)',
                                'Total Expenditure (COVID 1-dose coverage)',
                                'Total Savings in Expenditure (COVID 1-dose coverage)',
                                'Percent Share (COVID 1-dose coverage)')


gdp_adj_sheet <- adj_updated_gdppc
gdp_adj_sheet <- gdp_adj_sheet[,c(1:7,9,22,12,19,17,11,18,16)]
names(gdp_adj_sheet)[3:15] <- c('GDP per capita','CHE per capita',
                                'COVID 1-dose coverage',
                                'Population covered (COVID 1-dose coverage)',
                                'Relative GDP per capita',
                                'Price per dose',
                                'Savings per dose',
                                'Total Expenditure (100% coverage)',
                                'Total Savings in Expenditure (100% coverage)',
                                'Percent Share (100% coverage)',
                                'Total Expenditure (COVID 1-dose coverage)',
                                'Total Savings in Expenditure (COVID 1-dose coverage)',
                                'Percent Share (COVID 1-dose coverage)')

che_adj_sheet <- adj_updated
che_adj_sheet <- che_adj_sheet[,c(1:6,8,9,22,12,19,17,10,18,16)]
names(che_adj_sheet)[3:15] <- c('GDP per capita','CHE per capita',
                                'COVID 1-dose coverage',
                                'Population covered (COVID 1-dose coverage)',
                                'Relative CHE per capita',
                                'Price per dose',
                                'Savings per dose',
                                'Total Expenditure (100% coverage)',
                                'Total Savings in Expenditure (100% coverage)',
                                'Percent Share (100% coverage)',
                                'Total Expenditure (COVID 1-dose coverage)',
                                'Total Savings in Expenditure (COVID 1-dose coverage)',
                                'Percent Share (COVID 1-dose coverage)')
library(writexl)
write_xlsx(list(bilateral=bilateral_sheet,
                uniform=uniform_sheet,
                gdp_adj=gdp_adj_sheet,
                che_adj=che_adj_sheet),path = 'output/modelling_data.xlsx')
