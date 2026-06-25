library(readxl)
dataset = read_excel('data/Scenarios for regionalizing vaccine access in ASEAN_Jun24_updatedNov24.xlsx',sheet = '#4 Weighted by GDP')
names(dataset)[2:6] = dataset[1,2:6]
names(dataset)[8:9] = dataset[1,8:9]
names(dataset)[11:12] = dataset[1,11:12]
names(dataset)[14:17] = dataset[1,14:17]
names(dataset)[19:25] = dataset[1,19:25]
names(dataset)[27:33] = dataset[1,27:33]
dataset = dataset[2:12,]
dataset = cbind(dataset[,1],sapply(dataset[2:33], function(x){as.numeric(x)}))
dataset = as.data.frame(dataset)

save(dataset, file='data/scenarios.RData')

################################################################################

load('data/scenarios.RData')
options(scipen = 1000)
dataset <- dataset[,-3]

#bilateral <- dataset[,c(1,6,7)]
bilateral <- dataset[1:10,c('Country','Bilateral mechanisms*','Total cost per dose')]
colnames(bilateral)[2:3] <- c("perdose","total_perdose")
bilateral$perdose_gdppc_percent <- bilateral$perdose*100/dataset[1:10,]$`GDP per capita (2022)`
bilateral$percent_share <- bilateral$total_perdose*100/sum(bilateral$total_perdose)
bilateral = bilateral[order(dataset[1:10,]$`GDP per capita (2022)`),]

uniform <- dataset[1:10,c('Country','Pooled procurement via ASEAN','Total cost per dose.1',
                      'Total savings per dose')]
colnames(uniform)[2:4] <- c("perdose","total_perdose","total_savings_perdose")
uniform$perdose_gdppc_percent <- uniform$perdose*100/dataset[1:10,]$`GDP per capita (2022)`
uniform$percent_share <- uniform$total_perdose*100/sum(uniform$total_perdose)
uniform = uniform[order(dataset[1:10,]$`GDP per capita (2022)`),]

# gdp_naive <- dataset[,c(1,17,18,19,20,21,22,23,24)]
# colnames(gdp_naive)[2:9] <- c("perdose","total_perdose","total_percourse","savings_perdose",
#                             "total_savings_perdose","total_savings_percourse","percourse_total_savings_percent",
#                             "perdose_gdppc_percent")
# gdp_naive$perdose_gdppc_percent <- gdp_naive$perdose*100/dataset$`GDP per capita (2022)`

gdp_adj <- dataset[1:10,c('Country','Weighted by GDP (formula 2)',
                      'Total cost per dose.3','Total savings per dose.2')]
colnames(gdp_adj)[2:4] <- c("perdose","total_perdose","total_savings_perdose")
gdp_adj$perdose_gdppc_percent <- gdp_adj$perdose*100/dataset[1:10,]$`GDP per capita (2022)`
gdp_adj$percent_share <- gdp_adj$total_perdose*100/sum(gdp_adj$total_perdose)
gdp_adj = gdp_adj[order(dataset[1:10,]$`GDP per capita (2022)`),]

#gdp_naive = gdp_naive[order(dataset$`GDP per capita (2022)`),]
dataset = dataset[order(dataset$`GDP per capita (2022)`),]

################################################################################

spline_gdppc_percent_bilateral <- spline(x=log(x=dataset[1:10,4],base=10),
                               y=bilateral$perdose_gdppc_percent, n = 1000)

spline_gdppc_percent_uniform <- spline(x=log(x=dataset[1:10,4],base=10),
                                       y=uniform$perdose_gdppc_percent, n = 1000)


spline_gdppc_percent_gdp_adj <- spline(x=log(x=dataset[1:10,4],base=10),
                                       y=gdp_adj$perdose_gdppc_percent, n = 1000)

# spline_gdppc_percent_gdp_naive <- spline(x=log(x=dataset[1:10,4],base=10),
#                                        y=gdp_naive$perdose_gdppc_percent, n = 1000)
################################################################################

uniform$total_1dose_percent_gdp <- uniform$total_perdose*100/dataset$`GDP (2022)`

gdp_adj_analysis_alpha <- merge(gdp_adj_analysis_alpha, dataset[,c("Country","GDP per capita (2022)")],
                                by="Country")
gdp_adj_analysis_alpha <- gdp_adj_analysis_alpha[order(-gdp_adj_analysis_alpha$`GDP per capita (2022)`),]

