library(readxl)

vgb = read_excel('data/vgb_country.xlsx',sheet = 'data')

head(vgb)

oop_raw = read.csv('data/P_Data_Extract_From_World_Development_Indicators_oop_2021/oop_data.csv')
names(oop_raw)[5:9] = as.character(2019:2023)

oop_all_countries_2021 = oop_raw[,c("Country.Code","2021")]
oop_all_countries_2021 = oop_all_countries_2021[oop_all_countries_2021[,2]!=".." & oop_all_countries_2021[,2]!="",]
oop_all_countries_2021[,2] = as.numeric(oop_all_countries_2021[,2])


asean_iso = vgb$`ISO3C Code`

max(log(x=oop_all_countries_2021[,2], base=2))
min(log(x=oop_all_countries_2021[,2], base=2))


gdp_raw = read.csv('data/P_Data_Extract_From_World_Development_Indicators_gdp_2022/gdp_data.csv')

gdp_all_countries_2022 = gdp_raw[,c(4,5)]
gdp_all_countries_2022 = gdp_all_countries_2022[gdp_all_countries_2022[,2]!=".." & gdp_all_countries_2022[,2]!="",]
gdp_all_countries_2022[,2] = as.numeric(gdp_all_countries_2022[,2])

max(log(x=gdp_all_countries_2022[,2], base=2))
min(log(x=gdp_all_countries_2022[,2], base=2))

gdp_oop <- merge(gdp_all_countries_2022, oop_all_countries_2021, by = "Country.Code")
names(gdp_oop)[2:3] <- c("GDP_2022","OOP_2021")



exp_raw = read.csv('data/P_Data_Extract_From_World_Development_Indicators_total_exp/exp_2021.csv')

exp_all_countries_2021 = exp_raw[,c(2,7)]
exp_all_countries_2021 = exp_all_countries_2021[exp_all_countries_2021[,2]!=".." & exp_all_countries_2021[,2]!="",]
exp_all_countries_2021[,2] = as.numeric(exp_all_countries_2021[,2])

max(log(x=exp_all_countries_2021[,2], base=2))
min(log(x=exp_all_countries_2021[,2], base=2))

gdp_oop_exp <- merge(gdp_oop, exp_all_countries_2021, by = "Country.Code")
names(gdp_oop_exp)[4] <- "EXP_2021"

#################################################################################
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

load('data/scenarios.RData')




