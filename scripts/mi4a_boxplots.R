source('scripts/mi4a_boxplot_wrapper_functions.R')

covid_wpro_searo <- vaccine_prices_updated[vaccine_prices_updated$Vaccine=='COVID-19' &
                                             vaccine_prices_updated$Region %in% c('SEARO','WPRO'),]
covid_wpro_searo_noblanks <- covid_wpro_searo[covid_wpro_searo$Price.per.Dose.in.USD!="",]
covid_wpro_searo_numeric <- make_price_numeric(covid_wpro_searo_noblanks)
covid_wpro_searo_numeric[covid_wpro_searo_numeric$Manufacturer==
                           'Beijing Institute of Biological Products Co., Ltd. (CNBG Subsidiary)',]$Manufacturer <- 'BIBP'
covid_wpro_searo_numeric[covid_wpro_searo_numeric$Manufacturer=='Serum Institute of India Pvt. Ltd.',]$Manufacturer <- 'SII'
covid_wpro_searo_numeric[covid_wpro_searo_numeric$Manufacturer=='Janssen Vaccines Corp.',]$Manufacturer <- 'Janssen'
covid_wpro_searo_numeric[covid_wpro_searo_numeric$Manufacturer=='Gamaleya Research Institute',]$Manufacturer <- 'Gamaleya'
covid_wpro_searo_numeric[covid_wpro_searo_numeric$Manufacturer=='Sinovac Biotech Co. Ltd',]$Manufacturer <- 'Sinovac'
covid_wpro_searo_numeric[covid_wpro_searo_numeric$Manufacturer=='',]$Manufacturer <- 'Unknown'
covid_wpro_searo_numeric[covid_wpro_searo_numeric$Manufacturer=='Unknown',]$Manufacturer <- 'Blank'

covid_wpro_searo_numeric[covid_wpro_searo_numeric$Procurement.Mechanism=='COVAX',]$Procurement.Mechanism <- 'Covax'
covid_wpro_searo_numeric[covid_wpro_searo_numeric$Procurement.Mechanism=='Empty',]$Procurement.Mechanism <- 'Blank'

covid_rm_pfizer_moderna <- covid_wpro_searo_numeric[!covid_wpro_searo_numeric$Manufacturer %in% c('Pfizer','Moderna'),]

hic_only <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Income.Group=='HIC',]
umic_only <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Income.Group=='UMIC',]
lmic_only <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Income.Group=='LMIC',]

hic_bilateral <- hic_only[hic_only$Procurement.Mechanism=='Bilateral',]
umic_bilateral <- umic_only[umic_only$Procurement.Mechanism=='Bilateral',]
lmic_bilateral <- lmic_only[lmic_only$Procurement.Mechanism=='Bilateral',]

bilateral_only <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Procurement.Mechanism=='Bilateral',]


lmic_covax <- lmic_only[lmic_only$Procurement.Mechanism=='Covax',]
umic_covax <- umic_only[umic_only$Procurement.Mechanism=='Covax',]
hic_covax <- hic_only[hic_only$Procurement.Mechanism=='Covax',]

all_covax <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Procurement.Mechanism=='Covax',]
all_countries_covax <- unique(all_covax$Country.alias.2025) # 13 countries, 8 of which are LMICs
unique(hic_bilateral[hic_bilateral$Country.alias.2025 %in% all_countries_covax,]$Country.alias.2025) 
#all unique HICs which purchased the vaccine by bilateral also did so by Covax
unique(lmic_bilateral[lmic_bilateral$Country.alias.2025 %in% all_countries_covax,]$Country.alias.2025) 
#1 unique lmic which purchased the vaccine by bilateral also did so by Covax
unique(umic_bilateral[umic_bilateral$Country.alias.2025 %in% all_countries_covax,]$Country.alias.2025) 
#1 unique umic which purchased the vaccine by bilateral also did so by Covax




summary(all_covax$Price.per.Dose.in.USD)
plot_boxplot_price(filename = 'incomegrponly',input_data = covid_wpro_searo_numeric,
                   input_group_col = "Income.Group")
plot_boxplot_price(filename = 'manufactureronly',input_data = covid_wpro_searo_numeric,
                   input_group_col = "Manufacturer")
plot_boxplot_price(filename = 'procurementonly',input_data = covid_wpro_searo_numeric,
                   input_group_col = "Procurement.Mechanism")
# plot_boxplot_price(filename = 'manufacturer_exclude_pfizer_moderna',
#                    input_data = covid_rm_pfizer_moderna,
#                    input_group_col = "Manufacturer")

plot_boxplot_price(filename = 'hic_bilateral',input_data = hic_bilateral,
                   input_group_col = "Manufacturer")
plot_boxplot_price(filename = 'umic_bilateral',input_data = umic_bilateral,
                   input_group_col = "Manufacturer")
plot_boxplot_price(filename = 'lmic_bilateral',input_data = lmic_bilateral,
                   input_group_col = "Manufacturer")
plot_boxplot_price(filename = 'all_covax',input_data = all_covax,
                   input_group_col = "Manufacturer")


wrapper_2layer(filename = 'incomegrp_manufacturer',base_var = 'Income.Group',
               input_data = covid_wpro_searo_numeric,input_group_col = 'Manufacturer')
wrapper_2layer(filename = 'incomegrp_procurementmech',base_var = 'Income.Group',
               input_data = covid_wpro_searo_numeric,input_group_col = 'Procurement.Mechanism')

wrapper_2layer(filename = 'manufacturer_procurementmech',base_var = 'Manufacturer',
               input_data = covid_wpro_searo_numeric,input_group_col = 'Procurement.Mechanism')

wrapper_3layer(filename = 'incomegrp_procurement_manufacturer',
              first_var = 'Income.Group',second_var = 'Procurement.Mechanism',
              input_data = covid_wpro_searo_numeric, input_group_col = 'Manufacturer')






library(ggplot2)
ggplot(bilateral_only,aes(x=Income.Group,y=Price.per.Dose.in.USD,
                                    fill=Manufacturer))+
  geom_boxplot()

