source('scripts/0_load_mi4a_data.R')
source('scripts/plotting/supp2_mi4a_boxplot_wrapper_functions.R')

covid_wpro_searo <- vaccine_prices_updated[vaccine_prices_updated$Vaccine=='COVID-19' &
                                             vaccine_prices_updated$Region %in% c('SEARO','WPRO'),]
covid_wpro_searo_noblanks <- covid_wpro_searo[covid_wpro_searo$Price.per.Dose.in.USD!="",]
covid_wpro_searo_numeric <- make_price_numeric(covid_wpro_searo_noblanks)
#covid_wpro_searo_numeric[covid_wpro_searo_numeric$Procurement.Mechanism=='Other',]$Procurement.Mechanism <- 'Bilateral'

hic_only <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Income.Group=='HIC',]
umic_only <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Income.Group=='UMIC',]
lmic_only <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Income.Group=='LMIC',]

hic_bilateral <- hic_only[hic_only$Procurement.Mechanism %in% c('Bilateral','Other',
                                                                'Self-procurement'),]
umic_bilateral <- umic_only[umic_only$Procurement.Mechanism %in% c('Bilateral','Other'),]
lmic_bilateral <- lmic_only[lmic_only$Procurement.Mechanism %in% c('Bilateral','Other'),]

bilateral_only <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Procurement.Mechanism=='Bilateral',]

#lmic_covax <- lmic_only[lmic_only$Procurement.Mechanism=='Covax',]
#umic_covax <- umic_only[umic_only$Procurement.Mechanism=='Covax',]
#hic_covax <- hic_only[hic_only$Procurement.Mechanism=='Covax',]

# all_covax <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Procurement.Mechanism=='Covax',]
# all_countries_covax <- unique(all_covax$Country.alias.2025) # 13 countries, 8 of which are LMICs
# unique(hic_bilateral[hic_bilateral$Country.alias.2025 %in% all_countries_covax,]$Country.alias.2025) 
# #all unique HICs which purchased the vaccine by bilateral also did so by Covax
# unique(lmic_bilateral[lmic_bilateral$Country.alias.2025 %in% all_countries_covax,]$Country.alias.2025) 
# #1 unique lmic which purchased the vaccine by bilateral also did so by Covax
# unique(umic_bilateral[umic_bilateral$Country.alias.2025 %in% all_countries_covax,]$Country.alias.2025) 
# #1 unique umic which purchased the vaccine by bilateral also did so by Covax
# 
# all_regions_covax <-  vaccine_prices_updated[vaccine_prices_updated$Vaccine=='COVID-19' &
#                                                vaccine_prices_updated$Price.per.Dose.in.USD!='' &
#                                                vaccine_prices_updated$Procurement.Mechanism=='Covax',]
# all_regions_covax_numeric <- make_price_numeric(all_regions_covax)
# all_country_initials <- unique(vaccine_prices_updated$Country.alias.2025)
# for (i in (1:length(all_country_initials))){
#   all_ig <- all_regions_covax_numeric[all_regions_covax_numeric$Country.alias.2025==all_country_initials[i],]$Income.Group
#   if (length(unique(all_ig))>1){
#     print(paste0(">1 classification: ",all_country_initials[i]))
#   }
# } ## both are between LMIC & UMIC
# unique(all_regions_covax_numeric[all_regions_covax_numeric$Income.Group=='LIC',]$Country.alias.2025)
# unique(all_regions_covax_numeric[all_regions_covax_numeric$Income.Group=='LMIC',]$Country.alias.2025)
# unique(all_regions_covax_numeric[all_regions_covax_numeric$Income.Group=='UMIC',]$Country.alias.2025)
# #vaccine_prices_updated[vaccine_prices_updated$Country.alias.2025=='LMIC7252',]$Region
# View(vaccine_prices_updated[vaccine_prices_updated$Country.alias.2025=='UMIC7347',]) #wpro
# View(covid_wpro_searo_numeric[covid_wpro_searo_numeric$Country.alias.2025=='UMIC7347',])
# 
# vaccine_prices_all <- vaccine_prices_updated[vaccine_prices_updated$Price.per.Dose.in.USD!='' &
#                                                vaccine_prices_updated$Vaccine=='COVID-19',]
# vaccine_prices_all <- make_price_numeric(vaccine_prices_all)
# vaccine_prices_all <- vaccine_prices_all[!is.na(vaccine_prices_all$Price.per.Dose.in.USD),]
## WPRO & SEARO ################################################################

plot_boxplot_price(filename = 'incomegrponly',input_data = covid_wpro_searo_numeric,
                   input_group_col = "Income.Group")
plot_boxplot_price(filename = 'incomegrponly_all',input_data = vaccine_prices_all,
                   input_group_col = "Income.Group")

plot_boxplot_price(filename = 'manufactureronly',input_data = covid_wpro_searo_numeric,
                   input_group_col = "Manufacturer")
plot_boxplot_price(filename = 'manufacturer_all',input_data = vaccine_prices_all,
                   input_group_col = "Manufacturer")

plot_boxplot_price(filename = 'procurementonly',input_data = covid_wpro_searo_numeric,
                   input_group_col = "Procurement.Mechanism")
# plot_boxplot_price(filename = 'procurement_all',input_data = vaccine_prices_all,
#                    input_group_col = "Procurement.Mechanism")
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
plot_boxplot_price(filename = 'all_regions_bilateral',
                   input_data = bilateral_all_regions_numeric,
                   input_group_col = "Income.Group")

# All regions ##################################################################
plot_boxplot_price(filename = 'hic_all_bilateral',
                   input_data = hic_bilateral_all,
                   input_group_col = "Manufacturer")
plot_boxplot_price(filename = 'umic_all_bilateral',
                   input_data = umic_bilateral_all,
                   input_group_col = "Manufacturer")
plot_boxplot_price(filename = 'lmic_all_bilateral',
                   input_data = lmic_bilateral_all,
                   input_group_col = "Manufacturer")
plot_boxplot_price(filename = 'lic_all_bilateral',
                   input_data = lic_bilateral_all,
                   input_group_col = "Manufacturer")
plot_boxplot_price(filename = 'all_regions_covax',
                   input_data = all_regions_covax_numeric,
                   input_group_col = "Manufacturer")

#############################################################################

wrapper_2layer(filename = 'incomegrp_manufacturer',base_var = 'Income.Group',
               input_data = covid_wpro_searo_numeric,input_group_col = 'Manufacturer')
wrapper_2layer(filename = 'incomegrp_procurementmech',base_var = 'Income.Group',
               input_data = covid_wpro_searo_numeric,input_group_col = 'Procurement.Mechanism')

wrapper_2layer(filename = 'ig_manuf_all',base_var = 'Income.Group',
               input_data = vaccine_prices_all,input_group_col = 'Manufacturer')
wrapper_2layer(filename = 'ig_procure_all',base_var = 'Income.Group',
               input_data = vaccine_prices_all,input_group_col = 'Procurement.Mechanism')


wrapper_2layer(filename = 'manufacturer_procurementmech',base_var = 'Manufacturer',
               input_data = covid_wpro_searo_numeric,input_group_col = 'Procurement.Mechanism')

wrapper_3layer(filename = 'incomegrp_procurement_manufacturer',
              first_var = 'Income.Group',second_var = 'Procurement.Mechanism',
              input_data = covid_wpro_searo_numeric, input_group_col = 'Manufacturer')





