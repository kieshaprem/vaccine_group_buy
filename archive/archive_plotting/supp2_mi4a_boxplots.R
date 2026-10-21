source('scripts/0_load_mi4a_data.R')
source('scripts/plotting/supp2_mi4a_boxplot_wrapper_functions.R')

covid_wpro_searo <- vaccine_prices_updated[vaccine_prices_updated$Vaccine=='COVID-19' &
                                             vaccine_prices_updated$Region %in% c('SEARO','WPRO'),]
covid_wpro_searo_noblanks <- covid_wpro_searo[covid_wpro_searo$Price.per.Dose.in.USD!="",]
covid_wpro_searo_numeric <- make_price_numeric(covid_wpro_searo_noblanks)

plot_compare_ig_procurementmech()
