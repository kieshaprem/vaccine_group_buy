# The following codes are for producing Supplementary Figure 2
#
# ------------------------------------------------------------------------------
# Read in data for plotting
# ------------------------------------------------------------------------------
source('scripts/0_load_mi4a_data.R')
source('scripts/plotting/supp2_helper_get_box_plot.R')

covid_wpro_searo <- vaccine_prices_updated[vaccine_prices_updated$Vaccine=='COVID-19' &
                                             vaccine_prices_updated$Region %in% 
                                             c('SEARO','WPRO'),]
covid_wpro_searo_noblanks <- covid_wpro_searo[covid_wpro_searo$Price.per.Dose.in.USD!="",]
covid_wpro_searo_numeric <- make_price_numeric(covid_wpro_searo_noblanks)

# ------------------------------------------------------------------------------
# Code for plottng
# ------------------------------------------------------------------------------
png('output/plots/supp2_mi4a_incomegrp_procurement.png',
    height=10,width=30,res=300, units='cm',pointsize=10)
pushViewport(plotViewport(c(1,1,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3)))
income_grps <- c('LMIC','UMIC','HIC')
proper_names <- c('a) Lower-middle income countries',
                  'b) Upper-middle income countries',
                  'c) High-income countries')
for (column in (1:3)){
  curr_ig <- income_grps[column]
  ig_subdata <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Income.Group==curr_ig,]
  paneller_mi4a_procurement(income_grp = curr_ig,col = column, 
                            subdata = ig_subdata, 
                plot_name = proper_names[column])
}  
dev.off()
