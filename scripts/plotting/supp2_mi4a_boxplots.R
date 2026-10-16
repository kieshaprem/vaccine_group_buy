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
# Codes to create panelled plot
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
  # Filter data for current income group
  curr_ig <- income_grps[column]
  ig_subdata <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Income.Group==curr_ig,]
  plot_name <- proper_names[column]
  
  # Create plot for each income group
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=1))
  unique_x <- unique(ig_subdata[,"Income.Group"])
  
  # Set up ordering to display Income Group
  if (length(unique(ig_subdata[,"Income.Group"]))==4){
    ordering <- c('LIC','LMIC','UMIC','HIC')
  } else {
    ordering <- c('LMIC','UMIC','HIC')
  }
  x_name <- "Price per dose by Country Income Group"
  
  # Set up ordering to display procurement mechanism
  ordering <- c("Bilateral",
                'Self-procurement',
                "Multilateral",
                "Covax",
                "Donation",
                "UNICEF SD",
                "Other",
                "Blank")
  x_name <- "Price per dose\nby procurement\nmechanism"
  
  # Run function to get income group order for each row in the df
  input_data <- get_selected_grp_order(data = ig_subdata,
                                     group_col = "Procurement.Mechanism",
                                     desired_order = ordering)
  
  # Show y-axis name for first plot in the panel
  if (plot_name=='a) Lower-middle income countries'){
    y_axis_name = "Price per dose (US$)"
    x_name <- ''
  } else {
    y_axis_name <- ''
    x_name <- ''
  }
  
  # Get boxplot
  get_box_plot(data = input_data, group_col = "Procurement.Mechanism", 
             val_col = "Price.per.Dose.in.USD",
             y_axis_name = y_axis_name, x_axis_name = x_name,
             group_order_col = "selected_grp_order",
             bar_colour = "grey80", line_colour = "black",
             y_factor = 0.5,
             n_flag = T,plot_name = plot_name)
  popViewport()
}
dev.off()
