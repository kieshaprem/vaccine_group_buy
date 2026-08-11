source('scripts/plotting/get_box_plot.R')
source('scripts/plotting/general_helper_plotting_functions.R')

plot_boxplot_price <- function(input_data, input_group_col, plot_name=NA){
  print(plot_name)
  unique_x <- unique(input_data[,input_group_col])
  if (input_group_col=="Income.Group"){
    if (length(unique(input_data[,input_group_col]))==4){
    ordering <- c('LIC','LMIC','UMIC','HIC')
    } else {
      ordering <- c('LMIC','UMIC','HIC')
    }
    x_name <- "Price per dose by Country Income Group"
  }
  if (input_group_col=="Manufacturer"){
    ordering <- c("Blank",
                  'BIBP',
                  "SII",
                  "AstraZeneca",
                  "Janssen",
                  "Gamaleya",
                  "Sinovac",
                  "Moderna", "Pfizer")
    x_name <- "Price per dose by Manufacturer"
  }
  if (input_group_col=="Procurement.Mechanism"){
    ordering <- c("Bilateral",
                  'Self-procurement',
                  "Multilateral",
                  "Covax",
                  "Donation",
                  "UNICEF SD",
                  "Other",
                  "Blank")
    x_name <- "Price per dose\nby procurement\nmechanism"
   }
 input_data <- get_selected_grp_order(data = input_data,
                                       group_col = input_group_col,
                                       desired_order = ordering)
  if (plot_name=='a) Lower-middle income countries'){
    y_axis_name = "Price per dose (US$)"
    x_name <- ''
  } else {
    y_axis_name <- ''
    x_name <- ''
  }
  
  get_box_plot(data = input_data, group_col = input_group_col, 
               val_col = "Price.per.Dose.in.USD",
               y_axis_name = y_axis_name, x_axis_name = x_name,
               group_order_col = "selected_grp_order",
               bar_colour = "grey80", line_colour = "black",
               y_factor = 0.5,
               n_flag = T,plot_name = plot_name)
}

wrapper_2layer <- function(filename,base_var, input_data, input_group_col){
  unique_base_var <- unique(input_data[,base_var])
  for (var in unique_base_var){
    input_filename <- paste0(filename,"_",var)
    subdata <- input_data[input_data[,base_var]==var,]
    plot_boxplot_price(filename = input_filename,input_data = subdata,
                       input_group_col = input_group_col)
  }
}

wrapper_3layer <- function(filename,first_var,second_var,input_data,input_group_col){
  unique_first_var <- unique(input_data[,first_var])
  for (var in unique_base_var){
    input_filename <- paste0(filename,"_",var)
    subdata <- input_data[input_data[,first_var]==var,]
    wrapper_2layer(filename = input_filename,base_var = second_var,
                   input_data = subdata,input_group_col = input_group_col)
  }
}

paneller_mi4a <- function(income_grp, col, subdata, plot_name = NA){
  pushViewport(viewport(layout.pos.col=col,layout.pos.row=1))
  plot_boxplot_price(input_data = subdata, input_group_col = "Procurement.Mechanism",
                     plot_name = plot_name)
  popViewport()
}
  
prefix <- c('a) ','b) ','c) ')

plot_compare_ig_procurementmech <- function(coverage){
  png(paste0('output/plots/final_paper/boxplot_mi4a/panel_ig_procurementmech_',coverage,'.png'),
      height=10,width=30,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3)))
  income_grps <- c('LMIC','UMIC','HIC')
  proper_names <- c('a) Lower-middle income countries','b) Upper-middle income countries',
                    'c) High-income countries')
  for (column in (1:3)){
    curr_ig <- income_grps[column]
    ig_subdata <- covid_wpro_searo_numeric[covid_wpro_searo_numeric$Income.Group==curr_ig,]
    paneller_mi4a(income_grp = curr_ig,col = column, subdata = ig_subdata, 
                  plot_name = proper_names[column])
  }  
  dev.off()
}

################################################################################