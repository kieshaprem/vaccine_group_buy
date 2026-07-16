source('scripts/get_box_plot.R')

get_selected_grp_order <- function(data, group_col, desired_order){
  data$selected_grp_order <- character(nrow(data))
  for (i in 1:length(desired_order)){
    if (nrow(data[data[,group_col]==desired_order[i],]>0)){
      data[data[,group_col]==desired_order[i],]$selected_grp_order <- i
    }
  }
  return(data)
}

make_price_numeric <- function(data){
  data$Price.per.Dose.in.USD <- sapply(data$Price.per.Dose.in.USD,
                                       function(x){substr(x,3,nchar(x)-1)})
  data$Price.per.Dose.in.USD <- as.numeric(data$Price.per.Dose.in.USD)
  return(data)
}

plot_boxplot_price <- function(filename, input_data, input_group_col){
  unique_x <- unique(input_data[,input_group_col])

  if (input_group_col=="Income.Group"){
    if (length(unique(input_data[,input_group_col]))==4){
    ordering <- c('LIC','LMIC','UMIC','HIC')
    #other_factor <- 0.06
    #y_factor <- 1
    } else {
      ordering <- c('LMIC','UMIC','HIC')
    }
    x_name <- "Price per dose by Country Income Group"
    x_factor_coordinate <- 0.5
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
    x_factor_coordinate <- 0.25/4*length(unique_x)
    
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
    x_factor_coordinate <- 0.43/4*length(unique_x)
    #other_factor <- 0.028
    #y_factor <- 2.15
  }
  
  if (max(input_data$Price.per.Dose.in.USD)>=50){
    y_factor <- 2.27
  } else if (max(input_data$Price.per.Dose.in.USD)<=20){
    y_factor <- 0.35
    #other_factor <- 0.045
  } else {
    y_factor <- 1
    #other_factor <- 0.04
  }
  print(paste0("y_factor: ",y_factor))

  # if (substr(filename,nchar(filename)-5,nchar(filename))!='Pfizer'){
  #   other_factor <- 0.1
  # }
  # if (substr(filename,nchar(filename)-10,nchar(filename))=='AstraZeneca'){
  #   other_factor <- 0.05
  # }
  # if (length(unique_x)==1){
  #   other_factor <- 0.3
  # }
  # 
  # if (filename=='manufacturer_exclude_pfizer_moderna'){
  #   ordering <- c("Blank",
  #                 'BIBP',
  #                 "SII",
  #                 "AstraZeneca",
  #                 "Janssen",
  #                 "Gamaleya",
  #                 "Sinovac")
  #   x_name <- "Price per dose by Manufacturer (excluding Pfizer and Moderna)"
  #   other_factor <- 0.018
  #   y_factor <- 0.15
  # }
  # if (substr(filename,nchar(filename)-3,nchar(filename)) %in% c('LMIC','UMIC')){
  #   y_factor <- 0.15
  # }
  input_data <- get_selected_grp_order(data = input_data,
                                       group_col = input_group_col,
                                       desired_order = ordering)
  x_factor <- 10/3*length(unique_x)
  if (substr(filename,nchar(filename)-8,nchar(filename))=='bilateral'){
    bilateral_flag_input <- TRUE
    y_factor <- 1
  } else {
    bilateral_flag_input <- FALSE
  }
  #other_factor <- 0.373/x_factor
  png(paste0('output/plots/final_paper/boxplot_mi4a/boxplot_',filename,".png"),
      height=10,width=x_factor,res=300, units='cm',pointsize=10)
  get_box_plot(data = input_data, group_col = input_group_col, 
               val_col = "Price.per.Dose.in.USD",
               y_axis_name = "Price per dose (US$)", x_axis_name = x_name,
               group_order_col = "selected_grp_order",
               bar_colour = "grey80", line_colour = "black",x_factor = x_factor_coordinate,
               y_factor = y_factor,bilateral_flag = bilateral_flag_input)
  dev.off()
  print(paste0("x_factor_coordinate: ",x_factor_coordinate))
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
################################################################################