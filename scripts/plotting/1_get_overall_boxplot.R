source('scripts/plotting/get_box_plot.R')
source('scripts/plotting/mi4a_boxplot_wrapper_functions.R')
load('output/intermediate/psa_outcomes_empirical_full.rdata')
library(tidyr)
library(dplyr)


headline_boxplot <- function(df_long, filename){
  # png(paste0('output/plots/final_paper/headline_boxplot_',filename,".png"),
  #     height=10,width=40/3,res=300, units='cm',pointsize=10)
  ordering <- c('Bilateral\n','Uniform\n','GDPPC-\nadjusted',
                'CHEPC-\nadjusted')
  df_long <- get_selected_grp_order(data = df_long,
                         group_col = 'Pricing',
                         desired_order = ordering)
  get_box_plot(data = df_long, group_col = 'Pricing', val_col = 'Total Expenditure',
               y_axis_name = 'Total Expenditure (US$)', x_axis_name = 'Pricing Mechanism',
               group_order_col = 'selected_grp_order',bar_colour = 'grey',
               line_colour = 'black',y_factor = 1e8,
               n_flag = F,plot_name = 'h) Total expenditure')
  #dev.off()
}

regional_outcomes <- full_outcome_base[1:4,]
regional_outcomes <- t(regional_outcomes)
regional_outcomes <- data.frame(regional_outcomes)

# Convert wide to long
df_long <- regional_outcomes %>%
  pivot_longer(
    cols = names(regional_outcomes),
    names_to = "Pricing",           # new column for variable names
    values_to = "Total Expenditure"              # new column for values
  )
df_long <- as.data.frame(df_long)
df_long[df_long$Pricing=='bilateral_total',]$Pricing <- 'Bilateral\n'
df_long[df_long$Pricing=='uniform_total',]$Pricing <- 'Uniform\n'
df_long[df_long$Pricing=='adj_gdppc_total',]$Pricing <- 'GDPPC-\nadjusted'
df_long[df_long$Pricing=='adj_chepc_total',]$Pricing <- 'CHEPC-\nadjusted'

#headline_boxplot(df_long = df_long, filename = 'full_empirical')
