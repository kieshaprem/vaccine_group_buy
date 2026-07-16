library(grid)
library(rineq)
source('scripts/6_waterfall_plot.R')
source('scripts/5_unaffordability_index_plot.R')



paneller=function(row,column,weighting,affordability_metric)
{
  if (affordability_metric=='perdose_gdppc_percent'){
  bilateral_spline <- spline_gdppc_percent_bilateral
  uniform_spline <- spline_gdppc_percent_uniform
  gdppc_spline <- spline_gdppc_percent_gdp_adj
  chepc_spline <- spline_gdppc_percent_gov_exp_adj
  wealth_metric <- 'gdp_percapita'
}
  if (affordability_metric=='perdose_gov_exp_percent'){
    bilateral_spline <- spline_gov_exp_percent_bilateral
    uniform_spline <- spline_gov_exp_percent_uniform
    gdppc_spline <- spline_gov_exp_percent_gdppc_adj
    gov_exp_spline <- spline_gov_exp_percent_gov_exp_adj
    wealth_metric <- 'gov_exp'
  }
  if (affordability_metric=='perdose_chepc_percent'){
    bilateral_spline <- spline_chepc_percent_bilateral
    uniform_spline <- spline_chepc_percent_uniform
    gdppc_spline <- spline_chepc_percent_gdppc_adj
    chepc_spline <- spline_chepc_percent_chepc_adj
    wealth_metric <- 'chepc'
  }
  bilateral_full <- bilateral_full[order(bilateral_full[,wealth_metric]),]
  uniform_full <- uniform_full[order(uniform_full[,wealth_metric]),]
  adj_gdppc_full <- adj_gdppc_full[order(adj_gdppc_full[,wealth_metric]),]
  adj_chepc_full <- adj_chepc_full[order(adj_chepc_full[,wealth_metric]),]
  
  #adj_gov_exp_full <- adj_gov_exp_full[order(adj_gov_exp_full[,wealth_metric]),]
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  if (row==1){
    # if (column==1){
    #   input_dataset <- bilateral_full
    #   waterfall_plot(mechanism="bilateral",dataset = input_dataset, 
    #                  affordability_metric = affordability_metric)
    # }
    if (column==1){
      input_dataset <- uniform_full
      waterfall_plot(mechanism="uniform",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==2){
      if (weighting=='gdp_adj'){
        input_dataset <- adj_gdppc_full
      }
      if (weighting=='gov_exp_adj'){
        input_dataset <- adj_gov_exp_full
      }
      if (weighting=='chepc_adj'){
        input_dataset <- adj_chepc_full
      }
      waterfall_plot(mechanism=weighting,dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==3){
      input_dataset <- adj_chepc_full
      waterfall_plot(mechanism=weighting,dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
  }
  if (row==2){
    wci_val_bilateral <- get_wci(dataset = bilateral_full,
                                 wealth_metric = wealth_metric,
                                affordability_metric=affordability_metric)
    # if (column==1){
    #   unaffordability_plot(mechanism = "bilateral", 
    #                        spline_df_bilateral = bilateral_spline, 
    #                        spline_df_main = bilateral_spline,
    #                        WCI_val_bilateral = wci_val_bilateral,
    #                        WCI_val = wci_val_bilateral,
    #                        affordability_metric = affordability_metric)
    # }
    if (column==1){
      wci_val <- get_wci(dataset = uniform_full,
                         wealth_metric = wealth_metric,
                         affordability_metric = affordability_metric)
      unaffordability_plot(mechanism = "uniform", 
                           spline_df_bilateral = bilateral_spline, 
                           spline_df_main = uniform_spline,
                           WCI_val_bilateral = wci_val_bilateral,
                           WCI_val = wci_val,
                           affordability_metric = affordability_metric)
    }
    if (column==2){
      if (weighting=='gdp_adj'){
          main_spline <- gdppc_spline
          wci_val <- get_wci(dataset = adj_gdppc_full,
                             wealth_metric = wealth_metric,
                             affordability_metric = affordability_metric)
        }
      if (weighting=='gov_exp_adj') {
        main_spline <- gov_exp_spline
        wci_val <- get_wci(dataset = adj_gov_exp_full,
                           wealth_metric = wealth_metric,
                           affordability_metric = affordability_metric)}
      if (weighting=='chepc_adj') {
          main_spline <- chepc_spline
          wci_val <- get_wci(dataset = adj_chepc_full,
                             wealth_metric = wealth_metric,
                             affordability_metric = affordability_metric)
          }
      unaffordability_plot(mechanism = weighting,
                           spline_df_bilateral = bilateral_spline,
                           spline_df_main = main_spline,
                           WCI_val_bilateral = wci_val_bilateral,
                           WCI_val = wci_val,
                           affordability_metric = affordability_metric)
    }
    if (column==3){
      main_spline <- chepc_spline
      wci_val <- get_wci(dataset = adj_chepc_full,
                         wealth_metric = wealth_metric,
                         affordability_metric = affordability_metric)
      unaffordability_plot(mechanism = 'chepc_adj',
                           spline_df_bilateral = bilateral_spline,
                           spline_df_main = main_spline,
                           WCI_val_bilateral = wci_val_bilateral,
                           WCI_val = wci_val,
                           affordability_metric = affordability_metric)
    }
  }
  print(paste0("Column",column))
  print(paste0("Row",row))
}

get_plot_3af <- function(weighting_mechanism, affordability_measure){
  png(paste0('output/plots/final_paper/3a-f_new_9_',weighting_mechanism,'_',affordability_measure,'.png'),
             height=10,width=15,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=3)))
  for (row in (1:2)){
    for (column in (1:3)){
      paneller(row, column, weighting=weighting_mechanism, 
               affordability_metric=affordability_measure)
      popViewport()
    }
  }  
  popViewport()
  popViewport()
  dev.off()
}

get_plot_3af_4cols <- function(affordability_measure){
  png(paste0('output/plots/final_paper/3a-f_4cols_',affordability_measure,'.png'),
      height=10,width=20,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=4)))
  for (row in (1:2)){
    for (column in (1:4)){
      paneller_2_4(row, column, 
               affordability_metric=affordability_measure)
      popViewport()
    }
  }  
  popViewport()
  popViewport()
  dev.off()
}



get_plot_3af_bilateral <- function(affordability_measure){
  if (affordability_measure=='perdose_gdppc_percent'){
    bilateral_spline <- spline_gdppc_percent_bilateral
    wealth_metric <- 'gdp_percapita'
  }
  if (affordability_measure=='perdose_chepc_percent'){
    bilateral_spline <- spline_chepc_percent_bilateral
    wealth_metric <- 'chepc'
  }
  wci_val_bilateral <- get_wci(dataset = bilateral_full,
                               wealth_metric = wealth_metric,
                               affordability_metric=affordability_measure)
  png(paste0('output/plots/final_paper/3a-f_bilateral_',affordability_measure,'.png'),
      height=10,width=5,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
  input_dataset <- bilateral_full
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  waterfall_plot(mechanism="bilateral",dataset = input_dataset, 
                  affordability_metric = affordability_measure)
  popViewport()
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
  unaffordability_plot(mechanism = "bilateral", 
                         spline_df_bilateral = bilateral_spline, 
                         spline_df_main = bilateral_spline,
                         WCI_val_bilateral = wci_val_bilateral,
                         WCI_val = wci_val_bilateral,
                       affordability_metric = affordability_measure)
  popViewport()
  popViewport()
  popViewport()
  dev.off()
}

get_wci <- function(dataset,wealth_metric,affordability_metric){
  full_wci_val <- ci(ineqvar = log(x=dataset[,wealth_metric],base = 10),
                     outcome = dataset[,affordability_metric],
                     type = "CIw")$concentration_index
  return(round(full_wci_val,3))
}


paneller_2_4=function(row,column,affordability_metric)
{
  if (affordability_metric=='perdose_gdppc_percent'){
    bilateral_spline <- spline_gdppc_percent_bilateral
    uniform_spline <- spline_gdppc_percent_uniform
    gdppc_spline <- spline_gdppc_percent_gdp_adj
    chepc_spline <- spline_gdppc_percent_gov_exp_adj
    wealth_metric <- 'gdp_percapita'
  }
  if (affordability_metric=='perdose_gov_exp_percent'){
    bilateral_spline <- spline_gov_exp_percent_bilateral
    uniform_spline <- spline_gov_exp_percent_uniform
    gdppc_spline <- spline_gov_exp_percent_gdppc_adj
    gov_exp_spline <- spline_gov_exp_percent_gov_exp_adj
    wealth_metric <- 'gov_exp'
  }
  if (affordability_metric=='perdose_chepc_percent'){
    bilateral_spline <- spline_chepc_percent_bilateral
    uniform_spline <- spline_chepc_percent_uniform
    gdppc_spline <- spline_chepc_percent_gdppc_adj
    chepc_spline <- spline_chepc_percent_chepc_adj
    wealth_metric <- 'chepc'
  }
  bilateral_full <- bilateral_full[order(bilateral_full[,wealth_metric]),]
  uniform_full <- uniform_full[order(uniform_full[,wealth_metric]),]
  adj_gdppc_full <- adj_gdppc_full[order(adj_gdppc_full[,wealth_metric]),]
  adj_chepc_full <- adj_chepc_full[order(adj_chepc_full[,wealth_metric]),]
  
  #adj_gov_exp_full <- adj_gov_exp_full[order(adj_gov_exp_full[,wealth_metric]),]
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  if (row==1){
    if (column==1){
      input_dataset <- bilateral_full
      waterfall_plot(mechanism="bilateral",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==2){
      input_dataset <- uniform_full
      waterfall_plot(mechanism="uniform",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==3){
    input_dataset <- adj_gdppc_full
      waterfall_plot(mechanism="gdp_adj",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==4){
      input_dataset <- adj_chepc_full
      waterfall_plot(mechanism="chepc_adj",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
  }
  if (row==2){
    wci_val_bilateral <- get_wci(dataset = bilateral_full,
                                 wealth_metric = wealth_metric,
                                 affordability_metric=affordability_metric)
    if (column==1){
      unaffordability_plot(mechanism = "bilateral", 
                           spline_df_bilateral = bilateral_spline, 
                           spline_df_main = bilateral_spline,
                           WCI_val_bilateral = wci_val_bilateral,
                           WCI_val = wci_val_bilateral,
                           affordability_metric = affordability_metric)
    }
    if (column==2){
      wci_val <- get_wci(dataset = uniform_full,
                         wealth_metric = wealth_metric,
                         affordability_metric = affordability_metric)
      unaffordability_plot(mechanism = "uniform", 
                           spline_df_bilateral = bilateral_spline, 
                           spline_df_main = uniform_spline,
                           WCI_val_bilateral = wci_val_bilateral,
                           WCI_val = wci_val,
                           affordability_metric = affordability_metric)
    }
    if (column==3){
        main_spline <- gdppc_spline
        wci_val <- get_wci(dataset = adj_gdppc_full,
                           wealth_metric = wealth_metric,
                           affordability_metric = affordability_metric)
      unaffordability_plot(mechanism = "gdp_adj",
                           spline_df_bilateral = bilateral_spline,
                           spline_df_main = main_spline,
                           WCI_val_bilateral = wci_val_bilateral,
                           WCI_val = wci_val,
                           affordability_metric = affordability_metric)
    }
    if (column==4){
      main_spline <- chepc_spline
      wci_val <- get_wci(dataset = adj_chepc_full,
                         wealth_metric = wealth_metric,
                         affordability_metric = affordability_metric)
      unaffordability_plot(mechanism = 'chepc_adj',
                           spline_df_bilateral = bilateral_spline,
                           spline_df_main = main_spline,
                           WCI_val_bilateral = wci_val_bilateral,
                           WCI_val = wci_val,
                           affordability_metric = affordability_metric)
    }
  }
  print(paste0("Column",column))
  print(paste0("Row",row))
}
