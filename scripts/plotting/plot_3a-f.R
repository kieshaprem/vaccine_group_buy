library(grid)
library(rineq)
source('scripts/plotting/waterfall_plot.R')
source('scripts/plotting/5_unaffordability_index_plot.R')
source('scripts/0_process_calculations.R')


paneller=function(row,column,weighting,affordability_metric)
{
  if (affordability_metric=='perdose_gdppc_percent'){
  bilateral_spline <- spline_gdppc_percent_bilateral
  uniform_spline <- spline_gdppc_percent_uniform
  gdppc_spline <- spline_gdppc_percent_gdp_adj
  chepc_spline <- spline_gdppc_percent_chepc_adj
  wealth_metric <- 'gdppc'
}

  if (affordability_metric=='perdose_chepc_percent'){
    bilateral_spline <- spline_chepc_percent_bilateral
    uniform_spline <- spline_chepc_percent_uniform
    gdppc_spline <- spline_chepc_percent_gdppc_adj
    chepc_spline <- spline_chepc_percent_chepc_adj
    wealth_metric <- 'chepc'
  }
  bilateral <- bilateral[order(bilateral[,wealth_metric]),]
  uniform <- uniform[order(uniform[,wealth_metric]),]
  adj_gdppc <- adj_gdppc[order(adj_gdppc[,wealth_metric]),]
  adj_chepc <- adj_chepc[order(adj_chepc[,wealth_metric]),]
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  if (row==1){
    if (column==1){
      input_dataset <- uniform
      waterfall_plot(mechanism="uniform",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==2){
      if (weighting=='gdp_adj'){
        input_dataset <- adj_gdppc
      }
      if (weighting=='chepc_adj'){
        input_dataset <- adj_chepc
      }
      waterfall_plot(mechanism=weighting,dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==3){
      input_dataset <- adj_chepc
      waterfall_plot(mechanism=weighting,dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
  }
  if (row==2){
    ci_val_bilateral <- get_ci(df = bilateral,
                                 perspective = wealth_metric)

    if (column==1){
      ci_val <- get_ci(df = uniform,
                         perspective = wealth_metric)
      unaffordability_plot(mechanism = "uniform", 
                           spline_df_bilateral = bilateral_spline, 
                           spline_df_main = uniform_spline,
                           CI_val_bilateral = ci_val_bilateral,
                           CI_val = ci_val,
                           affordability_metric = affordability_metric)
    }
    if (column==2){
      if (weighting=='gdp_adj'){
          main_spline <- gdppc_spline
          ci_val <- get_ci(df = adj_gdppc,
                             perspective = wealth_metric)
        }
      
      if (weighting=='chepc_adj') {
          main_spline <- chepc_spline
          ci_val <- get_ci(df = adj_chepc,
                             perspective = wealth_metric)
          }
      unaffordability_plot(mechanism = weighting,
                           spline_df_bilateral = bilateral_spline,
                           spline_df_main = main_spline,
                           CI_val_bilateral = ci_val_bilateral,
                           CI_val = ci_val,
                           affordability_metric = affordability_metric)
    }
    if (column==3){
      main_spline <- chepc_spline
      ci_val <- get_ci(df = adj_chepc,
                         perspective = wealth_metric)
      unaffordability_plot(mechanism = 'chepc_adj',
                           spline_df_bilateral = bilateral_spline,
                           spline_df_main = main_spline,
                           CI_val_bilateral = ci_val_bilateral,
                           CI_val = ci_val,
                           affordability_metric = affordability_metric)
    }
  }
  print(paste0("Column",column))
  print(paste0("Row",row))
}

get_plot_3af <- function(weighting_mechanism, affordability_measure){
  png(paste0('output/plots/final_paper/equity/',weighting_mechanism,'_',affordability_measure,'.png'),
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

get_plot_3af_4cols <- function(affordability_measure,coverage){
  png(paste0('output/plots/final_paper/equity/',affordability_measure,'_',coverage,'.png'),
      height=10,width=20,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
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
    wealth_metric <- 'gdppc'
  }
  if (affordability_measure=='perdose_chepc_percent'){
    bilateral_spline <- spline_chepc_percent_bilateral
    wealth_metric <- 'chepc'
  }
  ci_val_bilateral <- get_ci(df = bilateral,
                               perspective = wealth_metric)
  png(paste0('output/plots/final_paper/3a-f_bilateral_',affordability_measure,'.png'),
      height=10,width=5,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
  input_dataset <- bilateral
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  waterfall_plot(mechanism="bilateral",dataset = input_dataset, 
                  affordability_metric = affordability_measure)
  popViewport()
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
  unaffordability_plot(mechanism = "bilateral", 
                         spline_df_bilateral = bilateral_spline, 
                         spline_df_main = bilateral_spline,
                         ci_val_bilateral = ci_val_bilateral,
                         ci_val = ci_val_bilateral,
                       affordability_metric = affordability_measure)
  popViewport()
  popViewport()
  popViewport()
  dev.off()
}

get_plot_3af_fullpage <- function(coverage){
  png(paste0('output/plots/final_paper/equity/noother_fullpage_',coverage,'.png'),
      height=24,width=18,res=600, units='cm',pointsize=8)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
  
  bilateral <- bilateral[order(bilateral[,'gdppc']),]
  uniform <- uniform[order(uniform[,'gdppc']),]
  adj_gdppc <- adj_gdppc[order(adj_gdppc[,'gdppc']),]
  adj_chepc <- adj_chepc[order(adj_chepc[,'gdppc']),]
  
  paneller_overall(pseudorow = 1, affordability_metric = 'perdose_gdppc_percent')
  
  bilateral <- bilateral[order(bilateral[,'chepc']),]
  uniform <- uniform[order(uniform[,'chepc']),]
  adj_gdppc <- adj_chepc[order(adj_gdppc[,'chepc']),]
  adj_chepc <- adj_chepc[order(adj_chepc[,'chepc']),]
  
  paneller_overall(pseudorow = 2, affordability_metric = 'perdose_chepc_percent')
  popViewport()
  popViewport()
  dev.off()
}


paneller_overall <- function(pseudorow,affordability_metric){
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=pseudorow))
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=4)))
  for (row in (1:2)){
    for (column in (1:4)){
      paneller_2_4(row, column, 
                   affordability_metric=affordability_metric)
      popViewport()
    }
  } 
  popViewport()
  popViewport()
}

paneller_2_4=function(row,column,affordability_metric)
{
  if (affordability_metric=='perdose_gdppc_percent'){
    bilateral_spline <- spline_gdppc_percent_bilateral
    uniform_spline <- spline_gdppc_percent_uniform
    gdppc_spline <- spline_gdppc_percent_gdppc_adj
    chepc_spline <- spline_gdppc_percent_chepc_adj
    wealth_metric <- 'gdppc'
  }

  if (affordability_metric=='perdose_chepc_percent'){
    bilateral_spline <- spline_chepc_percent_bilateral
    uniform_spline <- spline_chepc_percent_uniform
    gdppc_spline <- spline_chepc_percent_gdppc_adj
    chepc_spline <- spline_chepc_percent_chepc_adj
    wealth_metric <- 'chepc'
  }
  
  bilateral <- bilateral[order(bilateral[,wealth_metric]),]
  uniform <- uniform[order(uniform[,wealth_metric]),]
  adj_gdppc <- adj_gdppc[order(adj_gdppc[,wealth_metric]),]
  adj_chepc <- adj_chepc[order(adj_chepc[,wealth_metric]),]
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  if (row==1){
    if (column==1){
      input_dataset <- bilateral
      waterfall_plot(mechanism="bilateral",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==2){
      input_dataset <- uniform
      waterfall_plot(mechanism="uniform",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==3){
    input_dataset <- adj_gdppc
      waterfall_plot(mechanism="gdp_adj",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
    if (column==4){
      input_dataset <- adj_chepc
      waterfall_plot(mechanism="chepc_adj",dataset = input_dataset, 
                     affordability_metric = affordability_metric)
    }
  }
  if (row==2){
    ci_val_bilateral <- get_ci(df = bilateral,
                                 perspective = wealth_metric)
    if (column==1){
      unaffordability_plot(mechanism = "bilateral", 
                           spline_df_bilateral = bilateral_spline, 
                           spline_df_main = bilateral_spline,
                           CI_val_bilateral = ci_val_bilateral,
                           CI_val = ci_val_bilateral,
                           affordability_metric = affordability_metric)
    }
    if (column==2){
      ci_val <- get_ci(df = uniform,
                         perspective = wealth_metric)
      unaffordability_plot(mechanism = "uniform", 
                           spline_df_bilateral = bilateral_spline, 
                           spline_df_main = uniform_spline,
                           CI_val_bilateral = ci_val_bilateral,
                           CI_val = ci_val,
                           affordability_metric = affordability_metric)
    }
    if (column==3){
        main_spline <- gdppc_spline
        ci_val <- get_ci(df = adj_gdppc,
                           perspective = wealth_metric)
      unaffordability_plot(mechanism = "gdp_adj",
                           spline_df_bilateral = bilateral_spline,
                           spline_df_main = main_spline,
                           CI_val_bilateral = ci_val_bilateral,
                           CI_val = ci_val,
                           affordability_metric = affordability_metric)
    }
    if (column==4){
      main_spline <- chepc_spline
      ci_val <- get_ci(df = adj_chepc,
                         perspective = wealth_metric)
      unaffordability_plot(mechanism = 'chepc_adj',
                           spline_df_bilateral = bilateral_spline,
                           spline_df_main = main_spline,
                           CI_val_bilateral = ci_val_bilateral,
                           CI_val = ci_val,
                           affordability_metric = affordability_metric)
    }
  }
  print(paste0("Column",column))
  print(paste0("Row",row))
}
