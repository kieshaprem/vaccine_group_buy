library(grid)
source('scripts/plotting/waterfall_per_dose.R')
source('scripts/plotting/waterfall_total_per_dose.R')
source('scripts/plotting/plot_sens_analysis.R')
source('scripts/plotting/total_expenditure_compare_gdp_gov_exp.R')
source('scripts/plotting/1_plot_total_exp.R')

paneller_compare=function(row,plot_type,price_adjustment=NA,col)
{
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=row))
  
  if (plot_type=="sens_analysis"){
    if (col==1){
      percent_share()
    }
    if (col==2){
      get_exp_across(total_exp_df)
    }
  } else {
    if (row==1){
      waterfall_volume_per_dose(mechanism = price_adjustment)
    }
    if (row==2){
      waterfall_volume_total_per_dose(mechanism = price_adjustment)
    }
  }
  popViewport()
}


plot_compare_gdp_chepc_3rows <- function(coverage){
  png(paste0('output/plots/final_paper/2ag_',coverage,'.png'),
      height=18,width=18,res=600, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=3,ncol=3)))
  for (column in (1:3)){
    print(paste0("Column",column))
    pushViewport(viewport(layout.pos.col=column,layout.pos.row=1:3))
    pushViewport(viewport(layout=grid.layout(nrow=3,ncol=1)))
    if (column==1){
      paneller_compare(row=1, plot_type = "base", price_adjustment = "uniform",col = column)
      paneller_compare(row=2, plot_type="base", price_adjustment = "uniform",col = column)
      paneller_compare(row=3, plot_type="sens_analysis",col = column)
      popViewport()
      popViewport()
    }
    if (column==2){
      paneller_compare(row=1, plot_type="base", price_adjustment = "gdp_adj",col = column)
      paneller_compare(row=2, plot_type="base", price_adjustment = "gdp_adj",col = column)
      paneller_compare(row=3, plot_type="sens_analysis",col = column)
      popViewport()
      popViewport()
    }
    if (column==3){
      paneller_compare(row=1, plot_type="base", price_adjustment = "chepc_adj",col = column)
      paneller_compare(row=2, plot_type="base", price_adjustment = "chepc_adj",col = column)
      popViewport()
      popViewport()
    }
    # if (column==4){
    #   paneller_compare(row=1, plot_type = "sens_analysis")
    #   paneller_compare(row=2, plot_type = "sens_analysis")
    #   popViewport()
    #   popViewport()
    # }
  }  
  popViewport()
  popViewport()
  dev.off()
}


plot_compare_gdp_chepc <- function(coverage){
  png(paste0('output/plots/final_paper/country_expenditure/',coverage,'2.png'),
      height=10,width=15,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3)))
  for (column in (1:3)){
    print(paste0("Column",column))
    pushViewport(viewport(layout.pos.col=column,layout.pos.row=1))
    if (column==1){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller_compare(row=1, plot_type = "base", price_adjustment = "uniform")
      paneller_compare(row=2, plot_type="base", price_adjustment = "uniform")
      popViewport()
      popViewport()
    }
    if (column==2){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller_compare(row=1, plot_type="base", price_adjustment = "gdp_adj")
      paneller_compare(row=2, plot_type="base", price_adjustment = "gdp_adj")
      popViewport()
      popViewport()
    }
    if (column==3){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      #paneller_compare(row=1, plot_type = "sens_analysis",coverage_type = coverage,price_adjustment = "gdp_adj")
      #paneller_compare(row=2, plot_type = "sens_analysis",coverage_type = coverage,price_adjustment = "gdp_adj")
      paneller_compare(row=1, plot_type="base", price_adjustment = "chepc_adj")
      paneller_compare(row=2, plot_type="base", price_adjustment = "chepc_adj")
      popViewport()
      popViewport()
    }
  }  
  popViewport()
  popViewport()
  dev.off()
}


plot_compare_gdp_chepc_2_4 <- function(coverage){
  png(paste0('output/plots/final_paper/country_expenditure/complete_',coverage,'.png'),
      height=15,width=30,res=600, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=4)))
  for (column in (1:4)){
    print(paste0("Column",column))
    pushViewport(viewport(layout.pos.col=column,layout.pos.row=1:2))
    pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
    if (column==1){
      paneller_compare(row=1, plot_type = "base", price_adjustment = "uniform")
      paneller_compare(row=2, plot_type="base", price_adjustment = "uniform")
      popViewport()
      popViewport()
    }
    if (column==2){
      paneller_compare(row=1, plot_type="base", price_adjustment = "gdp_adj")
      paneller_compare(row=2, plot_type="base", price_adjustment = "gdp_adj")
      popViewport()
      popViewport()
    }
    if (column==3){
      paneller_compare(row=1, plot_type="base", price_adjustment = "chepc_adj")
      paneller_compare(row=2, plot_type="base", price_adjustment = "chepc_adj")
      popViewport()
      popViewport()
    }
    if (column==4){
      paneller_compare(row=1, plot_type = "sens_analysis")
      paneller_compare(row=2, plot_type = "sens_analysis")
      popViewport()
      popViewport()
    }
  }  
  popViewport()
  popViewport()
  dev.off()
}

