library(grid)
source('scripts/plotting/waterfall_per_dose.R')
source('scripts/plotting/waterfall_total_per_dose.R')
source('scripts/plotting/plot_sens_analysis.R')
source('scripts/plotting/total_expenditure_compare_gdp_gov_exp.R')
source('scripts/plotting/1_get_overall_boxplot.R')

paneller_compare=function(row,plot_type,price_adjustment=NA,col)
{
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=row))
  
  if (plot_type=="sens_analysis"){
    if (row==1){
      percent_share()
    }
    if (row==2){
      headline_boxplot(df_long = df_long, filename = NA)
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

plot_compare_gdp_chepc <- function(coverage){
  png(paste0('output/plots/final_paper/country_expenditure/',coverage,'.png'),
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
      height=20,width=30,res=300, units='cm',pointsize=10)
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


