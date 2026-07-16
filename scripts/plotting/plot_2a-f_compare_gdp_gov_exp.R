library(grid)
source('scripts/6-3_waterfall_per_dose.R')
source('scripts/6-4_waterfall_total_per_dose.R')
source('scripts/plot_sens_analysis.R')
source('scripts/total_expenditure_compare_gdp_gov_exp.R')

paneller_compare=function(row,plot_type,coverage_type,price_adjustment)
{
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=row))
  
  if (plot_type=="sens_analysis"){
    if (row==1){
      percent_share(coverage=coverage_type,mechanism = price_adjustment,
                    compare_gdp_gov_exp = TRUE)
    }
    if (row==2){
      total_expenditure_compare_gdp_gov_exp(coverage_type)
    }
  } else {
    if (row==1){
      waterfall_volume_per_dose(mechanism = price_adjustment,coverage=coverage_type)
    }
    if (row==2){
      waterfall_volume_total_per_dose(mechanism = price_adjustment,coverage=coverage_type)
    }
  }
  popViewport()
  #print(paste0("Column",column))
  #print(paste0("Row",row))
}

plot_compare_gdp_gov_exp <- function(coverage){
  png(paste0('output/plots/final_paper/2a-f_8_compare_gdp_chepc_',coverage,'.png'),
      height=10,width=15,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3)))
  for (column in (1:3)){
    print(paste0("Column",column))
    pushViewport(viewport(layout.pos.col=column,layout.pos.row=1))
    if (column==1){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller_compare(row=1, plot_type = "base", price_adjustment = "gdp_adj",coverage_type=coverage)
      paneller_compare(row=2, plot_type="base", price_adjustment = "gdp_adj",coverage_type=coverage)
      popViewport()
      popViewport()
    }
    if (column==2){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller_compare(row=1, plot_type="base", price_adjustment = "chepc_adj",coverage_type=coverage)
      paneller_compare(row=2, plot_type="base", price_adjustment = "chepc_adj",coverage_type=coverage)
      popViewport()
      popViewport()
    }
    if (column==3){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller_compare(row=1, plot_type = "sens_analysis",coverage_type = coverage,price_adjustment = "gdp_adj")
      paneller_compare(row=2, plot_type = "sens_analysis",coverage_type = coverage,price_adjustment = "gdp_adj")
      popViewport()
      popViewport()
    }
  }  
  popViewport()
  popViewport()
  dev.off()
}


