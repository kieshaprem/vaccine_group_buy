library(grid)
source('scripts/6-3_waterfall_per_dose.R')
source('scripts/6-4_waterfall_total_per_dose.R')
source('scripts/plot_sens_analysis.R')
source('scripts/total_expenditure.R')

paneller=function(row,plot_type,coverage_type,price_adjustment)
{
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=row))
 
  if (plot_type=="sens_analysis"){
    if (row==1){
      percent_share(coverage=coverage_type,mechanism = price_adjustment,compare_gdp_oop = FALSE)
    }
    if (row==2){
      total_expenditure(coverage_type, mechanism=price_adjustment)
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

plot_coverage <- function(coverage, price_adjustment){
  png(paste0('output/plots/final_paper/2a-f_updated3_',coverage,"_",price_adjustment,'.png'),
             height=10,width=15,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3)))
  for (column in (1:3)){
    print(paste0("Column",column))
    pushViewport(viewport(layout.pos.col=column,layout.pos.row=1))
    if (column==1){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller(row=1, plot_type = "base", price_adjustment = "uniform",coverage_type=coverage)
      paneller(row=2, plot_type="base", price_adjustment = "uniform",coverage_type=coverage)
      popViewport()
      popViewport()
    }
    if (column==2){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller(row=1, plot_type="base", price_adjustment = price_adjustment,coverage_type=coverage)
      paneller(row=2, plot_type="base", price_adjustment = price_adjustment,coverage_type=coverage)
      popViewport()
      popViewport()
    }
    if (column==3){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller(row=1, plot_type = "sens_analysis",coverage_type = coverage,price_adjustment = price_adjustment)
      paneller(row=2, plot_type = "sens_analysis",coverage_type = coverage,price_adjustment = price_adjustment)
      popViewport()
      popViewport()
    }
  }  
  popViewport()
  popViewport()
  dev.off()
}

plot_coverage(coverage = 'full',price_adjustment = 'gdp_adj')
plot_coverage(coverage = 'full',price_adjustment = 'ooppc_adj')

plot_coverage('partial')

bilateral_full <- bilateral_full[order(bilateral_full$gdp_percapita),]
uniform_full <- uniform_full[order(uniform_full$gdp_percapita),]
adj_full_gdppc <- adj_full_gdppc[order(adj_full_gdppc$gdp_percapita),]

#bilateral_partial <- bilateral_partial[order(main_sheet$gdp_percapita),]
#uniform_partial <- uniform_partial[order(main_sheet$gdp_percapita),]
#adj_partial_gdppc <- adj_partial_gdppc[order(main_sheet$gdp_percapita),]

