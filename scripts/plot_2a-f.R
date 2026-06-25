library(grid)
source('scripts/6-3_waterfall_per_dose.R')
source('scripts/6-4_waterfall_total_per_dose.R')
source('scripts/plot_sens_analysis.R')
source('scripts/total_expenditure.R')

paneller=function(row,mechanism)
{
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=row))
  if (mechanism=="uniform"){
    if (row==1){
      waterfall_volume_per_dose("uniform")
    }
    if (row==2){
      waterfall_volume_total_per_dose("uniform")
    }
  }
  if (mechanism=="gdp_adj"){
    if (row==1){
      waterfall_volume_per_dose("gdp_adj")
    }
    if (row==2){
      waterfall_volume_total_per_dose("gdp_adj")
    }
  }
  
  if (mechanism=="sens_analysis"){
    if (row==1){
      percent_share()
    }
    if (row==2){
      total_expenditure()
    }
  }
  
  popViewport()
  print(paste0("Column",column))
  print(paste0("Row",row))
}

png('output/plots/final_paper/2a-f_updated.png',height=10,width=15,res=300, units='cm',pointsize=10)
pushViewport(plotViewport(c(1,1,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3)))
for (column in (1:3)){
  print(paste0("Column",column))
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=1))
  if (column==1){
    pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
    paneller(row=1, mechanism = "uniform")
    paneller(row=2, mechanism = "uniform")
    popViewport()
    popViewport()
  }
  if (column==2){
    pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
    paneller(row=1, mechanism = "gdp_adj")
    paneller(row=2, mechanism = "gdp_adj")
    popViewport()
    popViewport()
  }
  # if (column==3){
  #   pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
  #   paneller(row=1, mechanism = "sens_analysis")
  #   paneller(row=2, mechanism = "sens_analysis")
  #   popViewport()
  #   popViewport()
  # }
}  
popViewport()
popViewport()
dev.off()

bilateral_full <- bilateral_full[order(-main_sheet$gdp_percapita),]
