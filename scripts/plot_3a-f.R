library(grid)
source('scripts/6_waterfall_plot.R')
source('scripts/5_unaffordability_index_plot.R')


paneller=function(row,column)
{
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  if (row==1){
    if (column==1){
      waterfall_plot("bilateral")
    }
    if (column==2){
      waterfall_plot("uniform")
    }
    if (column==3){
      waterfall_plot("gdp_adj")
    }
  }
  if (row==2){
    if (column==1){
      unaffordability_plot("bilateral")
    }
    if (column==2){
      unaffordability_plot("uniform")
    }
    if (column==3){
      unaffordability_plot("gdp_adj")
    }
  }
  print(paste0("Column",column))
  print(paste0("Row",row))
}

png('output/plots/final_paper/3a-f_new_2.png',height=10,width=15,res=300, units='cm',pointsize=10)
pushViewport(plotViewport(c(1,1,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=3)))
for (row in (1:2)){
  for (column in (1:3)){
    paneller(row, column)
    popViewport()
  }
}  
popViewport()
popViewport()
dev.off()
