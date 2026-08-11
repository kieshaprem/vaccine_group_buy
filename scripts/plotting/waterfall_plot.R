#bilateral <- bilateral[order(-dataset$`GDP per capita (2022)`),]
library(grid)
library(countrycode)

waterfall_plot <- function(mechanism, dataset, affordability_metric){
  dataset$iso2c <- countrycode(dataset$iso3c, origin='iso3c', destination = 'iso2c')
  #grid.rect()
  
  if (mechanism == "bilateral"){
      # legend
    grid.polygon(x = c(1.2,1.6,1.6,1.2),
                 y = c(47.75,47.75,48.75,48.75),
                 default.units = 'native',
                 gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
    
    # c(2.4,2.8,2.8,2.4)
    grid.polygon(x = c(2.4,2.8,2.8,2.4),
                 y = c(47.75,47.75,48.75,48.75),
                 default.units = 'native',
                 gp = gpar(col = 'red', fill = 'red'))
    grid.text(paste0('$'),x = unit(0,'npc')+unit(1.8,'lines'),y = unit(0,'npc')+unit(22,'lines'),gp = gpar(fontsize=5))
    
    if (affordability_metric=='perdose_gdppc_percent'){
      affordability_metric_name <- 'GDP per capita'
    }
      if (affordability_metric=='perdose_chepc_percent'){
      affordability_metric_name <- 'CHE per capita'
      }
    grid.text(paste0('Price of 1 dose'),x = unit(0,'npc')+unit(-4.2,'lines'),rot=90,
              gp = gpar(fontsize=6,fontface='bold',col = 'black'))
    
    grid.text(paste0('% of ',affordability_metric_name),x = unit(0,'npc')+unit(7,'lines'),rot=90,
              y = unit(0,'npc')+unit(22,'lines'),gp = gpar(fontsize=5))
    pushViewport(plotViewport(c(1,4,1,1),xscale=c(0.5,11.5),yscale=c(0,50)))
    
  } else {
    pushViewport(plotViewport(c(1,3,1,1),xscale=c(0.5,11.5),yscale=c(0,50)))
  }
  
  if (mechanism == "bilateral" & affordability_metric == 'perdose_gdppc_percent'){
    grid.text(paste0('a) Bilateral Pricing'),x = unit(0,'npc')+unit(3,'lines'),y = unit(0,'npc')+unit(21.3,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  
  if (mechanism == "bilateral" & affordability_metric == 'perdose_chepc_percent'){
    grid.text(paste0('i)'),x = unit(0,'npc')+unit(0.1,'lines'), y = unit(0,'npc')+unit(20,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }

  if (mechanism == "uniform"& affordability_metric == 'perdose_gdppc_percent'){
    grid.text(paste0('b) Uniform Pricing'),x = unit(0,'npc')+unit(3,'lines'),y = unit(0,'npc')+unit(21.3,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  
  if (mechanism == "uniform"& affordability_metric == 'perdose_chepc_percent'){
    grid.text(paste0('j)'),x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(20,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  
  if (mechanism == "gdp_adj"& affordability_metric == 'perdose_gdppc_percent'){
    grid.text(paste0('c) GDPPC-adjusted'),x = unit(0,'npc')+unit(1.2,'lines'),y = unit(0,'npc')+unit(21.3,'lines'),gp = gpar(fontsize=6,fontface='bold'))
    grid.text(as.expression(bquote(alpha == 10)), x=0.1,y=1.05,gp=gpar(fontsize=6))
  }

  if (mechanism == "gdp_adj"& affordability_metric == 'perdose_chepc_percent'){
    grid.text(paste0('k)'),x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(20,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  
  if (mechanism == "chepc_adj"& affordability_metric == 'perdose_gdppc_percent'){
    grid.text(paste0('d) CHEPC-adjusted'),x = unit(0,'npc')+unit(1.2,'lines'),y = unit(0,'npc')+unit(21.3,'lines'),gp = gpar(fontsize=6,fontface='bold'))
    grid.text(as.expression(bquote(alpha == 10)), x=0.1,y=1.05,gp=gpar(fontsize=6))
  }
  
  if (mechanism == "chepc_adj"& affordability_metric == 'perdose_chepc_percent'){
    grid.text(paste0('l)'),x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(20,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  
  #grid.text(paste0('ASEAN Countries ordered by GDP per capita'),x = unit(0,'npc')+unit(7.9,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=6,fontface='bold'))
   
  # bar chart
  for (i in (1:11)){
    grid.polygon(x = c(i+c(-0.4,0.4),rev(i+c(-0.4,0.4))),
                 y = c(0,0,dataset[i,]$perdose,
                       dataset[i,]$perdose),
                 default.units = 'native',
                 gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
  }
  if (affordability_metric=='perdose_gdppc_percent'){
    y_vals <- dataset[,affordability_metric]*50
    grid.yaxis(at=c(0,10,20,30,40,50), 
               label=c("0.0%","0.2%","0.4%","0.6%","0.8%","1.0%"),gp=gpar(fontsize=5),main=FALSE)
  }
  # if (affordability_metric=='perdose_gov_exp_percent'){
  #   y_vals <- dataset[,affordability_metric]*0.3764531
  #   grid.yaxis(at=c(0,15,30,45,60), 
  #              label=c("0%","40%","80%","120%","160%"),gp=gpar(fontsize=5),main=FALSE)
  # }
  if (affordability_metric=='perdose_chepc_percent'){
    y_vals <- dataset[,affordability_metric]
    grid.yaxis(at=c(0,10,20,30,40,50), 
               label=c("0%","10%","20%","30%","40%","50%"),gp=gpar(fontsize=5),main=FALSE)
  }
  grid.lines(seq(1,11), y_vals, default.units = "native",
             gp=gpar(col="red", lwd=0.8))
  grid.points(seq(1,11), y_vals, default.units = "native",
              gp=gpar(col="red", cex=0.4),pch=16)

  grid.yaxis(at=c(0,10,20,30,40,50), 
             label=c("$0","$10","$20","$30","$40","$50"),gp=gpar(fontsize=5))
  
  grid.xaxis(at=seq(1,11), 
             label=(dataset$iso2c),gp=gpar(fontsize=4.5))
  
  
  popViewport()
}
