#bilateral <- bilateral[order(-dataset$`GDP per capita (2022)`),]
library(grid)

waterfall_plot <- function(mechanism){
  
  pushViewport(plotViewport(c(1,3,1,1),xscale=c(0.5,10.5),yscale=c(0,60)))
  grid.rect()
  
  if (mechanism == "bilateral"){
    dataset <- bilateral
    grid.text('a) Bilateral Pricing',x = unit(0,'npc')+unit(3,'lines'),y = unit(0,'npc')+unit(16.3,'lines'),gp = gpar(fontsize=6,fontface='bold'))
    grid.text(paste0('Price of 1 dose'),x = unit(0,'npc')+unit(-4.2,'lines'),rot=90,
              gp = gpar(fontsize=6,fontface='bold',col = 'black'))
  }
  if (mechanism == "uniform"){
    dataset <- uniform
    grid.text('b) Uniform Pricing',x = unit(0,'npc')+unit(3,'lines'),y = unit(0,'npc')+unit(16.3,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  if (mechanism == "gdp_adj"){
    dataset <- gdp_adj
    grid.text('c) Weighted by GDP per capita',x = unit(0,'npc')+unit(5.5,'lines'),y = unit(0,'npc')+unit(16.3,'lines'),gp = gpar(fontsize=6,fontface='bold'))
    grid.text(as.expression(bquote(alpha == 10)), x=0.1,y=1.05,gp=gpar(fontsize=6))
  }
  
  # legend
  grid.polygon(x = c(0.8,1.2,1.2,0.8),
               y = c(57,57,58,58),
               default.units = 'native',
               gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
  
  grid.polygon(x = c(2,2.4,2.4,2),
               y = c(57,57,58,58),
               default.units = 'native',
               gp = gpar(col = 'red', fill = 'red'))
  grid.text(paste0('$'),x = unit(0,'npc')+unit(2,'lines'),y = unit(0,'npc')+unit(21.1,'lines'),gp = gpar(fontsize=4))
  grid.text(paste0('% of GDP per capita'),x = unit(0,'npc')+unit(7.5,'lines'),y = unit(0,'npc')+unit(21.1,'lines'),gp = gpar(fontsize=4))
  
  #grid.text(paste0('ASEAN Countries ordered by GDP per capita'),x = unit(0,'npc')+unit(7.9,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=6,fontface='bold'))
   
  # bar chart
  for (i in (1:10)){
    grid.polygon(x = c(i+c(-0.4,0.4),rev(i+c(-0.4,0.4))),
                 y = c(0,0,dataset[i,]$perdose,
                       dataset[i,]$perdose),
                 default.units = 'native',
                 gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
  }
  
  #lines
  grid.lines(seq(1,10), dataset$perdose_gdppc_percent*60, default.units = "native",
             gp=gpar(col="red", lwd=0.8))
  grid.points(seq(1,10), dataset$perdose_gdppc_percent*60, default.units = "native",
              gp=gpar(col="red", cex=0.4),pch=16)
  
  grid.yaxis(at=c(0,12,24,36,48,60), 
             label=c("0.0%","0.2%","0.4%","0.6%","0.8%","1.0%"),gp=gpar(fontsize=5),main=FALSE)
  grid.yaxis(at=c(0,10,20,30,40,50,60), 
             label=c("$0","$10","$20","$30","$40","$50","$60"),gp=gpar(fontsize=5))
  
  grid.xaxis(at=seq(1,10), 
             label=c("SG","BN","MY","TH","ID","VN","PH","LA","KH","MM"),gp=gpar(fontsize=4.5))
  
  
  popViewport()
}
