#gdp_adj <- gdp_adj[order(-dataset$`GDP per capita (2022)`),]

png(paste0("output/plots/waterfall_plot_weighted_adj_gdp.png"),height=8,width=8,units='cm',res=700,pointsize=10)
if(1)
{
  
  pushViewport(plotViewport(c(3,3,3,3),xscale=c(0.5,10.5),yscale=c(0,2.1)))#)NEW
  grid.rect()
  
  grid.yaxis(at=c(0,1,2), 
             label=c("0%","0.5%","1%"),gp=gpar(fontsize=7),main=FALSE)
  grid.yaxis(at=c(0,1,2), 
             label=c("$1","$10","$100"),gp=gpar(fontsize=7))
  
  grid.xaxis(at=seq(1,10), 
             label=rev(c("SG","BN","MY","TH","ID","VN","PH","LA","KH","MM")),
             gp=gpar(fontsize=8))
  
  
  grid.text(paste0('ASEAN Countries ordered by GDP per capita'),x = unit(0,'npc')+unit(7.9,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=8,fontface='bold'))
  grid.text(paste0('Price per dose, Weighted by GDP per capita,'),x = unit(0,'npc')+unit(6,'lines'),y = unit(0,'npc')+unit(18.4,'lines'),gp = gpar(fontsize=8,fontface='bold'))
  grid.text(as.expression(bquote(alpha == 10)), x=1.01,y=1.145,gp=gpar(fontsize=8))
  grid.text(paste0('$'),x = unit(0,'npc')+unit(1.7,'lines'),y = unit(0,'npc')+unit(17.1,'lines'),gp = gpar(fontsize=8))
  grid.text(paste0('% of GDP per capita'),x = unit(0,'npc')+unit(7.2,'lines'),y = unit(0,'npc')+unit(17.1,'lines'),gp = gpar(fontsize=8))
  
  
  for (i in (1:10)){
    grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
                 y = c(0,0,log(x=gdp_adj[i,]$perdose,base=10),
                       log(x=gdp_adj[i,]$perdose,base=10)),
                 default.units = 'native',
                 gp = gpar(col = '#0042A6', fill = '#0042A6'))
  }
  
  grid.lines(seq(1,10), gdp_adj$perdose_gdppc_percent*2.1, default.units = "native",
             gp=gpar(col="red", lwd=0.8))
  
  grid.points(seq(1,10), gdp_adj$perdose_gdppc_percent*2.1, default.units = "native",
              gp=gpar(col="red", cex=0.4),pch=16)
  
  grid.polygon(x = c(0.8,1.2,1.2,0.8),
               y = c(2.2,2.2,2.25,2.25),
               default.units = 'native',
               gp = gpar(col = '#0042A6', fill = '#0042A6'))
  
  grid.polygon(x = c(2,2.4,2.4,2),
               y = c(2.2,2.2,2.25,2.25),
               default.units = 'native',
               gp = gpar(col = 'red', fill = 'red'))
  popViewport()
}
dev.off()

