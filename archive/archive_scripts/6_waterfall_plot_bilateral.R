#bilateral <- bilateral[order(-dataset$`GDP per capita (2022)`),]

png(paste0("output/plots/waterfall_plot_bilateral.png"),height=8,width=8,units='cm',res=700,pointsize=10)
if(1)
{
  
  pushViewport(plotViewport(c(3,3,3,3),xscale=c(0.5,10.5),yscale=c(0,60)))#)NEW
  grid.rect()
  
  grid.yaxis(at=c(0,12,24,36,48,60), 
             label=c("0.0%","0.2%","0.4%","0.6%","0.8%","1.0%"),gp=gpar(fontsize=7),main=FALSE)
  grid.yaxis(at=c(0,10,20,30,40,50,60), 
             label=c("$0","$10","$20","$30","$40","$50","$60"),gp=gpar(fontsize=7))
  
  grid.xaxis(at=seq(1,10), 
             label=rev(c("SG","BN","MY","TH","ID","VN","PH","LA","KH","MM")),gp=gpar(fontsize=8))
  
  
  grid.text(paste0('ASEAN Countries ordered by GDP per capita'),x = unit(0,'npc')+unit(7.9,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=8,fontface='bold'))
  grid.text(paste0('Price per dose, Bilateral Pricing'),x = unit(0,'npc')+unit(6,'lines'),y = unit(0,'npc')+unit(18.4,'lines'),gp = gpar(fontsize=8,fontface='bold'))
  grid.text(paste0('$'),x = unit(0,'npc')+unit(1.7,'lines'),y = unit(0,'npc')+unit(17.1,'lines'),gp = gpar(fontsize=8))
  grid.text(paste0('% of GDP per capita'),x = unit(0,'npc')+unit(7.2,'lines'),y = unit(0,'npc')+unit(17.1,'lines'),gp = gpar(fontsize=8))
  
  
  for (i in (1:10)){
    grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
                 y = c(0,0,bilateral[i,]$perdose,
                       bilateral[i,]$perdose),
                 default.units = 'native',
                 gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
  }
  
  grid.lines(seq(1,10), bilateral$perdose_gdppc_percent*60, default.units = "native",
             gp=gpar(col="red", lwd=0.8))
  
  grid.points(seq(1,10), bilateral$perdose_gdppc_percent*60, default.units = "native",
              gp=gpar(col="red", cex=0.4),pch=16)
  
  grid.polygon(x = c(0.8,1.2,1.2,0.8),
               y = c(63,63,64,64),
               default.units = 'native',
               gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
  
  grid.polygon(x = c(2,2.4,2.4,2),
               y = c(63,63,64,64),
               default.units = 'native',
               gp = gpar(col = 'red', fill = 'red'))
  
  popViewport()
}
dev.off()

