png(paste0("output/plots/waterfall_plot_weighted_by_gdp.png"),height=8,width=8,units='cm',res=700,pointsize=10)
if(1)
{
  
  pushViewport(plotViewport(c(4,5,1,1),xscale=c(0.5,10.5),yscale=c(0,2.1)))#)NEW
  grid.rect()
  
   
   grid.yaxis(at=c(0.5,1.5), 
              label=c("1.125%","0.375%"),gp=gpar(fontsize=7, col="red"))
   grid.yaxis(at=c(0,1,2), 
              label=c("$1","$10","$100"),gp=gpar(fontsize=7, col="#0042A6"))
  grid.xaxis(at=seq(1,10), 
             label=c("SG","BN","MY","TH","ID","VN","PH","LA","KH","MM"),gp=gpar(fontsize=8))
  
  #grid.yaxis(gp=gpar(fontsize=8,col="#0042A6"))
  #grid.xaxis()
  #grid.lines(gp=gpar(lty=2,lwd=0.8))
  grid.text(paste0('ASEAN Countries ordered by GDP per capita'),x = unit(0,'npc')+unit(6,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  grid.text(paste0('Price per dose'),x = unit(0,'npc')+unit(-5.2,'lines'),rot=90,gp = gpar(fontsize=9,fontface='bold',col = '#0042A6'))
  grid.text(paste0('Affordability index'),x = unit(0,'npc')+unit(-3.8,'lines'),rot=90,gp = gpar(fontsize=9,fontface='bold',col = 'red'))
  
  
  for (i in (1:10)){
    grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
                 y = c(0,0,log(x=ordered_data[i,18],base=10),log(x=ordered_data[i,18],base=10)),
                 default.units = 'native',
                 gp = gpar(col = '#0042A6', fill = '#0042A6'))
  }
  
  grid.lines(seq(1,10), ordered_data[1:10,25]*2.1/1.5, default.units = "native",
             gp=gpar(col="red", lwd=0.8))
  
  grid.points(seq(1,10), ordered_data[1:10,25]*2.1/1.5, default.units = "native",
              gp=gpar(col="red", cex=0.4),pch=16)
  popViewport()
}
dev.off()