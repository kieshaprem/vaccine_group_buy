bilateral$percent_share <- bilateral$total_perdose*100/sum(bilateral$total_perdose)
colnames(uniform)[ncol(uniform)] <- "percent_share"
uniform$percent_share <- uniform$total_perdose*100/sum(uniform$total_perdose)
gdp_adj$percent_share <- gdp_adj$total_perdose*100/sum(gdp_adj$total_perdose)

library(grid)

png(paste0("output/plots/percent_share_scatter.png"),height=8,width=8,units='cm',res=700,pointsize=10)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(3,4,1,1),xscale=c(0.5,10.5),yscale=c(0,45)))#)NEW
  grid.rect()
  
  # grid.yaxis(at=c(0,10,20,30,40,50,60), 
  #            label=c("$0","$10","$20","$30","$40","$50","$60"),gp=gpar(fontsize=7))
  grid.yaxis(at=c(0,10,20,30,40),label=c("0%","10%","20%","30%","40%"))
  grid.xaxis(at=seq(1,10), 
             label=rev(c("SG","BN","MY","TH","ID","VN","PH","LA","KH","MM")),gp=gpar(fontsize=8))
  
  grid.text(paste0('ASEAN countries ordered by GDP per capita'),x = unit(0,'npc')+unit(7.9,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=8,fontface='bold'))
  grid.text('Percentage share',
            x = unit(0,'npc')+unit(-4.5,'lines'),
            rot=90,gp = gpar(fontsize=8,fontface='bold'))  #grid.text(paste0('$'),x = unit(0,'npc')+unit(1.7,'lines'),y = unit(0,'npc')+unit(17.1,'lines'),gp = gpar(fontsize=8))
  #grid.text(paste0('% of GDP per capita'),x = unit(0,'npc')+unit(7.2,'lines'),y = unit(0,'npc')+unit(17.1,'lines'),gp = gpar(fontsize=8))
  
  grid.points(seq(1,10), bilateral$percent_share, default.units = "native",
              gp=gpar(col="grey", cex=0.4),pch=16)
  
  grid.points(seq(1,10), uniform$percent_share, default.units = "native",
              gp=gpar(col="blue", cex=0.4, alpha=0.5),pch=16)
  
  grid.points(seq(1,10), gdp_adj$percent_share, default.units = "native",
              gp=gpar(col="orange", cex=0.4, alpha=0.5),pch=16)
  
  # grid.polygon(x = c(0.8,1.2,1.2,0.8),
  #              y = c(63,63,64,64),
  #              default.units = 'native',
  #              gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
  # 
  # grid.polygon(x = c(2,2.4,2.4,2),
  #              y = c(63,63,64,64),
  #              default.units = 'native',
  #              gp = gpar(col = 'red', fill = 'red'))
  popViewport()
}
dev.off()

