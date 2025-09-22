percent_share <- function(){
  pushViewport(plotViewport(c(1,2,1,1),xscale=c(0.5,10.5),yscale=c(-1,45)))#)NEW
  grid.rect()
  
  # grid.yaxis(at=c(0,10,20,30,40,50,60), 
  #            label=c("$0","$10","$20","$30","$40","$50","$60"),gp=gpar(fontsize=7))
  grid.yaxis(at=c(0,10,20,30,40),label=c("0%","10%","20%","30%","40%"),gp=gpar(fontsize=5))
  grid.xaxis(at=seq(1,10), 
             label=c("SG","BN","MY","TH","ID","VN","PH","LA","KH","MM"),gp=gpar(fontsize=5))
  
  #grid.text(paste0('ASEAN countries ordered by GDP per capita'),x = unit(0,'npc')+unit(7,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  grid.text('e) Percentage share',x = unit(0,'npc')+unit(3.4,'lines'),
            y = unit(0,'npc')+unit(16.3,'lines'),
            gp = gpar(fontsize=6,fontface='bold'))
  
  for (i in 1:10){
    grid.lines(x=c(i,i),y=c(gdp_adj_analysis_alpha$percent_share_min[i],
                            gdp_adj_analysis_alpha$percent_share_max[i]),
               gp=gpar(col="orange",lwd=1),
               default.units = 'native')
    grid.lines(x=c(i-0.15,i+0.15),y=c(gdp_adj_analysis_alpha$percent_share_min[i],
                                      gdp_adj_analysis_alpha$percent_share_min[i]),
               gp=gpar(col="orange",lwd=1),
               default.units = 'native')
    grid.lines(x=c(i-0.15,i+0.15),y=c(gdp_adj_analysis_alpha$percent_share_max[i],
                                      gdp_adj_analysis_alpha$percent_share_max[i]),
               gp=gpar(col="orange",lwd=1),
               default.units = 'native')
  }
  
  grid.points(seq(1.15,10.15), bilateral$share, default.units = "native",
              gp=gpar(col="grey50", cex=0.55),pch=16)
  
  grid.points(seq(1,10), uniform$share, default.units = "native",
              gp=gpar(col="blue", cex=0.55),pch=16)
  
  grid.points(seq(1,10), gdp_adj_analysis_alpha$percent_share_median, default.units = "native",
              gp=gpar(col="orange", cex=0.55),pch=16)
  
  grid.polygon(x = c(6.6,6.9,6.9,6.6),
               y = c(38,38,38.5,38.5),
               default.units = 'native',
               gp = gpar(col = 'orange', fill = 'orange'))
  
  grid.polygon(x = c(6.6,6.9,6.9,6.6),
               y = c(40.5,40.5,41,41),
               default.units = 'native',
               gp = gpar(col = 'blue', fill = 'blue'))
  
  grid.polygon(x = c(6.6,6.9,6.9,6.6),
               y = c(43,43,43.5,43.5),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50'))
  
  grid.text("Bilateral    ",x = unit(0,'npc')+unit(15.5,'lines'),y = unit(0,'npc')+unit(21.2,'lines'),
            gp = gpar(fontsize=4))
  grid.text("Uniform      ",x = unit(0,'npc')+unit(15.7,'lines'),y = unit(0,'npc')+unit(20,'lines'),
            gp = gpar(fontsize=4))
  grid.text("GDPPC-weighted",x = unit(0,'npc')+unit(16.7,'lines'),y = unit(0,'npc')+unit(18.8,'lines'),
            gp = gpar(fontsize=4))
  popViewport()
}
