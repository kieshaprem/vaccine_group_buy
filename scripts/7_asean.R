asean_df = data.frame(array(dim=c(3,4)))
colnames(asean_df) <- c("scenario","total_price_paid","savings","percent_savings")
asean_df[,1] <- c("bilateral","pooled_uniform","pooled_gdp")
asean_df[,2] <- c(6810766580.00,5096212515.00,5046102077.06)
asean_df[,3] <- c(NA,1714554065.00,1764664502.94)
asean_df[,4] <- c(NA,25.2,25.9)

library(grid)
png(paste0("output/plots/asean_comparison.png"),height=8,width=8,units='cm',res=700,pointsize=10)
if(1)
{
  pushViewport(plotViewport(c(4,4,1,1),xscale=c(0,4),yscale=c(0,8)))#)NEW
  grid.rect()
  
  grid.yaxis(at=c(2,4,6,8), 
             label=c("2T","4T","6T","8T"),gp=gpar(fontsize=7))
  grid.xaxis(at=c(1,2,3), 
             label=c("Bilateral","Pooled\nUniform","Pooled\nAdjusted by GDP"),gp=gpar(fontsize=8))
  
  
  # grid.text(paste0('ASEAN Countries ordered by GDP per capita'),x = unit(0,'npc')+unit(7,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=9,fontface='bold'))
  # grid.text(paste0('Price per dose, Weighted by Adj GDP per capita'),x = unit(0,'npc')+unit(7,'lines'),y = unit(0,'npc')+unit(16.4,'lines'),gp = gpar(fontsize=9,fontface='bold'))
  # grid.text(paste0('$'),x = unit(0,'npc')+unit(1.7,'lines'),y = unit(0,'npc')+unit(17.1,'lines'),gp = gpar(fontsize=8))
  # grid.text(paste0('% of GDP per capita'),x = unit(0,'npc')+unit(7.2,'lines'),y = unit(0,'npc')+unit(17.1,'lines'),gp = gpar(fontsize=8))
  
  
  for (i in (1:3)){
    grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
                 y = c(0,0,(asean_df$total_price_paid[i]/10^9-0),
                       (asean_df$total_price_paid[i]/10^9)-0),
                 default.units = 'native',
                 gp = gpar(col = '#0042A6', fill = '#0042A6'))
    
    if (i>1){
      grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
                   y = c(0,0,(asean_df$savings[i]/10^9-0),
                         (asean_df$savings[i]/10^9-0)),
                   default.units = 'native',
                   gp = gpar(col = 'brown', fill = 'brown'))
    }
    
  }

  popViewport()
}
dev.off()

