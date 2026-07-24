
percent_share <- function(){
  
  bilateral$iso2c <- countrycode(bilateral$iso3c,
                                 origin='iso3c',destination = 'iso2c')
  order_magnitude <- order_of_magnitude(max(percent_share_df[,'97.5%']))
  val_scale <- c(-1,ceiling(max(percent_share_df[,'97.5%'])/order_magnitude)*order_magnitude)
  pushViewport(plotViewport(c(1,2,1,1),xscale=c(0.5,12.5),yscale=val_scale))
  grid.rect()
  
  # grid.yaxis(at=c(0,10,20,30,40,50,60), 
  #            label=c("$0","$10","$20","$30","$40","$50","$60"),gp=gpar(fontsize=7))
  y_axis_locations <- axisTicks(val_scale,log=FALSE)
  y_axis_labels <- sapply(y_axis_locations,function(x){paste0(x,'%')})
  
  grid.yaxis(at = y_axis_locations, label = y_axis_labels, gp=gpar(fontsize=6,col='black'))
  
  grid.xaxis(at=seq(1,11), 
             label=c(bilateral$iso2c),gp=gpar(fontsize=6))
  
  grid.text('g) Percentage share',x = unit(0,'npc')+unit(3.4,'lines'),
            y = unit(0,'npc')+unit(61.3,'lines'),
            gp = gpar(fontsize=6,fontface='bold'))
  
  for (i in 1:length(bilateral$Country)){
    # gdppc_adj
    grid.lines(x=c(i,i),y=c(adj_gdppc_min$percent_share[i],
                            adj_gdppc_max$percent_share[i]),
               gp=gpar(col="orange",lwd=1),
               default.units = 'native')
    # grid.lines(x=c(i-0.1,i+0.1),y=c(adj_gdppc_min$percent_share[i],
    #                                   adj_gdppc_min$percent_share[i]),
    #            gp=gpar(col="orange",lwd=1),
    #            default.units = 'native')
    # grid.lines(x=c(i-0.1,i+0.1),y=c(adj_gdppc_max$percent_share[i],
    #                                   adj_gdppc_max$percent_share[i]),
    #            gp=gpar(col="orange",lwd=1),
    #            default.units = 'native')
    grid.points(seq(1,11), adj_gdppc$percent_share, default.units = "native",
                gp=gpar(col="orange", cex=0.45,alpha=0.5),pch=16)
    
    grid.text('GDPPC-adj',x = unit(0,'npc')+unit(4.5,'lines'),
              y = unit(0,'npc')+unit(37,'lines'),
              gp = gpar(fontsize=5))
    grid.polygon(x = c(0.7,1,1,0.7),
                 y = c(62.7,62.7,63.2,63.2),
                 default.units = 'native',
                 gp = gpar(col = 'orange', fill = 'orange'))
    
    # chepc_adj
    grid.lines(x=c(i+0.1,i+0.1),y=c(adj_chepc_min$percent_share[i],
                                      adj_chepc_max$percent_share[i]),
               gp=gpar(col="green",lwd=1),
               default.units = 'native')
    
    # grid.lines(x=c(i,i+0.2),y=c(adj_chepc_min$percent_share[i],
    #                             adj_chepc_min$percent_share[i]),
    #            gp=gpar(col="green",lwd=1),
    #            default.units = 'native')
    # grid.lines(x=c(i,i+0.2),y=c(adj_chepc_max$percent_share[i],
    #                             adj_chepc_max$percent_share[i]),
    #            gp=gpar(col="green",lwd=1),
    #            default.units = 'native')
    # 
    grid.points(seq(1.1,11.1), adj_chepc$percent_share, default.units = "native",
                gp=gpar(col="green", cex=0.45,alpha=0.5),pch=16)
    
    grid.text('CHEPC-adj',x = unit(0,'npc')+unit(4.5,'lines'),
              y = unit(0,'npc')+unit(35.5,'lines'),
              gp = gpar(fontsize=5))
    grid.polygon(x = c(0.7,1,1,0.7),
                 y = c(60.2,60.2,60.7,60.7),
                 default.units = 'native',
                 gp = gpar(col = 'green', fill = 'green'))
    grid.lines(x=c(i+0.15,i+0.15), y =c(percent_share_df[i,'2.5%'],
                                        percent_share_df[i,'97.5%']),default.units = "native",
                gp=gpar(col="grey50", cex=0.45,alpha=0.5))
     }
  
  grid.points(seq(1.15,11.15), percent_share_df[,'50%'], default.units = "native",
              gp=gpar(col="grey50", cex=0.45,alpha=0.5),pch=16)

  grid.polygon(x = c(0.7,1,1,0.7),
               y = c(67.7,67.7,68.2,68.2),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50'))
  grid.text("Bilateral",x = unit(0,'npc')+unit(4.5,'lines'),y = unit(0,'npc')+unit(40,'lines'),
            gp = gpar(fontsize=5))
  
  
  grid.points(seq(1,11), uniform$percent_share, default.units = "native",
              gp=gpar(col="blue", cex=0.45,alpha=0.5),pch=16)
  grid.polygon(x = c(0.7,1,1,0.7),
               y = c(65.2,65.2,65.7,65.7),
               default.units = 'native',
               gp = gpar(col = 'blue', fill = 'blue'))
    grid.text("Uniform",x = unit(0,'npc')+unit(4.5,'lines'),y = unit(0,'npc')+unit(38.5,'lines'),
            gp = gpar(fontsize=5))

    grid.text("d) Percent share",x = unit(0,'npc')+unit(3,'lines'),
              y = unit(0,'npc')+unit(26.8,'lines'),
              gp = gpar(fontsize=8,fontface='bold'))  
  
  popViewport()
}
