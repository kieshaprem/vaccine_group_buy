percent_share <- function(mechanism, coverage, compare_gdp_oop=FALSE){
  if (coverage=='full'){
    bilateral_analysis_price <- bilateral_full_analysis_price
    uniform_analysis_price <- uniform_full_analysis_price
    if (mechanism=='gdp_adj'){
      adj_analysis_alpha <- gdp_adj_full_analysis_alpha
    }
    if (mechanism=='ooppc_adj'){
      adj_analysis_alpha <- oop_adj_full_analysis_alpha
    }
  }
  if (coverage=='partial'){
    bilateral_analysis_price <- bilateral_partial_analysis_price
    uniform_analysis_price <- uniform_partial_analysis_price
    if (mechanism=='gdp_adj'){
      adj_analysis_alpha <- gdp_adj_partial_analysis_alpha
    }
    if (mechanism=='ooppc_adj'){
      adj_analysis_alpha <- oop_adj_partial_analysis_alpha
    }
  }
  adj_analysis_alpha$iso2c <- countrycode(adj_analysis_alpha$Country,
                                              origin='iso3c',destination = 'iso2c')
  pushViewport(plotViewport(c(1,2,1,1),xscale=c(0.5,12.5),yscale=c(-1,45)))#)NEW
  grid.rect()
  
  # grid.yaxis(at=c(0,10,20,30,40,50,60), 
  #            label=c("$0","$10","$20","$30","$40","$50","$60"),gp=gpar(fontsize=7))
  grid.yaxis(at=c(0,10,20,30,40),label=c("0%","10%","20%","30%","40%"),gp=gpar(fontsize=5))
  grid.xaxis(at=seq(1,11), 
             label=c(adj_analysis_alpha$iso2c),gp=gpar(fontsize=5))
  
  #grid.text(paste0('ASEAN countries ordered by GDP per capita'),x = unit(0,'npc')+unit(7,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  grid.text('e) Percentage share',x = unit(0,'npc')+unit(3.4,'lines'),
            y = unit(0,'npc')+unit(16.3,'lines'),
            gp = gpar(fontsize=6,fontface='bold'))
  
  for (i in 1:length(adj_analysis_alpha$Country)){
    grid.lines(x=c(i,i),y=c(adj_analysis_alpha$percent_share_min[i],
                            adj_analysis_alpha$percent_share_max[i]),
               gp=gpar(col="orange",lwd=1),
               default.units = 'native')
    grid.lines(x=c(i-0.15,i+0.15),y=c(adj_analysis_alpha$percent_share_min[i],
                                      adj_analysis_alpha$percent_share_min[i]),
               gp=gpar(col="orange",lwd=1),
               default.units = 'native')
    grid.lines(x=c(i-0.15,i+0.15),y=c(adj_analysis_alpha$percent_share_max[i],
                                      adj_analysis_alpha$percent_share_max[i]),
               gp=gpar(col="orange",lwd=1),
               default.units = 'native')
  }
  # if doing the comparison plot, set mechanism = 'gdp_adj'
  if(compare_gdp_oop==TRUE){
    if (coverage=='full'){
      other_percent_share <- oop_adj_full_analysis_alpha
    }
    if (coverage=='partial'){
      other_percent_share <- oop_adj_partial_analysis_alpha
    }
    for (i in 1:length(other_percent_share$Country)){
      grid.lines(x=c(i,i),y=c(other_percent_share$percent_share_min[i],
                              other_percent_share$percent_share_max[i]),
                 gp=gpar(col="green",lwd=1),
                 default.units = 'native')
      grid.lines(x=c(i-0.2,i+0.2),y=c(other_percent_share$percent_share_min[i],
                                        other_percent_share$percent_share_min[i]),
                 gp=gpar(col="green",lwd=1),
                 default.units = 'native')
      grid.lines(x=c(i-0.2,i+0.2),y=c(other_percent_share$percent_share_max[i],
                                        other_percent_share$percent_share_max[i]),
                 gp=gpar(col="green",lwd=1),
                 default.units = 'native')
      grid.text('CHE per capita\nadjusted',x = unit(0,'npc')+unit(4.7,'lines'),
                y = unit(0,'npc')+unit(16.6,'lines'),
                gp = gpar(fontsize=4,col='black'))
      grid.polygon(x = c(0.7,1,1,0.7),
                   y = c(35.5,35.5,36,36),
                   default.units = 'native',
                   gp = gpar(col = 'green', fill = 'green'))
    }
  }
  
  grid.points(seq(1.15,11.15), bilateral_analysis_price$percent_share_median, default.units = "native",
              gp=gpar(col="grey50", cex=0.55),pch=16)
  
  grid.points(seq(1,11), uniform_analysis_price$percent_share_median, default.units = "native",
              gp=gpar(col="blue", cex=0.55),pch=16)
  
  grid.points(seq(1,11), adj_analysis_alpha$percent_share_median, default.units = "native",
              gp=gpar(col="orange", cex=0.55),pch=16)
  
  grid.polygon(x = c(0.7,1,1,0.7),
               y = c(38,38,38.5,38.5),
               default.units = 'native',
               gp = gpar(col = 'orange', fill = 'orange'))
  
  grid.polygon(x = c(0.7,1,1,0.7),
               y = c(40.5,40.5,41,41),
               default.units = 'native',
               gp = gpar(col = 'blue', fill = 'blue'))
  
  grid.polygon(x = c(0.7,1,1,0.7),
               y = c(43,43,43.5,43.5),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50'))
  
  grid.text("Bilateral    ",x = unit(0,'npc')+unit(3,'lines'),y = unit(0,'npc')+unit(21.2,'lines'),
            gp = gpar(fontsize=4))
  grid.text("Uniform      ",x = unit(0,'npc')+unit(3.7,'lines'),y = unit(0,'npc')+unit(20,'lines'),
            gp = gpar(fontsize=4))
  if (mechanism=='ooppc_adj'){
    mechanism <- 'CHE per capita adjusted'
  }
  grid.text(mechanism,x = unit(0,'npc')+unit(4.7,'lines'),y = unit(0,'npc')+unit(18.8,'lines'),
            gp = gpar(fontsize=4))
  popViewport()
}
