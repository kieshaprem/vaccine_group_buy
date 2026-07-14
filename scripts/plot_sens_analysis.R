percent_share <- function(mechanism, coverage, compare_gdp_gov_exp=FALSE){
  if (coverage=='full'){
    bilateral_analysis_price <- bilateral_full_analysis_price
    uniform_analysis_price <- uniform_full_analysis_price
    if (mechanism=='gdp_adj'){
      adj_analysis_alpha <- gdp_adj_full_analysis_alpha
    }
    if (mechanism=='gov_exp_adj'){
      adj_analysis_alpha <- gov_exp_adj_full_analysis_alpha
    }
    if (mechanism=='chepc_adj'){
      adj_analysis_alpha <- chepc_adj_full_analysis_alpha
    }
  }
  if (coverage=='real'){
    bilateral_analysis_price <- bilateral_real_analysis_price
    uniform_analysis_price <- uniform_real_analysis_price
    if (mechanism=='gdp_adj'){
      adj_analysis_alpha <- gdp_adj_real_analysis_alpha
    }
    if (mechanism=='gov_exp_adj'){
      adj_analysis_alpha <- gov_exp_adj_real_analysis_alpha
    }
    if (mechanism=='chepc_adj'){
      adj_analysis_alpha <- chepc_adj_real_analysis_alpha
    }
  }
  adj_analysis_alpha <- adj_analysis_alpha[adj_analysis_alpha$Country==bilateral_full$iso3c,]
  adj_analysis_alpha$iso2c <- countrycode(adj_analysis_alpha$Country,
                                              origin='iso3c',destination = 'iso2c')
  pushViewport(plotViewport(c(1,2,1,1),xscale=c(0.5,12.5),yscale=c(-1,50)))#)NEW
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
  if(compare_gdp_gov_exp==TRUE){
    if (coverage=='full'){
      other_percent_share <- gov_exp_adj_full_analysis_alpha
    }
    if (coverage=='real'){
      other_percent_share <- gov_exp_adj_real_analysis_alpha
    }
    for (i in 1:length(other_percent_share$Country)){
      grid.lines(x=c(i+0.15,i+0.15),y=c(other_percent_share$percent_share_min[i],
                              other_percent_share$percent_share_max[i]),
                 gp=gpar(col="green",lwd=1),
                 default.units = 'native')
      grid.lines(x=c(i,i+0.3),y=c(other_percent_share$percent_share_min[i],
                                        other_percent_share$percent_share_min[i]),
                 gp=gpar(col="green",lwd=1),
                 default.units = 'native')
      grid.lines(x=c(i,i+0.3),y=c(other_percent_share$percent_share_max[i],
                                        other_percent_share$percent_share_max[i]),
                 gp=gpar(col="green",lwd=1),
                 default.units = 'native')
      grid.text('CHEPC-adj',x = unit(0,'npc')+unit(3.4,'lines'),
                y = unit(0,'npc')+unit(20.2,'lines'),
                 gp = gpar(fontsize=3.5))
      grid.polygon(x = c(0.7,1,1,0.7),
                   y = c(35.5,35.5,36,36),
                   default.units = 'native',
                   gp = gpar(col = 'green', fill = 'green'))
      grid.points(seq(1.15,11.15), other_percent_share$percent_share_median, default.units = "native",
                  gp=gpar(col="green", cex=0.45,alpha=0.5),pch=16)
    }
  }
  
  grid.points(seq(1.15,11.15), bilateral_analysis_price$percent_share_median, default.units = "native",
              gp=gpar(col="grey50", cex=0.45,alpha=0.5),pch=16)
  
  grid.points(seq(1,11), uniform_analysis_price$percent_share_median, default.units = "native",
              gp=gpar(col="blue", cex=0.45,alpha=0.5),pch=16)
  
  grid.points(seq(1,11), adj_analysis_alpha$percent_share_median, default.units = "native",
              gp=gpar(col="orange", cex=0.45,alpha=0.5),pch=16)
  
  grid.polygon(x = c(0.7,1,1,0.7),
               y = c(42,42,42.5,42.5),
               default.units = 'native',
               gp = gpar(col = 'orange', fill = 'orange'))
  
  grid.polygon(x = c(0.7,1,1,0.7),
               y = c(45.5,45.5,45,45),
               default.units = 'native',
               gp = gpar(col = 'blue', fill = 'blue'))
  
  grid.polygon(x = c(0.7,1,1,0.7),
               y = c(48,48,48.5,48.5),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50'))
  
  grid.text("Bilateral    ",x = unit(0,'npc')+unit(3,'lines'),y = unit(0,'npc')+unit(21.2,'lines'),
            gp = gpar(fontsize=4))
  grid.text("Uniform      ",x = unit(0,'npc')+unit(3.3,'lines'),y = unit(0,'npc')+unit(20,'lines'),
            gp = gpar(fontsize=4))
  # if (mechanism=='gov_exp_adj'){
  #   mechanism <- 'gov_exp adjusted'
  # }
  if (mechanism=='gdp_adj'){
    mechanism <- 'GDPPC-adj'
  }
  if (mechanism=='chepc_adj'){
    mechanism <- 'CHEPC-adj'
    
  }
  grid.text(mechanism,x = unit(0,'npc')+unit(3.3,'lines'),
            y = unit(0,'npc')+unit(21.4,'lines'),
            gp = gpar(fontsize=3.5))
  
  popViewport()
}
