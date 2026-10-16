#uniform <- uniform[order(-dataset$`GDP per capita (2022)`),]

library(grid)

total_expenditure <- function(coverage,mechanism){
  if (coverage=='full'){
    bilateral <- bilateral_full
    bilateral_min_end <- bilateral_min_full
    bilateral_max_end <- bilateral_max_full
    
    uniform <- uniform_full
    uniform_min_end <- uniform_min_full
    uniform_max_end <- uniform_max_full
    
    if (mechanism=='gdp_adj'){
      adj <- adj_gdppc_full
      adj_min_end <- adj_gdppc_full_min
      adj_max_end <- adj_gdppc_full_max
    }
    if (mechanism=='gov_exp_adj'){
      adj <- adj_gov_exp_full
    }
    if (mechanism=='chepc_adj'){
      adj <- adj_chepc_full
      adj_min_end <- adj_chepc_full_min
      adj_max_end <- adj_chepc_full_max
    }
  }
  if (coverage=='real'){
    bilateral <- bilateral_real
    bilateral_min_end <- bilateral_min_real
    bilateral_max_end <- bilateral_max_real
    
    uniform <- uniform_real
    uniform_min_end <- uniform_min_real
    uniform_max_end <- uniform_max_real
    
    if (mechanism=='gdp_adj'){
      adj <- adj_gdppc_real
      adj_min_end <- adj_gdppc_real_min
      adj_max_end <- adj_gdppc_real_max
    }
    if (mechanism=='gov_exp_adj'){
      adj <- adj_gov_exp_real
    }
    if (mechanism=='chepc_adj'){
      adj <- adj_chepc_real
      adj_min_end <- adj_chepc_real_min
      adj_max_end <- adj_chepc_real_max
    }
  }
#png(paste0("output/plots/total_exp.png"),height=8,width=8,units='cm',res=700,pointsize=10)
  pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,10),xscale=c(0,20*10^9)))
  #grid.rect(gp=gpar(fill=NA))
  
  grid.xaxis(gp=gpar(fontsize=5,col='black'),at = c(0,5*10^9,10*10^9,15*10^9,20*10^9),
             label = c('0','5,000','10,000','15,000','20,000'))
  
  # grid.text('Total expenditure for ASEAN (US$ millions)',y = unit(0,'npc')+unit(-2.2,'lines'),
  #           gp = gpar(fontsize=6,fontface='bold',col = 'black'))
  
  grid.text('f) Total expenditure for SEA (US$ millions)',gp=gpar(fontsize=6,fontface='bold'),
            x = unit(0,'npc')+unit(5,'lines'),
            y = unit(0,'npc')+unit(14.7,'lines'))

  # bilateral  
  grid.polygon(x = c(0,sum(bilateral$total_perdose),
                     rev(c(0,sum(bilateral$total_perdose)))),
               y = c(7,7,9,9),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
  grid.lines(x=c(sum(bilateral_min_end$total_perdose),sum(bilateral_max_end$total_perdose)),
                 y = c(8,8),default.units = 'native',gp=gpar(col='black',lwd=1))
  grid.text('Bilateral',x=unit(-2.1*10^9,units='native'),y=unit(8,units = 'native'),
            gp = gpar(fontsize=5,col = 'black'))
  total_bilateral_exp <- round(sum(bilateral$total_perdose)/10^6,1)
  grid.text(paste0(total_bilateral_exp,'M'),x=unit(sum(bilateral$total_perdose),units='native'),y=unit(8.3,units = 'native'),
            gp = gpar(fontsize=5,col = 'black'))
  
  # uniform
  grid.polygon(x = c(0,sum(uniform$total_perdose),
                     rev(c(0,sum(uniform$total_perdose)))),
               y = c(4,4,6,6),
               default.units = 'native',
               gp = gpar(col = 'blue', fill = 'blue',alpha=0.5))
 
  # grid.polygon(x = c(sum(uniform$total_perdose),sum(bilateral$total_perdose),
  #                    rev(c(sum(uniform$total_perdose),sum(bilateral$total_perdose)))),
  #              y = c(4,4,6,6),
  #              default.units = 'native',
  #              gp = gpar(col = 'red', fill = 'white',alpha=0.5,lty='dashed'))
  # grid.lines(x=c(sum(uniform_min_end$total_savings_perdose),sum(uniform_max_end$total_savings_perdose)),
  #            y = c(5,5),default.units = 'native',gp=gpar(col='black',lwd=1))
  uniform_percent_reduction <- as.character(round((sum(uniform$total_perdose)-sum(bilateral$total_perdose))*100/
                                       sum(bilateral$total_perdose),1))
  # add range later
  
  grid.text(paste0(uniform_percent_reduction,'%'),x=unit(7.7*10^9,units='native'),y=unit(5.3,units = 'native'),
            gp = gpar(fontsize=5,col = 'red'))
  
  grid.text('Uniform',x=unit(-2.1*10^9,units='native'),y=unit(5,units = 'native'),
            gp = gpar(fontsize=5,col = 'black'))
  
  # Adjusted
  grid.polygon(x = c(0,sum(adj$total_perdose),
                     rev(c(0,sum(adj$total_perdose)))),
               y = c(1,1,3,3),
               default.units = 'native',
               gp = gpar(col = 'orange', fill = 'orange',alpha=0.5))
  
  # grid.polygon(x = c(sum(adj$total_perdose),sum(bilateral$total_perdose),
  #                    rev(c(sum(adj$total_perdose),sum(bilateral$total_perdose)))),
  #              y = c(1,1,3,3),
  #              default.units = 'native',
  #              gp = gpar(col = 'red', fill = 'white',alpha=0.5,lty='dashed'))
  # grid.lines(x=c(sum(adj_min_end$total_savings_perdose),sum(adj_max_end$total_savings_perdose)),
  #            y = c(2,2),default.units = 'native',gp=gpar(col='black',lwd=1))
  adj_percent_reduction <- as.character(round((sum(adj$total_perdose)-sum(bilateral$total_perdose))*100/
                                                sum(bilateral$total_perdose),1))
  
  grid.text(paste0(adj_percent_reduction,'%'),x=unit(7.7*10^9,units='native'),y=unit(2.3,units = 'native'),
            gp = gpar(fontsize=5,col = 'red'))
  if (mechanism=='gov_exp_adj'){
    mechanism <- 'GHEPC\nadjusted'
  }
  if (mechanism=='chepc_adj'){
    mechanism <- 'CHEPC\nadjusted'
  }
  if (mechanism=='gdp_adj'){
    mechanism <- 'GDPPC\nadjusted'
  }
  grid.text(mechanism,x=unit(-2.3*10^9,units='native'),y=unit(2,units = 'native'),
            gp = gpar(fontsize=5,col = 'black'))
  
  popViewport()
}