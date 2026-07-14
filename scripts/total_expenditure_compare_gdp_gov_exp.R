#uniform <- uniform[order(-dataset$`GDP per capita (2022)`),]

library(grid)

total_expenditure_compare_gdp_gov_exp <- function(coverage){
  if (coverage=='full'){
    bilateral <- bilateral_full
    uniform <- uniform_full
    adj_gdppc <- adj_gdppc_full
    adj_gov_exp <- adj_chepc_full
  }
  if (coverage=='real'){
    bilateral <- bilateral_real
    uniform <- uniform_real
    adj_gdppc <- adj_gdppc_real
    adj_gov_exp <- adj_chepc_real
  }
  #png(paste0("output/plots/total_exp.png"),height=8,width=8,units='cm',res=700,pointsize=10)
  pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,10),xscale=c(0,8*10^9)))
  #grid.rect(gp=gpar(fill=NA))
  
  grid.xaxis(gp=gpar(fontsize=5,col='black'),at = c(0,2*10^9,4*10^9,6*10^9,8*10^9),
             label = c('0','2,000','4,000','6,000','8,000'))
  
  # grid.text('Total expenditure for ASEAN (US$ millions)',y = unit(0,'npc')+unit(-2.2,'lines'),
  #           gp = gpar(fontsize=6,fontface='bold',col = 'black'))
  
  grid.text('f) Total expenditure for SEA (US$ millions)',gp=gpar(fontsize=6,fontface='bold'),
            x = unit(0,'npc')+unit(5,'lines'),
            y = unit(0,'npc')+unit(14.7,'lines'),)
  
  # bilateral  
  grid.polygon(x = c(0,sum(bilateral$total_perdose),
                     rev(c(0,sum(bilateral$total_perdose)))),
               y = c(7,7,8,8),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
  grid.text('Bilateral',x=unit(-1.1*10^9,units='native'),y=unit(7.5,units = 'native'),
            gp = gpar(fontsize=5,col = 'black'))
  total_bilateral_exp <- round(sum(bilateral$total_perdose)/10^6,1)
  grid.text(paste0(total_bilateral_exp,'M'),x=unit(7.7*10^9,units='native'),y=unit(7.5,units = 'native'),
            gp = gpar(fontsize=5,col = 'black'))
  
  # uniform
  grid.polygon(x = c(0,sum(uniform$total_perdose),
                     rev(c(0,sum(uniform$total_perdose)))),
               y = c(5,5,6,6),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
  
  grid.polygon(x = c(sum(uniform$total_perdose),sum(bilateral$total_perdose),
                     rev(c(sum(uniform$total_perdose),sum(bilateral$total_perdose)))),
               y = c(5,5,6,6),
               default.units = 'native',
               gp = gpar(col = 'red', fill = 'white',alpha=0.5,lty='dashed'))
  uniform_percent_reduction <- as.character(round((sum(uniform$total_perdose)-sum(bilateral$total_perdose))*100/
                                                    sum(bilateral$total_perdose),1))
  
  grid.text(paste0(uniform_percent_reduction,'%'),x=unit(7.7*10^9,units='native'),y=unit(5.5,units = 'native'),
            gp = gpar(fontsize=5,col = 'red'))
  
  grid.text('Uniform',x=unit(-1.1*10^9,units='native'),y=unit(5.5,units = 'native'),
            gp = gpar(fontsize=5,col = 'black'))
  
  # Adjusted GDPPC
  grid.polygon(x = c(0,sum(adj_gdppc$total_perdose),
                     rev(c(0,sum(adj_gdppc$total_perdose)))),
               y = c(3,3,4,4),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
  
  grid.polygon(x = c(sum(adj_gdppc$total_perdose),sum(bilateral$total_perdose),
                     rev(c(sum(adj_gdppc$total_perdose),sum(bilateral$total_perdose)))),
               y = c(3,3,4,4),
               default.units = 'native',
               gp = gpar(col = 'red', fill = 'white',alpha=0.5,lty='dashed'))
  adj_percent_reduction <- as.character(round((sum(adj_gdppc$total_perdose)-sum(bilateral$total_perdose))*100/
                                                sum(bilateral$total_perdose),1))
  
  grid.text(paste0(adj_percent_reduction,'%'),x=unit(7.7*10^9,units='native'),y=unit(3.5,units = 'native'),
            gp = gpar(fontsize=5,col = 'red'))
  
  grid.text('GDPPC-\nadjusted',x=unit(-1.3*10^9,units='native'),y=unit(3.5,units = 'native'),
            gp = gpar(fontsize=5,col = 'black'))
  
  # Adjusted gov_exp
  grid.polygon(x = c(0,sum(adj_gov_exp$total_perdose),
                     rev(c(0,sum(adj_gov_exp$total_perdose)))),
               y = c(1,1,2,2),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
  
  grid.polygon(x = c(sum(adj_gov_exp$total_perdose),sum(bilateral$total_perdose),
                     rev(c(sum(adj_gov_exp$total_perdose),sum(bilateral$total_perdose)))),
               y = c(1,1,2,2),
               default.units = 'native',
               gp = gpar(col = 'red', fill = 'white',alpha=0.5,lty='dashed'))
  adj_gov_exp_percent_reduction <- as.character(round((sum(adj_gov_exp$total_perdose)-sum(bilateral$total_perdose))*100/
                                                sum(bilateral$total_perdose),1))
  
  grid.text(paste0(adj_gov_exp_percent_reduction,'%'),x=unit(7.7*10^9,units='native'),y=unit(1.5,units = 'native'),
            gp = gpar(fontsize=5,col = 'red'))
  
  grid.text('CHEPC-\nadjusted',x=unit(-1.3*10^9,units='native'),y=unit(1.5,units = 'native'),
            gp = gpar(fontsize=5,col = 'black'))
  
  popViewport()
}