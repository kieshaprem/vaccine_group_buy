
total_expenditure_ranges <- function(coverage){
  if (coverage=='full'){
    bilateral <- bilateral_full
    bilateral_min_end <- bilateral_min_full
    bilateral_max_end <- bilateral_max_full
    
    uniform <- uniform_full
    uniform_min_end <- uniform_min_full
    uniform_max_end <- uniform_max_full
    
    adj_gdppc <- adj_gdppc_full
    adj_gdppc_min_end <- adj_gdppc_full_min
    adj_gdppc_max_end <- adj_gdppc_full_max
    
    adj_chepc <- adj_chepc_full
    adj_chepc_min_end <- adj_chepc_full_min
    adj_chepc_max_end <- adj_chepc_full_max
  }
  if (coverage=='real'){
    bilateral <- bilateral_real
    bilateral_min_end <- bilateral_min_real
    bilateral_max_end <- bilateral_max_real
    
    uniform <- uniform_real
    uniform_min_end <- uniform_min_real
    uniform_max_end <- uniform_max_real
    
    adj_gdppc <- adj_gdppc_real
    adj_gdppc_min_end <- adj_gdppc_real_min
    adj_gdppc_max_end <- adj_gdppc_real_max
    
    adj_chepc <- adj_chepc_real
    adj_chepc_min_end <- adj_chepc_real_min
    adj_chepc_max_end <- adj_chepc_real_max
  }
  pushViewport(plotViewport(c(1.5,4,1,1),xscale=c(0,10),yscale=c(0,20*10^9)))
  grid.yaxis(gp=gpar(fontsize=5,col='black'),at = c(0,5*10^9,10*10^9,15*10^9,20*10^9),
             label = c('0','5,000','10,000','15,000','20,000'))
  grid.xaxis(gp=gpar(fontsize=5,col='black'),at = c(0,2,4,6,8,10),
             label = c(" ",'Bilateral','Uniform','GDPPC-adj','CHEPC-adj'," "))
  
  grid.text('Total expenditure for SEA (US$ millions)'
            ,gp=gpar(fontsize=7,fontface='bold'),
            x = unit(0,'npc')+unit(-4,'lines'),
            y = unit(0,'npc')+unit(17,'lines'),rot=90)
  
  # bilateral  
  grid.polygon(y = c(0,sum(bilateral$total_perdose),
                     rev(c(0,sum(bilateral$total_perdose)))),
               x = c(1.5,1.5,2.5,2.5),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
  # grid.text('Bilateral',x=unit(-1.1*10^9,units='native'),y=unit(7.5,units = 'native'),
  #           gp = gpar(fontsize=5,col = 'black'))
  # total_bilateral_exp <- round(sum(bilateral$total_perdose)/10^6,1)
  # grid.text(paste0(total_bilateral_exp,'M'),x=unit(7.7*10^9,units='native'),y=unit(7.5,units = 'native'),
  #           gp = gpar(fontsize=5,col = 'black'))
  grid.lines(y=c(sum(bilateral_min_end$total_perdose),sum(bilateral_max_end$total_perdose)),
             x = c(2,2),default.units = 'native',gp=gpar(col='black',lwd=1))
  grid.lines(y=c(sum(bilateral_min_end$total_perdose),sum(bilateral_min_end$total_perdose)),
             x = c(1.9,2.1),default.units = 'native',gp=gpar(col='black',lwd=1))
  grid.lines(y=c(sum(bilateral_max_end$total_perdose),sum(bilateral_max_end$total_perdose)),
             x = c(1.9,2.1),default.units = 'native',gp=gpar(col='black',lwd=1))
  
  # uniform
  grid.polygon(y = c(0,sum(uniform$total_perdose),
                     rev(c(0,sum(uniform$total_perdose)))),
               x = c(3.5,3.5,4.5,4.5),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
  grid.lines(y=c(sum(uniform_min_end$total_perdose),sum(uniform_max_end$total_perdose)),
             x = c(4,4),default.units = 'native',gp=gpar(col='black',lwd=1))
  grid.lines(y=c(sum(uniform_min_end$total_perdose),sum(uniform_min_end$total_perdose)),
             x = c(3.9,4.1),default.units = 'native',gp=gpar(col='black',lwd=1))
  grid.lines(y=c(sum(uniform_max_end$total_perdose),sum(uniform_max_end$total_perdose)),
             x = c(3.9,4.1),default.units = 'native',gp=gpar(col='black',lwd=1))
  
  
  # grid.polygon(y = c(sum(uniform$total_perdose),sum(bilateral$total_perdose),
  #                    rev(c(sum(uniform$total_perdose),sum(bilateral$total_perdose)))),
  #              x = c(5.5,5.5,6.5,6.5),
  #              default.units = 'native',
  #              gp = gpar(col = 'red', fill = 'white',alpha=0.5,lty='dashed'))
  # uniform_percent_reduction <- as.character(round((sum(uniform$total_perdose)-sum(bilateral$total_perdose))*100/
  #                                                   sum(bilateral$total_perdose),1))
  
  # grid.text(paste0(uniform_percent_reduction,'%'),x=unit(7.7*10^9,units='native'),y=unit(5.5,units = 'native'),
  #           gp = gpar(fontsize=5,col = 'red'))
  
  # grid.text('Uniform',x=unit(-1.1*10^9,units='native'),y=unit(5.5,units = 'native'),
  #           gp = gpar(fontsize=5,col = 'black'))
  
  # Adjusted GDPPC
  grid.polygon(y = c(0,sum(adj_gdppc$total_perdose),
                     rev(c(0,sum(adj_gdppc$total_perdose)))),
               x = c(5.5,5.5,6.5,6.5),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
  grid.lines(y=c(sum(adj_gdppc_min_end$total_perdose),sum(adj_gdppc_max_end$total_perdose)),
             x = c(6,6),default.units = 'native',gp=gpar(col='black',lwd=1))
  grid.lines(y=c(sum(adj_gdppc_min_end$total_perdose),sum(adj_gdppc_min_end$total_perdose)),
             x = c(5.9,6.1),default.units = 'native',gp=gpar(col='black',lwd=1))
  grid.lines(y=c(sum(adj_gdppc_max_end$total_perdose),sum(adj_gdppc_max_end$total_perdose)),
             x = c(5.9,6.1),default.units = 'native',gp=gpar(col='black',lwd=1))
  
  
  # grid.polygon(x = c(sum(adj_gdppc$total_perdose),sum(bilateral$total_perdose),
  #                    rev(c(sum(adj_gdppc$total_perdose),sum(bilateral$total_perdose)))),
  #              y = c(3,3,4,4),
  #              default.units = 'native',
  #              gp = gpar(col = 'red', fill = 'white',alpha=0.5,lty='dashed'))
  # adj_percent_reduction <- as.character(round((sum(adj_gdppc$total_perdose)-sum(bilateral$total_perdose))*100/
  #                                               sum(bilateral$total_perdose),1))
  
  # grid.text(paste0(adj_percent_reduction,'%'),x=unit(7.7*10^9,units='native'),y=unit(3.5,units = 'native'),
  #           gp = gpar(fontsize=5,col = 'red'))
  # 
  # grid.text('GDPPC-\nadjusted',x=unit(-1.3*10^9,units='native'),y=unit(3.5,units = 'native'),
  #           gp = gpar(fontsize=5,col = 'black'))
  
  # Adjusted chepc
  grid.polygon(y = c(0,sum(adj_chepc$total_perdose),
                     rev(c(0,sum(adj_chepc$total_perdose)))),
               x = c(7.5,7.5,8.5,8.5),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
  grid.lines(y=c(sum(adj_chepc_min_end$total_perdose),sum(adj_chepc_max_end$total_perdose)),
             x = c(8,8),default.units = 'native',gp=gpar(col='black',lwd=1))
  grid.lines(y=c(sum(adj_chepc_min_end$total_perdose),sum(adj_chepc_min_end$total_perdose)),
             x = c(7.9,8.1),default.units = 'native',gp=gpar(col='black',lwd=1))
  grid.lines(y=c(sum(adj_chepc_max_end$total_perdose),sum(adj_chepc_max_end$total_perdose)),
             x = c(7.9,8.1),default.units = 'native',gp=gpar(col='black',lwd=1))
  
  
  # grid.polygon(x = c(sum(adj_chepc$total_perdose),sum(bilateral$total_perdose),
  #                    rev(c(sum(adj_chepc$total_perdose),sum(bilateral$total_perdose)))),
  #              y = c(1,1,2,2),
  #              default.units = 'native',
  #              gp = gpar(col = 'red', fill = 'white',alpha=0.5,lty='dashed'))
  # adj_chepc_percent_reduction <- as.character(round((sum(adj_chepc$total_perdose)-sum(bilateral$total_perdose))*100/
  #                                                       sum(bilateral$total_perdose),1))
  # 
  # grid.text(paste0(adj_chepc_percent_reduction,'%'),x=unit(7.7*10^9,units='native'),y=unit(1.5,units = 'native'),
  #           gp = gpar(fontsize=5,col = 'red'))
  
  # grid.text('CHEPC-\nadjusted',x=unit(-1.3*10^9,units='native'),y=unit(1.5,units = 'native'),
  #           gp = gpar(fontsize=5,col = 'black'))
  
  popViewport()
}