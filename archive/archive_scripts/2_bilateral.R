library(grid)

bilateral_affordable = dataset[1:10,]$`Bilateral mechanisms*`*2*100/dataset[1:10,]$`GDP per capita (2022)`

png(paste0("output/plots/plot_price_gdp_bilateral_affordability.png"),height=8,width=8,units='cm',res=700,pointsize=10)
if(1)
{
  pushViewport(plotViewport(c(4,4,1,2),xscale=c(2,6),yscale=c(6,10)))#)NEW
  grid.rect()

  grid.yaxis(at=seq(6,10), 
            label=format(c(1,10,100,1000,10000), big.mark = ",", scientific = FALSE),gp=gpar(fontsize=8))
  grid.xaxis(at=seq(2,6), 
            label=format(c(100,1000,10000,100000,1000000), big.mark=",", scientific=FALSE),gp=gpar(fontsize=8))
  
  #grid.yaxis()
  #grid.xaxis()
   
  grid.text(paste0('GDP per capita in 2022 (current US$)'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  grid.text(paste0('Total cost per dose (in millions)'),x = unit(0,'npc')+unit(-3.6,'lines'),rot=90,gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  
  cex_size = dataset[1:10,7]/7.5
  for (i in (1:10)){
    if (dataset[i,7]==15.00){
      colour = "#ED2939"
    }
    if (dataset[i,7]==10.00){
      colour = "#F9E300"
    }
    if (dataset[i,7]==9.95){
      colour = "#0042A6"
    }
    grid.points(x=log(x=dataset[i,5],base=10),
                y=log(x=dataset[i,8],base=10),
                default.units = 'native',pch=16,gp=gpar(alpha=0.5,lwd=0.8, col=colour,
                                                        cex=bilateral_affordable[i]*4))
  }
  
  # grid.points(x=log(x=dataset[1:10,5],base=10),
  #             y=log(x=dataset[1:10,8],base=10),
  #             default.units = 'native',pch=16,gp=gpar(alpha=0.5,lwd=0.8, col="#0047AB"))
  
  legend_box <- rectGrob(x = 0.17, y = 0.16, width = 0.3, height = 0.2,
                         gp = gpar(col = "#A9A9A9", alpha=0.8, lty = 1, lwd=0.8))
  
  vax_1_box <- rectGrob(x = 0.09, y = 0.19, width = 0.025, height = 0.0125,
                        gp = gpar(col = "#0042A6", fill = "#0042A6", lty = 1, lwd=0.8))
  vax_2_box <- rectGrob(x = 0.09, y = 0.14, width = 0.025, height = 0.0125,
                        gp = gpar(col = "#F9E300", fill = "#F9E300", lty = 1, lwd=0.8))
  vax_3_box <- rectGrob(x = 0.09, y = 0.09, width = 0.025, height = 0.0125,
                        gp = gpar(col = "#ED2939", fill = "#ED2939", lty = 1, lwd=0.8))
  
  grid.draw(legend_box)
  grid.draw(vax_1_box)
  grid.draw(vax_2_box)
  grid.draw(vax_3_box)
  
  grid.text("Price per dose", x=0.17,y=0.24,gp=gpar(fontsize=6, fontface='bold'))
  grid.text(" $9.95", x=0.2,y=0.19,gp=gpar(fontsize=6))
  grid.text("$10.00", x=0.2,y=0.14,gp=gpar(fontsize=6))
  grid.text("$15.00", x=0.2,y=0.09,gp=gpar(fontsize=6))
  
  
  popViewport()
}
dev.off()



png(paste0("output/plots/plot_population_price_bilateral.png"),height=8,width=8,units='cm',res=700,pointsize=10)
if(1)
{
  
  pushViewport(plotViewport(c(4,4,1,2),xscale=c(5,9),yscale=c(6,10)))#)NEW
  grid.rect()
  
  grid.yaxis(at=seq(6,10), 
             label=format(c(1,10,100,1000,10000), big.mark = ",", scientific = FALSE),gp=gpar(fontsize=8))
  grid.xaxis(at=seq(5,9), 
             label=format(c(0.1,1,10,100,1000),big.mark = ",", scientific = FALSE),gp=gpar(fontsize=8))
  
  #grid.yaxis()
  #grid.xaxis()
  
  grid.text(paste0('Population (2022) (in millions)'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  grid.text(paste0('Total cost per dose (in millions)'),x = unit(0,'npc')+unit(-3.6,'lines'),rot=90,gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  
  grid.points(x=log(x=dataset[1:10,2],base=10),
              y=log(x=dataset[1:10,8],base=10),
              default.units = 'native',pch=16,gp=gpar(alpha=0.5,lwd=0.8, col="#0047AB"))
  
  popViewport()
}
dev.off()

