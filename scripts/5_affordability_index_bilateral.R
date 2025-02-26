#bilateral <- bilateral[order(-dataset$`GDP per capita (2022)`),]
library(grid)

png(paste0("output/plots/plot_unaffordability_index_bilateral.png"),height=8,width=8,units='cm',res=700,pointsize=10)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(4,5,1,1),
                            xscale=c(min(log(x=dataset[1:10,4],base=10)),
                                     max(log(x=dataset[1:10,4],base=10))),
                            yscale=c(0,1)))#)NEW
  grid.rect()
  
  
  grid.yaxis(at=c(0,0.2,0.4,0.6,0.8,1),label = c("0%","20%","40%","60%","80%","100%"),gp=gpar(fontsize=8))
  grid.xaxis(at=c(3.5,4,4.5), 
             label=c(expression(paste(10^"3.5")),"10,000",expression(paste(10^"4.5"))),gp=gpar(fontsize=8))

  grid.text(paste0('GDP per capita in 2022 (current US$)'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  grid.text(paste0('Price of 1 dose\nas % of GDP per capita'),x = unit(0,'npc')+unit(-4,'lines'),rot=90,gp = gpar(fontsize=9,fontface='bold',col = 'black'))

  vax_0_box <- rectGrob(x = 0.05, y = 0.97, width = 0.025, height = 0.0125,
                        gp = gpar(col = "grey", fill = "grey", lty = 1, lwd=0.8))

  grid.draw(vax_0_box)

  grid.text("     Bilateral pricing", x=0.19,y=0.97,gp=gpar(fontsize=6))

  x_curve <- spline_gdppc_percent$x
  y_curve<- spline_gdppc_percent$y
  
  x_line <- seq(min(log(x=dataset[1:10,4],base=10)),
                max(log(x=dataset[1:10,4],base=10)),
                length.out=10000)
  y_line <- seq(0,1,length.out=10000)
  
  index <- as.integer(median(c(which.min(abs(x_line-x_curve[4])),which.min(abs(x_line-x_curve[5])))))
  
  x_val <- x_line[index]
  y_val <- y_line[index]
  
  # grid.points(log(x=dataset[1:10,4],base=10),
  #             bilateral$perdose_gdppc_percent,default.units = 'native',
  #             pch=16,gp=gpar(cex = 0.6,lwd=1, col="black"))
  
  grid.polygon(x=c(x_curve[x_curve<x_val],x_val,
                   rev(x_line[x_line<x_val])),
               y = c(y_curve[x_curve<x_val],y_val,
                     rev(y_line[y_line<y_val])),
               default.units = 'native',gp=gpar(col=NA,fill="grey"))
  
  grid.polygon(x=c((x_line[x_line>x_val]),rev(c(x_val,x_curve[x_curve>x_val]))),
               y = c((y_line[y_line>y_val]),rev(c(y_val,y_curve[x_curve>x_val]))),
               default.units = 'native',gp=gpar(col=NA,fill="grey"))
  
  
  grid.lines(x=seq(min(log(x=dataset[1:10,4],base=10)),
                   max(log(x=dataset[1:10,4],base=10)),
                   length.out=100),
             y=seq(0,1,length.out=100),default.units = 'native',gp=gpar(col='black',lty='dashed'))
  
  grid.rect(gp=gpar(fill=NA))
  
  
  popViewport()
}
dev.off()