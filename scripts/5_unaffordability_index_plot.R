#bilateral <- bilateral[order(-dataset$`GDP per capita (2022)`),]
library(grid)

#png(paste0("output/plots/plot_unaffordability_index_bilateral.png"),height=8,width=8,units='cm',res=700,pointsize=10)
unaffordability_plot <- function(mechanism){
  pushViewport(plotViewport(c(1,3,1,1),
                            xscale=c(min(log(x=dataset[1:10,4],base=10)),
                                     max(log(x=dataset[1:10,4],base=10))),
                            yscale=c(0,1)))
  grid.rect()
  if (mechanism=="bilateral"){
    grid.text(paste0('Price of 1 dose\nas % of GDP per capita'),x = unit(0,'npc')+unit(-4.2,'lines'),rot=90,
              gp = gpar(fontsize=6,fontface='bold',col = 'black'))
    grid.text('d)',x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(15.5,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  grid.yaxis(at=c(0,0.2,0.4,0.6,0.8,1),label = c("0.0%","0.2%","0.4%","0.6%","0.8%","1.0%"),
             gp=gpar(fontsize=5))
  grid.xaxis(at=c(3.5,4,4.5), 
             label=c(expression(paste(10^"3.5")),expression(paste(10^"4.0")),
                     expression(paste(10^"4.5"))),gp=gpar(fontsize=5))
  
  grid.text(paste0('GDP per capita in 2022, log10 scale'),y = unit(0,'npc')+unit(-2.5,'lines'),
            gp = gpar(fontsize=6,fontface='bold',col = 'black'))

  x_curve_bilateral <- spline_gdppc_percent_bilateral$x
  y_curve_bilateral<- spline_gdppc_percent_bilateral$y
  
  if (mechanism=="uniform"){
    mechanism_name <- "Uniform Pricing"
    grid.text("Uniform pricing (WCI = -0.613)", x=0.41,y=0.91,gp=gpar(fontsize=4))
    color <- "blue"
    x_curve_main <- spline_gdppc_percent_uniform$x
    y_curve_main<- spline_gdppc_percent_uniform$y
    grid.text('e)',x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(15.5,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  
  if (mechanism=="gdp_adj"){
    mechanism_name <- "Weighted by GDP per capita"
    grid.text("Weighted by GDP per capita, \n(WCI = -0.404)                       ", x=0.40,y=0.88,gp=gpar(fontsize=4))
    grid.text(as.expression(bquote(alpha == 10)), x=0.77,y=0.905,gp=gpar(fontsize=4))
    color <- "orange"
    x_curve_main <- spline_gdppc_percent_gdp_adj$x
    y_curve_main<- spline_gdppc_percent_gdp_adj$y
    grid.text('f)',x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(15.5,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  
  # legend
  vax_0_box <- rectGrob(x = 0.05, y = 0.965, width = 0.025, height = 0.0125,
                        gp = gpar(col = "grey50", fill = "grey50", alpha=0.5, lty = 1, lwd=0.8))
  grid.draw(vax_0_box)
  grid.text("Bilateral pricing (WCI = -0.664)", x=0.41,y=0.97,gp=gpar(fontsize=4))

  x_line <- seq(min(log(x=dataset[1:10,4],base=10)),
                max(log(x=dataset[1:10,4],base=10)),
                length.out=10000)
  y_line <- seq(0,1,length.out=10000)
  
  index <- as.integer(median(c(which.min(abs(x_line-spline_gdppc_percent_bilateral$x[257])),which.min(abs(x_line-spline_gdppc_percent_bilateral$x[298])))))
  
  x_val <- x_line[index]
  y_val <- y_line[index]
  
  if (mechanism!="bilateral"){
  grid.polygon(x=c(x_curve_main[x_curve_main<x_val],x_val,
                   rev(x_line[x_line<x_val])),
               y = c(y_curve_main[x_curve_main<x_val],y_val,
                     rev(y_line[y_line<y_val])),
               default.units = 'native',gp=gpar(col=NA,fill=color,alpha=0.5))
  
  grid.polygon(x=c((x_line[x_line>x_val]),rev(c(x_val,x_curve_main[x_curve_main>x_val]))),
               y = c((y_line[y_line>y_val]),rev(c(y_val,y_curve_main[x_curve_main>x_val]))),
               default.units = 'native',gp=gpar(col=NA,fill=color,alpha=0.5))
  vax_1_box <- rectGrob(x = 0.05, y = 0.905, width = 0.025, height = 0.0125,
                        gp = gpar(col = color, fill = color,alpha=0.5, lty = 1, lwd=0.8))
  grid.draw(vax_1_box)
  }
  
  grid.polygon(x=c(x_curve_bilateral[x_curve_bilateral<x_val],x_val,
                   rev(x_line[x_line<x_val])),
               y = c(y_curve_bilateral[x_curve_bilateral<x_val],y_val,
                     rev(y_line[y_line<y_val])),
               default.units = 'native',gp=gpar(col=NA,fill="grey50",alpha=0.5))
  
  grid.polygon(x=c((x_line[x_line>x_val]),rev(c(x_val,x_curve_bilateral[x_curve_bilateral>x_val]))),
               y = c((y_line[y_line>y_val]),rev(c(y_val,y_curve_bilateral[x_curve_bilateral>x_val]))),
               default.units = 'native',gp=gpar(col=NA,fill="grey50",alpha=0.5))
  
  
  grid.lines(x=seq(min(log(x=dataset[1:10,4],base=10)),
                   max(log(x=dataset[1:10,4],base=10)),
                   length.out=100),
             y=seq(0,1,length.out=100),default.units = 'native',gp=gpar(col='black',lty='dashed'))
  
  grid.rect(gp=gpar(fill=NA))
  
  popViewport()
}