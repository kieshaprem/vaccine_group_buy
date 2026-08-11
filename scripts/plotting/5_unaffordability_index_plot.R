#bilateral <- bilateral[order(-dataset$`GDP per capita (2022)`),]
library(grid)
library(econocharts)
#png(paste0("output/plots/plot_unaffordability_index_bilateral.png"),height=8,width=8,units='cm',res=700,pointsize=10)
unaffordability_plot <- function(mechanism, spline_df_bilateral, 
                                 spline_df_main, CI_val, CI_val_bilateral,
                                 affordability_metric){
  if (affordability_metric=='perdose_gdppc_percent'){
    wealth_measure <- 'gdppc'
    affordability_metric_name <- 'GDP per capita'
    x_axis_loc <- c(3.5,4,4.5)
    x_axis_labels <- c(expression(paste(10^"3.5")),expression(paste(10^"4.0")),
                       expression(paste(10^"4.5")))
    y_axis_loc <- c(0,0.2,0.4,0.6,0.8,1)
    y_axis_labels <- c("0.0%","0.2%","0.4%","0.6%","0.8%","1.0%")
    # chepc_spline_min <- spline_chepc_percent_gdppc_min
    # chepc_spline_max <- spline_chepc_percent_gdppc_max
    # gdppc_spline_min <- spline_gdppc_percent_gdppc_min
    # gdppc_spline_max <- spline_gdppc_percent_gdppc_max
  }
  
  if (affordability_metric=='perdose_chepc_percent'){
    wealth_measure <- 'chepc'
    affordability_metric_name <- 'CHE per capita'
    x_axis_loc <- c(2,3)
    x_axis_labels <- c(expression(paste(10^"2")),
                       expression(paste(10^"3")))
    y_axis_loc <- c(0,10,20,30,40,50)
    y_axis_labels <- c("0%","10%","20%","30%","40%","50%")
    # chepc_spline_min <- spline_chepc_percent_chepc_min
    # chepc_spline_max <- spline_chepc_percent_chepc_max
    # gdppc_spline_min <- spline_gdppc_percent_chepc_min
    # gdppc_spline_max <- spline_gdppc_percent_chepc_max
  }
  
  pushViewport(plotViewport(c(2,1,1,3),
                            xscale=c(min(log(x=bilateral[,wealth_measure],base=10)),
                                     max(log(x=bilateral[,wealth_measure],base=10))),
                            yscale=c(min(y_axis_loc),max(y_axis_loc))))
  
  if (mechanism=="bilateral"){
       # grid.text(paste0('Price of 1 dose\nas % of ',affordability_metric_name),x = unit(0,'npc')+unit(-4.2,'lines'),rot=90,
    #           gp = gpar(fontsize=6,fontface='bold',col = 'black'))
    if (affordability_metric=='perdose_gdppc_percent'){
      text <- 'e)'
    }
    if (affordability_metric=='perdose_chepc_percent'){
      text <- 'm)'
    }
    grid.text(text,x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(12,'lines'),gp = gpar(fontsize=6,fontface='bold'))
    #   } else {
    # pushViewport(plotViewport(c(4,3,1,1),
    #                           xscale=c(min(log(x=bilateral[,wealth_measure],base=10)),
    #                                    max(log(x=bilateral[,wealth_measure],base=10))),
    #                           yscale=c(min(y_axis_loc),max(y_axis_loc))))
  }
grid.xaxis(at=x_axis_loc,
           label=x_axis_labels,
           gp=gpar(fontsize=5))
  grid.yaxis(at=y_axis_loc,label = y_axis_labels,
             gp=gpar(fontsize=5),main = F)
   
  # grid.text(paste0(affordability_metric_name,
  #                  ' in 2023, log10 scale'),y = unit(0,'npc')+unit(-2.5,'lines'),
  #           gp = gpar(fontsize=6,fontface='bold',col = 'black'))

  x_curve_bilateral <- spline_df_bilateral$x
  y_curve_bilateral<- spline_df_bilateral$y
  x_curve_main <- spline_df_main$x
  y_curve_main<- spline_df_main$y
  if (mechanism=="uniform"){
    if (affordability_metric=='perdose_gdppc_percent'){
      text <- 'f)'
    }
    if (affordability_metric=='perdose_chepc_percent'){
      text <- 'n)'
    }
    grid.text(paste0("Uniform pricing (CI = ",CI_val,")"), x=0.41,y=0.90,gp=gpar(fontsize=4))
    color <- "blue"
    grid.text(text,x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(12,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  
  if (mechanism=="gdp_adj"){
    if (affordability_metric=='perdose_gdppc_percent'){
      text <- 'g)'
    }
    if (affordability_metric=='perdose_chepc_percent'){
      text <- 'o)'
    }
    grid.text(paste0("GDPPC-adjusted, \n(CI = ",CI_val,")       "),x=0.30,y=0.86,gp=gpar(fontsize=4))
    grid.text(as.expression(bquote(alpha == 10)), x=0.63,y=0.9,gp=gpar(fontsize=4))
    color <- "orange"
    grid.text(text,x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(12,'lines'),gp = gpar(fontsize=6,fontface='bold'))
    }
  if (mechanism=="chepc_adj"){
    if (affordability_metric=='perdose_gdppc_percent'){
      text <- 'h)'
    }
    if (affordability_metric=='perdose_chepc_percent'){
      text <- 'p)'
    }
    grid.text(paste0("CHEPC-adjusted, \n(CI = ",CI_val,")       "),x=0.30,y=0.86,gp=gpar(fontsize=4))
    grid.text(as.expression(bquote(alpha == 10)), x=0.62,y=0.9,gp=gpar(fontsize=4))
    color <- "green"
    grid.text(text,x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(12,'lines'),gp = gpar(fontsize=6,fontface='bold'))
   }
  
  # legend
  vax_0_box <- rectGrob(x = 0.05, y = 0.965, width = 0.025, height = 0.0125,
                        gp = gpar(col = "grey50", fill = "grey50", alpha=0.5, lty = 1, lwd=0.8))
  grid.draw(vax_0_box)
  grid.text(paste0("Bilateral pricing (CI = ",CI_val_bilateral,")"), x=0.41,y=0.96,gp=gpar(fontsize=4))

  x_line <- seq(min(log(x=bilateral[,wealth_measure],base=10)),
                max(log(x=bilateral[,wealth_measure],base=10)),
                length.out=1000)
  y_line <- seq(min(y_axis_loc),max(y_axis_loc),length.out=1000)
  
  diagonal_line <- data.frame(x=x_line,y=y_line)
  
  
  intersect_val <- curve_intersect(diagonal_line,
                                   spline_df_main,empirical = T)
  
  x_val <- intersect_val$x
  y_val <- intersect_val$y
  
  if (mechanism!="bilateral"){
  #grid.points(x_curve_main, y_curve_main)
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
  
  intersect_val_bilateral <- curve_intersect(diagonal_line,
                                   spline_df_bilateral,empirical = T)
  x_val_bilateral <- intersect_val_bilateral$x
  y_val_bilateral <- intersect_val_bilateral$y
  
  grid.polygon(x=c(x_curve_bilateral[x_curve_bilateral<x_val_bilateral],x_val_bilateral,
                   rev(x_line[x_line<x_val_bilateral])),
               y = c(y_curve_bilateral[x_curve_bilateral<x_val_bilateral],y_val_bilateral,
                     rev(y_line[y_line<y_val_bilateral])),
               default.units = 'native',gp=gpar(col=NA,fill="grey50",alpha=0.5))
  
  grid.polygon(x=c((x_line[x_line>x_val_bilateral]),rev(c(x_val_bilateral,
                                                          x_curve_bilateral[x_curve_bilateral>x_val_bilateral]))),
               y = c((y_line[y_line>y_val_bilateral]),rev(c(y_val_bilateral,
                                                            y_curve_bilateral[x_curve_bilateral>x_val_bilateral]))),
               default.units = 'native',gp=gpar(col=NA,fill="grey50",alpha=0.5))
  
  grid.lines(x=x_line,
             y=y_line,default.units = 'native',gp=gpar(col='black',lty='dashed'))
  
  grid.rect(gp=gpar(fill=NA))
  
  popViewport()
}