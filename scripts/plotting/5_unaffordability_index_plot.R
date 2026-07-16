#bilateral <- bilateral[order(-dataset$`GDP per capita (2022)`),]
library(grid)
library(econocharts)
#png(paste0("output/plots/plot_unaffordability_index_bilateral.png"),height=8,width=8,units='cm',res=700,pointsize=10)
unaffordability_plot <- function(mechanism, spline_df_bilateral, 
                                 spline_df_main, WCI_val, WCI_val_bilateral,
                                 affordability_metric){
  if (affordability_metric=='perdose_gdppc_percent'){
    wealth_measure <- 'gdp_percapita'
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
  if (affordability_metric=='perdose_gov_exp_percent'){
    wealth_measure <- 'gov_exp'
    affordability_metric_name <- 'GHE per capita'
    x_axis_loc <- c(1,2,3)
    x_axis_labels <- c(expression(paste(10^"1")),expression(paste(10^"2")),
                       expression(paste(10^"3")))
    y_axis_loc <- c(0,40,80,120,160)
    y_axis_labels <- c("0%","40%","80%","120%","160%")
    # gov_exp_spline_min <- spline_gov_exp_percent_gov_exp_min
    # gov_exp_spline_max <- spline_gov_exp_percent_gov_exp_max
    # gdppc_spline_min <- spline_gdppc_percent_gov_exp_min
    # gdppc_spline_max <- spline_gdppc_percent_gov_exp_max
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
  
  pushViewport(plotViewport(c(1,3,1,1),
                            xscale=c(min(log(x=bilateral_full[,wealth_measure],base=10)),
                                     max(log(x=bilateral_full[,wealth_measure],base=10))),
                            yscale=c(min(y_axis_loc),max(y_axis_loc))))
  grid.rect()
  
  if (mechanism=="bilateral"){
    grid.text(paste0('Price of 1 dose\nas % of ',affordability_metric_name),x = unit(0,'npc')+unit(-4.2,'lines'),rot=90,
              gp = gpar(fontsize=6,fontface='bold',col = 'black'))
    grid.text('e)',x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(15.5,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  grid.xaxis(at=x_axis_loc, 
             label=x_axis_labels,
             gp=gpar(fontsize=5))
  grid.yaxis(at=y_axis_loc,label = y_axis_labels,
             gp=gpar(fontsize=5))
   
  grid.text(paste0(affordability_metric_name,
                   ' in 2023, log10 scale'),y = unit(0,'npc')+unit(-2.5,'lines'),
            gp = gpar(fontsize=6,fontface='bold',col = 'black'))

  x_curve_bilateral <- spline_df_bilateral$x
  y_curve_bilateral<- spline_df_bilateral$y
  x_curve_main <- spline_df_main$x
  y_curve_main<- spline_df_main$y
  if (mechanism=="uniform"){
    grid.text(paste0("Uniform pricing (WCI = ",WCI_val,")"), x=0.39,y=0.90,gp=gpar(fontsize=4))
    color <- "blue"
    grid.text('f)',x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(15.5,'lines'),gp = gpar(fontsize=6,fontface='bold'))
  }
  
  if (mechanism=="gdp_adj"){
    print('lagi gdp_dj')
    grid.text(paste0("GDPPC-adjusted, \n(WCI = ",WCI_val,")       "),x=0.27,y=0.88,gp=gpar(fontsize=4))
    grid.text(as.expression(bquote(alpha == 10)), x=0.52,y=0.905,gp=gpar(fontsize=4))
    color <- "orange"
    grid.text('g)',x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(15.5,'lines'),gp = gpar(fontsize=6,fontface='bold'))
    # grid.lines(x=gdppc_spline_min$x,
    #            y=gdppc_spline_min$y,
    #            default.units = 'native',
    #            gp=gpar(col='#FF8C00',lty='dashed'))
    # grid.lines(x=gdppc_spline_max$x,
    #            y=gdppc_spline_max$y,
    #            default.units = 'native',
    #            gp=gpar(col='#FF8C00',lty='dashed'))
  }
  if (mechanism=="chepc_adj"){
    grid.text(paste0("CHEPC-adjusted, \n(WCI = ",WCI_val,")       "),x=0.27,y=0.88,gp=gpar(fontsize=4))
    grid.text(as.expression(bquote(alpha == 10)), x=0.52,y=0.905,gp=gpar(fontsize=4))
    color <- "green"
    grid.text('h)',x = unit(0,'npc')+unit(0.1,'lines'),y = unit(0,'npc')+unit(15.5,'lines'),gp = gpar(fontsize=6,fontface='bold'))
    # grid.lines(x=gov_exp_spline_min$x,
    #            y=gov_exp_spline_min$y,
    #            default.units = 'native',
    #            gp=gpar(col='#013220',lty='dashed'))
    # grid.lines(x=gov_exp_spline_max$x,
    #            y=gov_exp_spline_max$y,
    #            default.units = 'native',
    #            gp=gpar(col='#013220',lty='dashed'))
  }
  
  # legend
  vax_0_box <- rectGrob(x = 0.05, y = 0.965, width = 0.025, height = 0.0125,
                        gp = gpar(col = "grey50", fill = "grey50", alpha=0.5, lty = 1, lwd=0.8))
  grid.draw(vax_0_box)
  grid.text(paste0("Bilateral pricing (WCI = ",WCI_val_bilateral,")"), x=0.4,y=0.96,gp=gpar(fontsize=4))

  x_line <- seq(min(log(x=bilateral_full[,wealth_measure],base=10)),
                max(log(x=bilateral_full[,wealth_measure],base=10)),
                length.out=1000)
  y_line <- seq(min(y_axis_loc),max(y_axis_loc),length.out=1000)
  
  diagonal_line <- data.frame(x=x_line,y=y_line)
  
  
  intersect_val <- curve_intersect(diagonal_line,
                                   spline_gdppc_percent_chepc_adj,empirical = T)
  
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
  
  grid.polygon(x=c(x_curve_bilateral[x_curve_bilateral<x_val],x_val,
                   rev(x_line[x_line<x_val])),
               y = c(y_curve_bilateral[x_curve_bilateral<x_val],y_val,
                     rev(y_line[y_line<y_val])),
               default.units = 'native',gp=gpar(col=NA,fill="grey50",alpha=0.5))
  
  grid.polygon(x=c((x_line[x_line>x_val]),rev(c(x_val,x_curve_bilateral[x_curve_bilateral>x_val]))),
               y = c((y_line[y_line>y_val]),rev(c(y_val,y_curve_bilateral[x_curve_bilateral>x_val]))),
               default.units = 'native',gp=gpar(col=NA,fill="grey50",alpha=0.5))
  
  grid.lines(x=x_line,
             y=y_line,default.units = 'native',gp=gpar(col='black',lty='dashed'))
  
  grid.rect(gp=gpar(fill=NA))
  
  popViewport()
}