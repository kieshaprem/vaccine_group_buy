# ------------------------------------------------------------------------------
# Function to create plot 3e-h (GDP per capita perspective) and 3i-l (CHE per 
# capita perspective) in the Main Paper
# ------------------------------------------------------------------------------

library(grid)
library(econocharts)

unaffordability_plot <- function(mechanism, spline_df_bilateral, 
                                 spline_df_main, CI_val, CI_val_bilateral,
                                 affordability_metric){
  if (affordability_metric=='perdose_gdppc_percent'){
    wealth_measure <- 'gdppc'
    affordability_metric_name <- 'GDP per capita'
    x_axis_loc <- c(3.5,4,4.5)
    x_axis_labels <- c(expression(paste(10^"3.5")),
                       expression(paste(10^"4.0")),
                       expression(paste(10^"4.5")))
    y_axis_loc <- c(0,0.2,0.4,0.6,0.8,1)
    y_axis_labels <- c("0.0%","0.2%","0.4%","0.6%","0.8%","1.0%")
    }
  
  if (affordability_metric=='perdose_chepc_percent'){
    wealth_measure <- 'chepc'
    affordability_metric_name <- 'CHE per capita'
    x_axis_loc <- c(2,3)
    x_axis_labels <- c(expression(paste(10^"2")),
                       expression(paste(10^"3")))
    y_axis_loc <- c(0,10,20,30,40,50)
    y_axis_labels <- c("0%","10%","20%","30%","40%","50%")
  }
  
  pushViewport(plotViewport(c(2,1,1,3),
              xscale=c(min(log(x=bilateral[,wealth_measure],base=10)),
              max(log(x=bilateral[,wealth_measure],base=10))),
              yscale=c(min(y_axis_loc),max(y_axis_loc))))
  
  if (mechanism=="bilateral"){
    if (affordability_metric=='perdose_gdppc_percent'){
      text <- 'e)'
    }
    if (affordability_metric=='perdose_chepc_percent'){
      text <- 'm)'
    }
    grid.text(text,x = unit(0,'npc')+unit(0.1,'lines'),
              y = unit(0,'npc')+unit(12,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
  }
  grid.xaxis(at=x_axis_loc,
           label=x_axis_labels,
           gp=gpar(fontsize=5))
  grid.yaxis(at=y_axis_loc,
             label = y_axis_labels,
             gp=gpar(fontsize=5),main = F)
   
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
    grid.text(paste0("Uniform pricing (CI = ",CI_val,")"), x=0.41,y=0.90,
              gp=gpar(fontsize=4))
    color <- "blue"
    grid.text(text,x = unit(0,'npc')+unit(0.1,'lines'),
              y = unit(0,'npc')+unit(12,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
  }
  
  if (mechanism=="gdp_adj"){
    if (affordability_metric=='perdose_gdppc_percent'){
      text <- 'g)'
    }
    if (affordability_metric=='perdose_chepc_percent'){
      text <- 'o)'
    }
    grid.text(paste0("GDPPC-adjusted, \n(CI = ",CI_val,")       "),
              x=0.30,y=0.86,gp=gpar(fontsize=4))
    grid.text(as.expression(bquote(alpha == 10)), x=0.63,y=0.9,
              gp=gpar(fontsize=4))
    color <- "orange"
    grid.text(text,x = unit(0,'npc')+unit(0.1,'lines'),
              y = unit(0,'npc')+unit(12,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
    }
  if (mechanism=="chepc_adj"){
    if (affordability_metric=='perdose_gdppc_percent'){
      text <- 'h)'
    }
    if (affordability_metric=='perdose_chepc_percent'){
      text <- 'p)'
    }
    grid.text(paste0("CHEPC-adjusted, \n(CI = ",CI_val,")       "),
              x=0.30,y=0.86,gp=gpar(fontsize=4))
    grid.text(as.expression(bquote(alpha == 10)), x=0.62,y=0.9,
              gp=gpar(fontsize=4))
    color <- "green"
    grid.text(text,x = unit(0,'npc')+unit(0.1,'lines'),
              y = unit(0,'npc')+unit(12,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
   }
  
  # legend
  leg_0_box <- rectGrob(x = 0.05, y = 0.965, width = 0.025, height = 0.0125,
                        gp = gpar(col = "grey50", fill = "grey50", 
                        alpha=0.5, lty = 1, lwd=0.8))
  grid.draw(leg_0_box)
  grid.text(paste0("Bilateral pricing (CI = ",CI_val_bilateral,")"), 
            x=0.41,y=0.96,gp=gpar(fontsize=4))
  
  # line of equality
  x_line <- seq(min(log(x=bilateral[,wealth_measure],base=10)),
                max(log(x=bilateral[,wealth_measure],base=10)),
                length.out=1000)
  y_line <- seq(min(y_axis_loc),max(y_axis_loc),length.out=1000)
  diagonal_line <- data.frame(x=x_line,y=y_line)
  
  # find where line of equality intersects with the concentration curve 
  intersect_val <- curve_intersect(diagonal_line,
                                    spline_df_main,empirical = T)
  x_val <- intersect_val$x
  y_val <- intersect_val$y
   
  if (mechanism!="bilateral"){
    # If not bilateral mechanism, draw the other mechanism's concentration curve
    
    # Draw section where concentration curve is above the line of equality
    grid.polygon(x=c(x_curve_main[x_curve_main<x_val],x_val,
                     rev(x_line[x_line<x_val])),
                 y = c(y_curve_main[x_curve_main<x_val],y_val,
                       rev(y_line[y_line<y_val])),
                 default.units = 'native',gp=gpar(col=NA,fill=color,alpha=0.5))
    
    # Draw section where concentration curve is lower than the line of equality
    grid.polygon(x=c((x_line[x_line>x_val]),
                     rev(c(x_val,x_curve_main[x_curve_main>x_val]))),
                 y = c((y_line[y_line>y_val]),
                       rev(c(y_val,y_curve_main[x_curve_main>x_val]))),
                 default.units = 'native',gp=gpar(col=NA,fill=color,alpha=0.5))
    leg_1_box <- rectGrob(x = 0.05, y = 0.905, width = 0.025, height = 0.0125,
                          gp = gpar(col = color, fill = color,alpha=0.5,
                                    lty = 1,lwd=0.8))
    grid.draw(leg_1_box)
  }
  
  # find where line of equality intersects with the bilateral concentration curve 
  intersect_val_bilateral <- curve_intersect(diagonal_line,
                                   spline_df_bilateral,empirical = T)
  x_val_bilateral <- intersect_val_bilateral$x
  y_val_bilateral <- intersect_val_bilateral$y
  
  # Draw bilateral mechanism's concentration curve
  
  # Draw section where concentration curve is above the line of equality
  grid.polygon(x=c(x_curve_bilateral[x_curve_bilateral<x_val_bilateral],
                   x_val_bilateral,
                   rev(x_line[x_line<x_val_bilateral])),
               y = c(y_curve_bilateral[x_curve_bilateral<x_val_bilateral],
                     y_val_bilateral,
                     rev(y_line[y_line<y_val_bilateral])),
               default.units = 'native',gp=gpar(col=NA,fill="grey50",alpha=0.5))
  
  # Draw section where concentration curve is lower than the line of equality
   grid.polygon(x=c((x_line[x_line>x_val_bilateral]),rev(c(x_val_bilateral,
                    x_curve_bilateral[x_curve_bilateral>x_val_bilateral]))),
               y = c((y_line[y_line>y_val_bilateral]),rev(c(y_val_bilateral,
                    y_curve_bilateral[x_curve_bilateral>x_val_bilateral]))),
               default.units = 'native',gp=gpar(col=NA,fill="grey50",alpha=0.5))
   
  # Draw the line of equality
  grid.lines(x=x_line,
             y=y_line,default.units = 'native',gp=gpar(col='black',lty='dashed'))
  
  grid.rect(gp=gpar(fill=NA))
  
  popViewport()
}