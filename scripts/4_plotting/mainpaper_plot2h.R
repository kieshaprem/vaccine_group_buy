library(grid)
source('scripts/4_plotting/helper_plotting.R')
# ------------------------------------------------------------------------------
# Function to create plot 2h in the Main Paper
# ------------------------------------------------------------------------------

# inputted df must have 3 columns (lower, median, upper) and 4 rows named with
# each pricing mechanism
get_exp_across <- function(df){
  order_magnitude <- order_of_magnitude(max(df$upper))
  val_scale <- c(-1,ceiling(max(df$upper)/order_magnitude)*order_magnitude)
  
  pushViewport(plotViewport(c(1,2,2,1),xscale=c(0.5,4.5),yscale=val_scale))
  grid.rect()
  y_axis_locations <- axisTicks(val_scale,log=FALSE)
  y_axis_labels <- sapply(y_axis_locations,
                          function(x){get_formatted_number(x)})
  
  # Axes
  grid.yaxis(at = y_axis_locations, label = y_axis_labels, 
             gp=gpar(fontsize=6,col='black'))
  grid.xaxis(at=seq(1,4), 
             label=rownames(df),gp=gpar(fontsize=6))
  # Median points
  grid.points(seq(1,4), df$median, default.units = "native",
              gp=gpar(col="grey20", cex=0.45),pch=16)
  # Lines for 95% UI
  for (i in 1:nrow(df)){
    grid.lines(x=c(i,i),y=c(df[i,]$lower,df[i,]$upper),
               gp=gpar(col="grey20",lwd=0.8),
               default.units = 'native')
  }
  
  # Find the lowest total expenditure value across all the 4 mechanisms
  marker <- min(sum(bilateral$total_perdose),sum(uniform$total_perdose),
                sum(adj_gdppc$total_perdose),sum(adj_chepc$total_perdose))
  grid.lines(x=c(0.5,4.5),y = c(marker,marker), default.units = 'native',
             gp=gpar(lwd=1,col='grey20',lty=2))
  
  # Axis labels and title
  grid.text("Total Expenditure of SEA",x=-0.25,default.units = 'native',
            rot =90, gp = gpar(fontsize=8,fontface='bold'))  
  grid.text("h)",x=0.25,y=val_scale[2]+0.07*val_scale[2],
            default.units = 'native',gp = gpar(fontsize=8,fontface='bold'))  
  popViewport()
}
