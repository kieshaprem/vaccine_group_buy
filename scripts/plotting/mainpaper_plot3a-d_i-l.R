# ------------------------------------------------------------------------------
# Function to create plot 3a-d (GDP per capita perspective) and 3i-l (CHE per 
# capita perspective) in the Main Paper
# ------------------------------------------------------------------------------

hybrid_bar <- function(mechanism, dataset, affordability_metric){
  dataset$iso2c <- countrycode(dataset$iso3c, origin='iso3c', 
                               destination = 'iso2c')
  pushViewport(plotViewport(c(2,1,1,3),xscale=c(0.5,11.5),yscale=c(0,50)))
  
  if (mechanism == "bilateral"){
    # legend
    grid.polygon(x = c(4.6,5,5,4.6),
                 y = c(51,51,52,52),
                 default.units = 'native',
                 gp = gpar(col = 'grey10', fill = 'grey10',alpha=0.5))
    
    grid.polygon(x = c(6.4,6.8,6.8,6.4),
                 y = c(51,51,52,52),
                 default.units = 'native',
                 gp = gpar(col = 'red', fill = 'red'))
    
    if (affordability_metric=='perdose_gdppc_percent'){
      affordability_metric_name <- 'GDP per capita'
    }
    if (affordability_metric=='perdose_chepc_percent'){
      affordability_metric_name <- 'CHE per capita'
    }
  }
  
  # bar chart indicating vaccine price per dose for each country
  for (i in (1:11)){
    grid.polygon(x = c(i+c(-0.4,0.4),rev(i+c(-0.4,0.4))),
                 y = c(0,0,dataset[i,]$perdose,
                       dataset[i,]$perdose),
                 default.units = 'native',
                 gp = gpar(col = 'grey10', fill = 'grey10',alpha=0.5))
  }
  if (affordability_metric=='perdose_gdppc_percent'){
    y_vals <- dataset[,affordability_metric]*50
    # secondary y-axis
    grid.yaxis(at=c(0,10,20,30,40,50), 
               label=c("0.0%","0.2%","0.4%","0.6%","0.8%","1.0%"),
               gp=gpar(fontsize=5),main=FALSE)
  }
  if (affordability_metric=='perdose_chepc_percent'){
    y_vals <- dataset[,affordability_metric]
    # secondary y-axis
    grid.yaxis(at=c(0,10,20,30,40,50), 
               label=c("0%","10%","20%","30%","40%","50%"),
               gp=gpar(fontsize=5),main=FALSE)
  }
  
  # points and lines to show the trend of affordability (financial burden) of
  # vaccine price per dose expressed as % of GDP or CHE per capita
  grid.lines(seq(1,11), y_vals, default.units = "native",
             gp=gpar(col="red", lwd=0.8))
  grid.points(seq(1,11), y_vals, default.units = "native",
              gp=gpar(col="red", cex=0.4),pch=16)
  
  # primary y-axis
  grid.yaxis(at=c(0,10,20,30,40,50), 
             label=c("$0","$10","$20","$30","$40","$50"),gp=gpar(fontsize=5))
  # x-axis
  grid.xaxis(at=seq(1,11), 
             label=(dataset$iso2c),gp=gpar(fontsize=4))
  popViewport()
}
