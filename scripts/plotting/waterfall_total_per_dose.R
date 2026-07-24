#uniform <- uniform[order(-dataset$`GDP per capita (2022)`),]
library(grid)
waterfall_volume_total_per_dose <- function(mechanism){
  if (mechanism=='uniform'){
    filename <- "waterfall_uniform_total_perdose"
    title <- 'Uniform Pricing'
    grid.text('e)',
              x = unit(0,'npc')+unit(1.4,'lines'),
              y = unit(0,'npc')+unit(27,'lines'),
              gp = gpar(fontsize=8,fontface='bold'))
    data <- uniform
  }
if (mechanism=='gdp_adj'){
     data <- adj_gdppc
    grid.text('f)',
              x = unit(0,'npc')+unit(1.4,'lines'),
              y = unit(0,'npc')+unit(27,'lines'),
              gp = gpar(fontsize=8,fontface='bold'))
}

  if (mechanism=='chepc_adj'){
    data <- adj_chepc
    grid.text('g)',
              x = unit(0,'npc')+unit(1.4,'lines'),
              y = unit(0,'npc')+unit(27,'lines'),
              gp = gpar(fontsize=8,fontface='bold'))
  }
  
  baseline <- bilateral
  baseline$iso2c <- countrycode(bilateral$iso3c,origin = 'iso3c',
                                destination = 'iso2c')
  order_magnitude <- order_of_magnitude(max(bilateral$total_perdose))
  val_scale <- c(-1e8,ceiling(max(bilateral$total_perdose)/order_magnitude)*order_magnitude+3e8)
  
  pushViewport(plotViewport(c(1,2,2,1),xscale=c(-1,80),yscale=val_scale))
  
  y_axis_locations <- axisTicks(val_scale,log=FALSE)
  print(y_axis_locations)
  y_axis_labels <- sapply(y_axis_locations,function(x){get_formatted_number(x,others = 'B')})

  grid.yaxis(at = y_axis_locations, label = y_axis_labels, gp=gpar(fontsize=6,col='black'))
  
  
  # grid.yaxis(at=c(0,1000,2000,3000,4000,5000,6000),
  #            label=c('0','1,000','2,000','3,000','4,000','5,000','6,000'),
  #            gp=gpar(fontsize=5))
  x_axis_loc <- seq(3,75,7)
  grid.xaxis(at=c(x_axis_loc), 
             label=c(baseline$iso2c),
             gp=gpar(fontsize=5))
  for (i in (1:nrow(data))){
    grid.polygon(x = c(x_axis_loc[i]+c(-0.55,0.55),rev(x_axis_loc[i]+c(-0.55,0.55))),
                 y = c(-1e8,-1e8,baseline[i,]$total_perdose,
                       baseline[i,]$total_perdose),
                 default.units = 'native',
                 gp = gpar(col = 'grey50', fill = 'grey50', alpha =0.5))
    
    if (data[i,]$savings_perdose>0){
      grid.polygon(x = c(x_axis_loc[i]+c(0.65,2.35),rev(x_axis_loc[i]+c(0.65,2.35))),
                   y = c(baseline[i,]$total_perdose,
                         baseline[i,]$total_perdose,
                         data[i,]$total_perdose,
                         data[i,]$total_perdose),
                   default.units = 'native',
                   gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
      grid.text(paste0("-",get_formatted_number(data[i,]$total_savings_perdose)),
                x=x_axis_loc[i]+1,y=baseline[i,]$total_perdose+1.5e8,default.units = 'native',gp=gpar(fontsize=6))
    } else{
      grid.polygon(x = c(x_axis_loc[i]+c(0.65,2.35),rev(x_axis_loc[i]+c(0.65,2.35))),
                   y=c(baseline[i,]$total_perdose,
                   baseline[i,]$total_perdose,
                   data[i,]$total_perdose,
                   data[i,]$total_perdose),
                   default.units = 'native',
                   gp = gpar(col = 'red', fill = 'red',alpha=0.5))
      grid.text(paste0("+",get_formatted_number(data[i,]$total_savings_perdose,others = 'B')),
                x=x_axis_loc[i]+0.5,y=data[i,]$total_perdose+1.5e8,default.units = 'native' ,gp=gpar(fontsize=6))
    }
  }

  grid.rect(gp=gpar(fill=NA))
 
  if (mechanism == "uniform"){
    grid.text('Total expenditure ($)',
              x = unit(0,'npc')+unit(-3,'lines'),
              rot=90,gp = gpar(fontsize=8,fontface='bold'))
  }
  popViewport()
}

#waterfall_volume_total_per_dose('gdp_adj')
#waterfall_volume_total_per_dose('uniform')
