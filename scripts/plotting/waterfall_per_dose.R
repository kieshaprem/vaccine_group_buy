#uniform <- uniform[order(-dataset$`GDP per capita (2022)`),]

library(grid)
waterfall_volume_per_dose <- function(mechanism){  
  baseline <- bilateral
  baseline$iso2c <- countrycode(baseline$iso3c,origin = 'iso3c',destination = 'iso2c')
  
  pushViewport(plotViewport(c(1,2,1,1),xscale=c(-1,80),yscale=c(0,60)))
  x_axis_loc <- seq(3,75,7)
  grid.xaxis(at=c(x_axis_loc), 
             label=c(baseline$iso2c),
             gp=gpar(fontsize=6))
  grid.yaxis(gp=gpar(fontsize=6))
  
  if (mechanism=='uniform'){
    filename <- "waterfall_uniform_perdose"
    # grid.text('a) Uniform Pricing',
    #           x = unit(0,'npc')+unit(4,'lines'),
    #           y = unit(0,'npc')+unit(19,'lines'),
    #           gp = gpar(fontsize=8,fontface='bold'))
    grid.text("a) Fixed Pricing",x=12,y=65,default.units = 'native' ,
              gp=gpar(fontsize=8,fontface = 'bold'))
    
    data <- uniform}
if (mechanism=='gdp_adj'){
    filename <- "waterfall_gdp_perdose"
    data <- adj_gdppc
    grid.text("b) Weighted by GDP per capita",x=22,y=65,default.units = 'native' ,
              gp=gpar(fontsize=8,fontface = 'bold'))
    grid.text(as.expression(bquote(alpha == 10)),x=70,y=65,default.units = 'native' ,
              gp=gpar(fontsize=8,fontface = 'bold'))
    
    # grid.text('b) Weighted by GDP per capita',
    #           x = unit(0,'npc')+unit(6,'lines'),
    #           y = unit(0,'npc')+unit(26.8,'lines'),
    #           gp = gpar(fontsize=8,fontface='bold'))
    # grid.text(as.expression(bquote(alpha == 10)),  x = unit(0,'npc')+unit(14,'lines'),
    #           y = unit(0,'npc')+unit(19,'lines'),gp=gpar(fontsize=8))
}
    if (mechanism=='chepc_adj'){
    data <- adj_chepc
    # grid.text('c) Weighted by CHE per capita',
    #           x = unit(0,'npc')+unit(6,'lines'),
    #           y = unit(0,'npc')+unit(26.8,'lines'),
    #           gp = gpar(fontsize=8,fontface='bold'))
    
    grid.text("c) Weighted by CHE per capita",x=22,y=65,default.units = 'native' ,
              gp=gpar(fontsize=8,fontface='bold'))
    grid.text(as.expression(bquote(alpha == 10)),x=70,y=65,default.units = 'native' ,
              gp=gpar(fontsize=8,fontface='bold'))
    
    # grid.text(as.expression(bquote(alpha == 10)), x = unit(0,'npc')+unit(14,'lines'),
    #           y = unit(0,'npc')+unit(19,'lines'),gp=gpar(fontsize=8))
  }

  
  for (i in (1:nrow(data))){
    grid.polygon(x = c(x_axis_loc[i]+c(-0.55,0.55),rev(x_axis_loc[i]+c(-0.55,0.55))),
                 y = c(0,0,baseline[i,]$perdose,
                       baseline[i,]$perdose),
                 default.units = 'native',
                 gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
    
    if (data[i,]$savings_perdose>0){
      grid.polygon(x = c(x_axis_loc[i]+c(0.65,2.35),rev(x_axis_loc[i]+c(0.65,2.35))),
                   y = c(baseline[i,]$perdose,
                         baseline[i,]$perdose,
                         data[i,]$perdose,
                         data[i,]$perdose),
                   default.units = 'native',
                   gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
      grid.text(paste0("-",sprintf("%.1f", round(abs(data[i,]$savings_perdose),1))),
                x=x_axis_loc[i]+0.5,y=baseline[i,]$perdose+1.5,default.units = 'native',gp=gpar(fontsize=5))
    }
    
    else{
      grid.polygon(x = c(x_axis_loc[i]+c(0.65,2.35),rev(x_axis_loc[i]+c(0.65,2.35))),
                   y = c(baseline[i,]$perdose,
                         baseline[i,]$perdose,
                         data[i,]$perdose,
                         data[i,]$perdose),
                   default.units = 'native',
                   gp = gpar(col = 'red', fill = 'red',alpha=0.5))
      grid.text(paste0("+",sprintf("%.1f", round(abs(data[i,]$savings_perdose),1))),
                x=x_axis_loc[i]+0.3,y=data[i,]$perdose+1.5,default.units = 'native' ,gp=gpar(fontsize=5))
    }
  }
  grid.rect(gp=gpar(fill=NA))
    
  # legend
  if (mechanism == "uniform"){
    grid.text('Price per dose (US$)',
              x = unit(0,'npc')+unit(-3,'lines'),
              rot=90,gp = gpar(fontsize=8,fontface='bold'))
    
    
    grid.polygon(x = c(3,5,5,3),
                 y = c(57.5,57.5,58.5,58.5),
                 default.units = 'native',
                 gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
    grid.text("Bilateral price ($)",x=20,y=58,default.units = 'native' ,gp=gpar(fontsize=6))
    
    grid.polygon(x = c(3,5,5,3),
                 y = c(54.5,54.5,55.5,55.5),
                 default.units = 'native',
                 gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
    
    grid.text("  Price decrease ($)",x=20,y=55,default.units = 'native' ,gp=gpar(fontsize=6))
    
    grid.polygon(x = c(3,5,5,3),
                 y = c(51.5,51.5,52.5,52.5),
                 default.units = 'native',
                 gp = gpar(col = 'red', fill = 'red',alpha=0.5))  
    grid.text(" Price increase ($)",x=20,y=52,default.units = 'native' ,gp=gpar(fontsize=6))
    
  }

  popViewport()
}

