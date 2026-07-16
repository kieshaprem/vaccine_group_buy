#uniform <- uniform[order(-dataset$`GDP per capita (2022)`),]

library(grid)
waterfall_volume_per_dose <- function(mechanism,coverage){  
  if (coverage=='full'){
    baseline <- bilateral_full
  }
  if (coverage=='real'){
    baseline <- bilateral_real
  }
  baseline$iso2c <- countrycode(baseline$iso3c,origin = 'iso3c',destination = 'iso2c')
  
  pushViewport(plotViewport(c(1,2,1,1),xscale=c(-1,80),yscale=c(0,60)))
  x_axis_loc <- seq(3,75,7)
  grid.xaxis(at=c(x_axis_loc), 
             label=c(baseline$iso2c),
             gp=gpar(fontsize=5))
  grid.yaxis(gp=gpar(fontsize=5))
  
  if (mechanism=='uniform'){
    filename <- "waterfall_uniform_perdose"
    grid.text('a) Uniform Pricing',
              x = unit(0,'npc')+unit(3,'lines'),
              y = unit(0,'npc')+unit(16.3,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
    if (coverage=='full'){
      data <- uniform_full
    }
    if (coverage=='real'){
      data <- uniform_real
    }}
  
if (mechanism=='gdp_adj'){
    filename <- "waterfall_gdp_perdose"
    if (coverage=='full'){
      data <- adj_gdppc_full
    }
    if (coverage=='real'){
      data <- adj_gdppc_real
    }
    grid.text('b) Weighted by GDP per capita',
              x = unit(0,'npc')+unit(5,'lines'),
              y = unit(0,'npc')+unit(16.3,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
    grid.text(as.expression(bquote(alpha == 10)), x=0.1,y=1.05,gp=gpar(fontsize=6))
}
  if (mechanism=='gov_exp_adj'){
    if (coverage=='full'){
      data <- adj_gov_exp_full
    }
    if (coverage=='real'){
      data <- adj_gov_exp_real
    }
    grid.text('b) Weighted by GHE per capita',
              x = unit(0,'npc')+unit(5,'lines'),
              y = unit(0,'npc')+unit(16.3,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
    grid.text(as.expression(bquote(alpha == 10)), x=0.1,y=1.05,gp=gpar(fontsize=6))
  }
  if (mechanism=='chepc_adj'){
    if (coverage=='full'){
      data <- adj_chepc_full
    }
    if (coverage=='real'){
      data <- adj_chepc_real
    }
    grid.text('c) Weighted by CHE per capita',
              x = unit(0,'npc')+unit(5,'lines'),
              y = unit(0,'npc')+unit(16.3,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
    grid.text(as.expression(bquote(alpha == 10)), x=0.1,y=1.05,gp=gpar(fontsize=6))
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
                x=x_axis_loc[i]+0.5,y=baseline[i,]$perdose+1.5,default.units = 'native',gp=gpar(fontsize=4))
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
                x=x_axis_loc[i]+0.3,y=data[i,]$perdose+1.5,default.units = 'native' ,gp=gpar(fontsize=4))
    }
  }
  grid.rect(gp=gpar(fill=NA))
    
  # legend
  if (mechanism == "uniform"){
    grid.text('Price per dose ($)',
              x = unit(0,'npc')+unit(-3.5,'lines'),
              rot=90,gp = gpar(fontsize=6,fontface='bold'))
    
    
    grid.polygon(x = c(3,5,5,3),
                 y = c(57.5,57.5,58.5,58.5),
                 default.units = 'native',
                 gp = gpar(col = 'grey50', fill = 'grey50',alpha=0.5))
    grid.text("Bilateral price ($)",x=20,y=58,default.units = 'native' ,gp=gpar(fontsize=4))
    
    grid.polygon(x = c(3,5,5,3),
                 y = c(54.5,54.5,55.5,55.5),
                 default.units = 'native',
                 gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
    
    grid.text("  Price decrease ($)",x=20,y=55,default.units = 'native' ,gp=gpar(fontsize=4))
    
    grid.polygon(x = c(3,5,5,3),
                 y = c(51.5,51.5,52.5,52.5),
                 default.units = 'native',
                 gp = gpar(col = 'red', fill = 'red',alpha=0.5))  
    grid.text(" Price increase ($)",x=20,y=52,default.units = 'native' ,gp=gpar(fontsize=4))
    
  }

  # grid.text(paste0('ASEAN Countries ordered by GDP per capita'),
  #           x = unit(0,'npc')+unit(9,'lines'),
  #           y = unit(0,'npc')+unit(-2.8,'lines'),
  #           gp = gpar(fontsize=8,fontface='bold'))
  popViewport()
}


# waterfall_volume_per_dose('gdp_adj')
# waterfall_volume_per_dose('uniform')
