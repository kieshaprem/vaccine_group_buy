#uniform <- uniform[order(-dataset$`GDP per capita (2022)`),]

waterfall_volume_per_dose <- function(mechanism){

  
  if (mechanism=='uniform'){
    filename <- "waterfall_uniform_perdose"
    data <- uniform
    title <- 'Uniform Pricing'
  } else{
    filename <- "waterfall_gdp_perdose"
    data <- gdp_adj
    title <- 'Weighted by GDP'
  }
  
  png(paste0("output/plots/",filename,'.png'),
      height=8.2,width=8.2,units='cm',res=700,pointsize=10)
  grid.newpage()
  pushViewport(plotViewport(c(3,4,2,1),xscale=c(0.7,42.3),yscale=c(0,60)))
  x_axis_loc <- seq(3,40,4)
  grid.xaxis(at=c(x_axis_loc), 
             label=rev(c("SG","BN","MY","TH","ID","VN","PH","LA","KH","MM")),
             gp=gpar(fontsize=8))
  
  #grid.yaxis(at=c(0,1,2),label=c("1","10","100"), gp=gpar(fontsize=8))
  
  # if (mechanism=='uniform'){
  #   grid.yaxis(at=c(0,1,2),
  #              label=c("1.00","10.00","100.00"),
  #              gp=gpar(fontsize=8))
  #   # grid.yaxis(at=log(7.5,base=10),label="7.50",
  #   #            gp=gpar(fontsize=))
  #   grid.lines(y=log(7.5,base=10)/2,gp=gpar(lty='dashed',lwd=1))
  # } else{
  #   grid.yaxis(at=c(0,1,2),label=c("1.00","10.00","100.00"),
  #              gp=gpar(fontsize=8))
  # }
  
  grid.yaxis()
  
  for (i in (1:10)){
    grid.polygon(x = c(x_axis_loc[i]+c(-0.45,0.45),rev(x_axis_loc[i]+c(-0.45,0.45))),
                 y = c(0,0,bilateral[i,]$perdose,
                       bilateral[i,]$perdose),
                 default.units = 'native',
                 gp = gpar(col = 'grey', fill = 'grey'))
    
    if (data[i,]$savings_perdose>0){
      grid.polygon(x = c(x_axis_loc[i]+c(0.55,1.45),rev(x_axis_loc[i]+c(0.55,1.45))),
                   y = c(bilateral[i,]$perdose,
                         bilateral[i,]$perdose,
                         data[i,]$perdose,
                         data[i,]$perdose),
                   default.units = 'native',
                   gp = gpar(col = 'blue', fill = 'blue',alpha=0.5))
    }
    
    else{
      grid.polygon(x = c(x_axis_loc[i]+c(0.55,1.45),rev(x_axis_loc[i]+c(0.55,1.45))),
                   y = c(bilateral[i,]$perdose,
                         bilateral[i,]$perdose,
                         data[i,]$perdose,
                         data[i,]$perdose),
                   default.units = 'native',
                   gp = gpar(col = 'red', fill = 'red',alpha=0.5))
    }
  }
  
  grid.rect(gp=gpar(fill=NA))
  
  # grid.polygon(x = c(2,2.4,2.4,2),
  #              y = c(2.2,2.2,2.25,2.25),
  #              default.units = 'native',
  #              gp = gpar(col = '#270f36', fill = '#270f36'))
  
  # grid.polygon(x = c(0.4,0.8,0.8,0.4),
  #              y = c(11,11,11.15,11.15),
  #              default.units = 'native',
  #              gp = gpar(col = '#270f36', fill = '#270f36'))
  # 
  # grid.polygon(x = c(4.2,4.6,4.6,4.2),
  #              y = c(11,11,11.15,11.15),
  #              default.units = 'native',
  #              gp = gpar(col = '#632b6c', fill = '#632b6c'))
  # 
  # 
  # grid.polygon(x = c(0.4,0.8,0.8,0.4),
  #              y = c(10.5,10.5,10.65,10.65),
  #              default.units = 'native',
  #              gp = gpar(col = '#fcc3a3', fill = '#fcc3a3'))
  


  grid.text('Price per dose ($)',
            x = unit(0,'npc')+unit(-4.5,'lines'),
            rot=90,gp = gpar(fontsize=8,fontface='bold'))
  grid.text(title,x = unit(0,'npc')+unit(9,'lines'),
            y = unit(0,'npc')+unit(19,'lines'),
            gp = gpar(fontsize=8,fontface='bold'))
  #grid.text(paste0('Total per dose ($)'),x = unit(0,'npc')+unit(4.1,'lines'),y = unit(0,'npc')+unit(23.9,'lines'),gp = gpar(fontsize=6))
  #grid.text(paste0('Price per dose ($)'),x = unit(0,'npc')+unit(12.4,'lines'),y = unit(0,'npc')+unit(23.9,'lines'),gp = gpar(fontsize=6))
  #grid.text(paste0('Price per dose (% of GDP per capita)'),x = unit(0,'npc')+unit(5.7,'lines'),y = unit(0,'npc')+unit(22.8,'lines'),gp = gpar(fontsize=6))
  grid.text(paste0('ASEAN Countries ordered by GDP per capita'),
            x = unit(0,'npc')+unit(9,'lines'),
            y = unit(0,'npc')+unit(-2.8,'lines'),
            gp = gpar(fontsize=8,fontface='bold'))
  popViewport()
  
  dev.off()
}


waterfall_volume_per_dose('gdp_adj')
waterfall_volume_per_dose('uniform')
