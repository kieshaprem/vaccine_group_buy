#uniform <- uniform[order(-dataset$`GDP per capita (2022)`),]

waterfall_volume_total_per_dose <- function(mechanism){
  if (mechanism=='uniform'){
    filename <- "waterfall_uniform_total_perdose"
    data <- uniform
    title <- 'Uniform Pricing'
  }
  else{
    filename <- "waterfall_gdp_totalperdose"
    data <- gdp_adj
    title <- 'Weighted by GDP'
  }
  
  png(paste0("output/plots/",filename,'.png'),
      height=8.2,width=8.2,units='cm',res=700,pointsize=10)
  grid.newpage()
  pushViewport(plotViewport(c(3,4,2,1),xscale=c(0.7,42.3),yscale=c(-50,2800)))
  
  # grid.yaxis(at=seq(6,10),label = c("1","10","100","1,000","10,000"),
  #            gp=gpar(fontsize=8))
  grid.yaxis(at=c(0,500,1000,1500,2000,2500),
             label=c('0','500','1,000','1,500','2,000','2,500'),
             gp=gpar(fontsize=8))
  x_axis_loc <- seq(3,40,4)
  grid.xaxis(at=c(x_axis_loc), 
             label=rev(c("SG","BN","MY","TH","ID","VN","PH","LA","KH","MM")),
             gp=gpar(fontsize=8))
  
  for (i in (1:10)){
    grid.polygon(x = c(x_axis_loc[i]+c(-0.45,0.45),rev(x_axis_loc[i]+c(-0.45,0.45))),
                 y = c(-50,-50,bilateral[i,]$total_perdose/1e6,
                       bilateral[i,]$total_perdose/1e6),
                       # log(x=bilateral[i,]$total_perdose/1e6,base=10),
                       # log(x=bilateral[i,]$total_perdose/1e6,base=10)),
                 default.units = 'native',
                 gp = gpar(col = 'grey', fill = 'grey'))
    
    if (data[i,]$savings_perdose>0){
      grid.polygon(x = c(x_axis_loc[i]+c(0.55,1.45),rev(x_axis_loc[i]+c(0.55,1.45))),
                   y = c(bilateral[i,]$total_perdose/1e6,
                         bilateral[i,]$total_perdose/1e6,
                         data[i,]$total_perdose/1e6,
                         data[i,]$total_perdose/1e6),
                   # y = c(log(x=bilateral[i,]$total_perdose/1e6,base=10),
                   #       log(x=bilateral[i,]$total_perdose/1e6,base=10),
                   #       log(x=data[i,]$total_perdose/1e6,base=10),
                   #       log(x=data[i,]$total_perdose/1e6,base=10)),
                   default.units = 'native',
                   gp = gpar(col = 'blue', fill = 'blue',alpha=0.5))
    } else{
      grid.polygon(x = c(x_axis_loc[i]+c(0.55,1.45),rev(x_axis_loc[i]+c(0.55,1.45))),
                   y=c(bilateral[i,]$total_perdose/1e6,
                   bilateral[i,]$total_perdose/1e6,
                   data[i,]$total_perdose/1e6,
                   data[i,]$total_perdose/1e6),
                   # y = c(log(x=bilateral[i,]$total_perdose/1e6,base=10),
                   #       log(x=bilateral[i,]$total_perdose/1e6,base=10),
                   #       log(x=data[i,]$total_perdose/1e6,base=10),
                   #       log(x=data[i,]$total_perdose/1e6,base=10)),
                   default.units = 'native',
                   gp = gpar(col = 'red', fill = 'red',alpha=0.5))
    }
  }
  
  grid.rect(gp=gpar(fill=NA))
 
  grid.text('Total per dose ($ millions)',
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

waterfall_volume_total_per_dose('gdp_adj')
waterfall_volume_total_per_dose('uniform')
