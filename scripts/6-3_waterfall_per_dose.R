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
  pushViewport(plotViewport(c(3,4,2,1),xscale=c(-1,71),yscale=c(0,60)))
  x_axis_loc <- seq(3,70,7)
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
    grid.polygon(x = c(x_axis_loc[i]+c(-0.55,0.55),rev(x_axis_loc[i]+c(-0.55,0.55))),
                 y = c(0,0,bilateral[i,]$perdose,
                       bilateral[i,]$perdose),
                 default.units = 'native',
                 gp = gpar(col = 'grey', fill = 'grey'))
    
    if (data[i,]$savings_perdose>0){
      grid.polygon(x = c(x_axis_loc[i]+c(0.65,2.35),rev(x_axis_loc[i]+c(0.65,2.35))),
                   y = c(bilateral[i,]$perdose,
                         bilateral[i,]$perdose,
                         data[i,]$perdose,
                         data[i,]$perdose),
                   default.units = 'native',
                   gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
      grid.text(paste0("-",sprintf("%.1f", round(abs(data[i,]$savings_perdose),1))),
                x=x_axis_loc[i]+0.3,y=bilateral[i,]$perdose+1.5,default.units = 'native',gp=gpar(fontsize=7))
    }
    
    else{
      grid.polygon(x = c(x_axis_loc[i]+c(0.65,2.35),rev(x_axis_loc[i]+c(0.65,2.35))),
                   y = c(bilateral[i,]$perdose,
                         bilateral[i,]$perdose,
                         data[i,]$perdose,
                         data[i,]$perdose),
                   default.units = 'native',
                   gp = gpar(col = 'red', fill = 'red',alpha=0.5))
      grid.text(paste0("+",sprintf("%.1f", round(abs(data[i,]$savings_perdose),1))),
                x=x_axis_loc[i]+0.3,y=data[i,]$perdose+1.5,default.units = 'native' ,gp=gpar(fontsize=7))
    }
  }
  grid.rect(gp=gpar(fill=NA))
  
  grid.polygon(x = c(2,4,4,2),
               y = c(57.5,57.5,58.5,58.5),
               default.units = 'native',
               gp = gpar(col = 'grey', fill = 'grey'))
  grid.text("Bilateral price ($)",x=16,y=58,default.units = 'native' ,gp=gpar(fontsize=7))
  
  grid.polygon(x = c(2,4,4,2),
               y = c(54.5,54.5,55.5,55.5),
               default.units = 'native',
               gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
  
  grid.text("  Price decrease ($)",x=16,y=55,default.units = 'native' ,gp=gpar(fontsize=7))
  
  grid.polygon(x = c(2,4,4,2),
               y = c(51.5,51.5,52.5,52.5),
               default.units = 'native',
               gp = gpar(col = 'red', fill = 'red',alpha=0.5))  
  grid.text(" Price increase ($)",x=16,y=52,default.units = 'native' ,gp=gpar(fontsize=7))
  

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
