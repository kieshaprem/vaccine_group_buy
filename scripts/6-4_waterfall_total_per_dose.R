#uniform <- uniform[order(-dataset$`GDP per capita (2022)`),]
library(grid)
waterfall_volume_total_per_dose <- function(mechanism){
  
  pushViewport(plotViewport(c(1,2,1,1),xscale=c(-1,71),yscale=c(-50,3000)))
  
  grid.yaxis(at=c(0,1000,2000,3000),
             label=c('0','1,000','2,000','3,000'),
             gp=gpar(fontsize=5))
  x_axis_loc <- seq(3,70,7)
  grid.xaxis(at=c(x_axis_loc), 
             label=rev(c("SG","BN","MY","TH","ID","VN","PH","LA","KH","MM")),
             gp=gpar(fontsize=5))
  
  if (mechanism=='uniform'){
    filename <- "waterfall_uniform_total_perdose"
    data <- uniform
    title <- 'Uniform Pricing'
    grid.text('c)',
              x = unit(0,'npc')+unit(0.4,'lines'),
              y = unit(0,'npc')+unit(15.6,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
  }
  else{
    filename <- "waterfall_gdp_totalperdose"
    data <- gdp_adj
    grid.text('d)',
              x = unit(0,'npc')+unit(0.4,'lines'),
              y = unit(0,'npc')+unit(15.6,'lines'),
              gp = gpar(fontsize=6,fontface='bold'))
  }
  
  for (i in (1:10)){
    grid.polygon(x = c(x_axis_loc[i]+c(-0.55,0.55),rev(x_axis_loc[i]+c(-0.55,0.55))),
                 y = c(-50,-50,bilateral[i,]$total_perdose/1e6,
                       bilateral[i,]$total_perdose/1e6),
                 default.units = 'native',
                 gp = gpar(col = 'grey50', fill = 'grey50', alpha =0.5))
    
    if (data[i,]$savings_perdose>0){
      grid.polygon(x = c(x_axis_loc[i]+c(0.65,2.35),rev(x_axis_loc[i]+c(0.65,2.35))),
                   y = c(bilateral[i,]$total_perdose/1e6,
                         bilateral[i,]$total_perdose/1e6,
                         data[i,]$total_perdose/1e6,
                         data[i,]$total_perdose/1e6),
                   default.units = 'native',
                   gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
      grid.text(paste0("-",round(data[i,]$total_savings_perdose/1e6),"M"),
                x=x_axis_loc[i]+0.3,y=bilateral[i,]$total_perdose/1e6+80,default.units = 'native',gp=gpar(fontsize=4))
    } else{
      grid.polygon(x = c(x_axis_loc[i]+c(0.65,2.35),rev(x_axis_loc[i]+c(0.65,2.35))),
                   y=c(bilateral[i,]$total_perdose/1e6,
                   bilateral[i,]$total_perdose/1e6,
                   data[i,]$total_perdose/1e6,
                   data[i,]$total_perdose/1e6),
                   default.units = 'native',
                   gp = gpar(col = 'red', fill = 'red',alpha=0.5))
      grid.text(paste0("+",round(abs(data[i,]$total_savings_perdose/1e6)),"M"),
                x=x_axis_loc[i]+0.3,y=data[i,]$total_perdose/1e6+80,default.units = 'native' ,gp=gpar(fontsize=4))
    }
  }
  grid.polygon(x = c(2,4,4,2),
               y = c(2875,2875,2925,2925),
               default.units = 'native',
               gp = gpar(col = 'grey50', fill = 'grey50', alpha=0.5))
  grid.text("Bilateral price ($)",x=16,y=2900,default.units = 'native' ,gp=gpar(fontsize=4))
  
  grid.polygon(x = c(2,4,4,2),
               y = c(2725,2725,2775,2775),
               default.units = 'native',
               gp = gpar(col = '#0042A6', fill = '#0042A6',alpha=0.5))
  
  grid.text("  Price decrease ($)",x=16,y=2750,default.units = 'native' ,gp=gpar(fontsize=4))
  
  grid.polygon(x = c(2,4,4,2),
               y = c(2575,2575,2625,2625),
               default.units = 'native',
               gp = gpar(col = 'red', fill = 'red',alpha=0.5))  
  grid.text(" Price increase ($)",x=16,y=2600,default.units = 'native' ,gp=gpar(fontsize=4))
  
  grid.rect(gp=gpar(fill=NA))
 
  if (mechanism == "uniform"){
    grid.text('Total per dose ($ millions)',
              x = unit(0,'npc')+unit(-3.5,'lines'),
              rot=90,gp = gpar(fontsize=6,fontface='bold'))
  }
  # grid.text(title,x = unit(0,'npc')+unit(5,'lines'),
  #           y = unit(0,'npc')+unit(19,'lines'),
  #           gp = gpar(fontsize=8,fontface='bold'))
    # grid.text(paste0('ASEAN Countries ordered by GDP per capita'),
    #         x = unit(0,'npc')+unit(9,'lines'),
    #         y = unit(0,'npc')+unit(-2.8,'lines'),
    #         gp = gpar(fontsize=8,fontface='bold'))
  
  popViewport()
  #dev.off()
}

#waterfall_volume_total_per_dose('gdp_adj')
#waterfall_volume_total_per_dose('uniform')
