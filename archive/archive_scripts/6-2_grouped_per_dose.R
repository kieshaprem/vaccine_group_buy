#uniform <- uniform[order(-dataset$`GDP per capita (2022)`),]

waterfall_volume_per_dose <- function(mechanism){
  
  filename <- "perdose_grouped_all"
  png(paste0("output/plots/",filename,'.png'),
      height=8,width=8,units='cm',res=700,pointsize=10)
    grid.newpage()
    pushViewport(plotViewport(c(3,3,3,2),xscale=c(0.5,41.5),yscale=c(0,2)))
    
    # grid.yaxis(at=c(0,5,10), 
    #            label=c("0%","0.5%","1%"),gp=gpar(fontsize=7),main=FALSE)
    
    # grid.yaxis(at=c(-5,0,5,10), 
    #            label=c(expression(paste(-10^5)),
    #                    1,
    #                    expression(paste(10^5)),
    #                    expression(paste(10^10))),
    #            gp=gpar(fontsize=8))
    
    grid.yaxis(at=c(0,1,2),label=c("$1","$10","$100"))
    x_axis_loc <- seq(3,40,4)
    
    grid.rect()
    for (i in (1:10)){
      grid.polygon(x = c(x_axis_loc[i]+c(-1.35,-0.45),rev(x_axis_loc[i]+c(-1.35,-0.45))),
                   y = c(0,0,log(x=bilateral[i,]$perdose,base=10),
                         log(x=bilateral[i,]$perdose,base=10)),
                   default.units = 'native',
                   gp = gpar(col = 'grey', fill = 'grey'))
      
      grid.polygon(x = c(x_axis_loc[i]+c(-0.45,0.45),rev(x_axis_loc[i]+c(-0.45,0.45))),
                   y = c(0,0,log(x=uniform[i,]$perdose,base=10),
                         log(x=uniform[i,]$perdose,base=10)),
                   default.units = 'native',
                   gp = gpar(col = 'blue', fill = 'blue'))
      
      grid.polygon(x = c(x_axis_loc[i]+c(0.45,1.35),rev(x_axis_loc[i]+c(0.45,1.35))),
                   y = c(0,0,log(x=gdp_adj[i,]$perdose,base=10),
                         log(x=gdp_adj[i,]$perdose,base=10)),
                   default.units = 'native',
                   gp = gpar(col = 'orange', fill = 'orange'))
      
      
      # 
      # 
      # 
      # if (data[i,]$total_savings_perdose<0){
      #   grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
      #                y = c(0,0,
      #                      -log(x=abs(data[i,]$total_savings_perdose),base=10),
      #                      -log(x=abs(data[i,]$total_savings_perdose),base=10)),
      #                default.units = 'native',
      #                gp = gpar(col = 'maroon', fill = 'maroon'))}
      # 
      # else{
      #   grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
      #                y = c(0,0,
      #                      log(x=abs(data[i,]$total_savings_perdose),base=10),
      #                      log(x=abs(data[i,]$total_savings_perdose),base=10)),
      #                default.units = 'native',
      #                gp = gpar(col = 'maroon', fill = 'maroon'))}
      # 
      # if (data[i,]$savings_perdose<0){
      #   grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
      #                y = c(0,0,
      #                      -log(x=abs(data[i,]$savings_perdose),base=10),
      #                      -log(x=abs(data[i,]$savings_perdose),base=10)),
      #                default.units = 'native',
      #                gp = gpar(col = '#f09f9c', fill = '#f09f9c'))
      # }
      # 
      # else{
      #   grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
      #                y = c(0,0,
      #                      log(x=abs(data[i,]$savings_perdose),base=10),
      #                      log(x=abs(data[i,]$savings_perdose),base=10)),
      #                default.units = 'native',
      #                gp = gpar(col = '#f09f9c', fill = '#f09f9c'))
      # }
      # 
      # grid.polygon(x = c(i+c(-0.45,0.45),rev(i+c(-0.45,0.45))),
      #              y = c(0,0,log(x=data[i,]$perdose,base=10),
      #                    log(x=data[i,]$perdose,base=10)),
      #              default.units = 'native',
      #              gp = gpar(col = '#c76b98', fill = '#c76b98'))
      # 
    }
    
    #grid.lines(seq(1,10), uniform$perdose_gdppc_percent*10, default.units = "native",
    #gp=gpar(col="#fcc3a3", lwd=0.8))
    
    #grid.points(seq(1,10), uniform$perdose_gdppc_percent*10, default.units = "native",
    #gp=gpar(col="#fcc3a3", cex=0.4),pch=16)
    
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
    # 
    grid.text('Price per dose',x = unit(0,'npc')+unit(8,'lines'),y = unit(0,'npc')+unit(18,'lines'),gp = gpar(fontsize=8,fontface='bold'))
    #grid.text(paste0('Total per dose ($)'),x = unit(0,'npc')+unit(4.1,'lines'),y = unit(0,'npc')+unit(23.9,'lines'),gp = gpar(fontsize=6))
    #grid.text(paste0('Price per dose ($)'),x = unit(0,'npc')+unit(12.4,'lines'),y = unit(0,'npc')+unit(23.9,'lines'),gp = gpar(fontsize=6))
    #grid.text(paste0('Price per dose (% of GDP per capita)'),x = unit(0,'npc')+unit(5.7,'lines'),y = unit(0,'npc')+unit(22.8,'lines'),gp = gpar(fontsize=6))
    grid.text(paste0('ASEAN Countries ordered by GDP per capita'),x = unit(0,'npc')+unit(8,'lines'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=8,fontface='bold'))
    grid.xaxis(at=c(0.5,x_axis_loc,41.5), 
               label=rev(c(" ","SG","BN","MY","TH","ID","VN","PH","LA","KH","MM"," ")),gp=gpar(fontsize=8))
    popViewport()
    
  dev.off()
}

