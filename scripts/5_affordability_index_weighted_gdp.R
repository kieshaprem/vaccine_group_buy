png(paste0("output/plots/plot_affordability_index_weighted_gdp.png"),height=8,width=8,units='cm',res=700,pointsize=10)
if(1)
{
  
  pushViewport(plotViewport(c(4,4,1,2),xscale=c(-max(log(x=ordered_data[1:10,5],base=10))-0.02,-min(log(x=ordered_data[1:10,5],base=10))+0.02),yscale=c(-0.1,1.32)))#)NEW
  grid.rect()
  
  #grid.yaxis(at=seq(6,10), 
  #label=format(c(1,10,100,1000,10000), big.mark = ",", scientific = FALSE),gp=gpar(fontsize=8))
  grid.xaxis(at=c(-4.5,-4,-3.5), 
             label=c(expression(paste(10^"4.5")),"10,000",expression(paste(10^"3.5"))),gp=gpar(fontsize=8))
  
  grid.yaxis()
  #grid.xaxis()
  grid.lines(gp=gpar(lty=2,lwd=0.8))
  grid.text(paste0('GDP per capita in 2022 (current US$)'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  grid.text(paste0('Affordability index (%)'),x = unit(0,'npc')+unit(-3.6,'lines'),rot=90,gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  
  #grid.lines(-log(x=ordered_data[1:10,5],base=10), ordered_data[1:10,]$`Affordability Index`, default.units = "native",
             #gp=gpar(col="navy", lwd=0.8))
  grid.lines(-log(x=ordered_data[1:10,5],base=10), ordered_data[1:10,25], default.units = "native",
  gp=gpar(col="red", lwd=0.8))
  
  grid.points(-log(x=ordered_data[1:10,5],base=10), ordered_data[1:10,25], default.units = "native",
              gp=gpar(col="red", cex=0.4, alpha=0.5),pch=16)
  
  #grid.lines(-log(x=ordered_data[1:10,5],base=10), ordered_data[1:10,33], default.units = "native",
  #gp=gpar(col="orange", lwd=0.8))
  
  #legend_box <- rectGrob(x = 0.73, y = 0.85, width = 0.4, height = 0.2,
  #gp = gpar(col = "#A9A9A9", alpha=0.8, lty = 1, lwd=0.8))
  
  # vax_1_box <- rectGrob(x = 0.05, y = 0.95, width = 0.025, height = 0.0125,
  #                       gp = gpar(col = "navy", fill = "#0042A6", lty = 1, lwd=0.8))
  # vax_2_box <- rectGrob(x = 0.05, y = 0.90, width = 0.025, height = 0.0125,
  #                       gp = gpar(col = "red", fill = "#ED2939", lty = 1, lwd=0.8))
  # 
  # vax_3_box <- rectGrob(x = 0.05, y = 0.85, width = 0.025, height = 0.0125,
  #                       gp = gpar(col = "orange", fill = "orange", lty = 1, lwd=0.8))
  # 
  # #grid.draw(legend_box)
  # grid.draw(vax_1_box)
  # grid.draw(vax_2_box)
  # grid.draw(vax_3_box)
  
  #grid.text("Scenarios", x=0.18,y=0.9,gp=gpar(fontsize=6, fontface='bold'))
  # grid.text("         Equal pricing", x=0.15,y=0.95,gp=gpar(fontsize=6))
  # grid.text("         GDP Weighting", x=0.17,y=0.90,gp=gpar(fontsize=6))
  # grid.text("Adjusted GDP Weighting", x=0.3,y=0.85,gp=gpar(fontsize=6))
  # 
  
  popViewport()
}
dev.off()