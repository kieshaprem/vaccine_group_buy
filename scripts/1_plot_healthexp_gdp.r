library(grid)

# png(paste0("output/plots/plot_gdp_oop_all.png"),height=8,width=12,units='cm',res=700,pointsize=10)
# if(1)
# {
#   #grid.newpage()
#   #pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1,width=rep(1,1),height=rep(1,1))))
#   
#   #pushViewport(viewport(layout.pos.row=1,layout.pos.col=1))
#   #NEW(
#   pushViewport(plotViewport(c(4,5,1,2),xscale=c(7,18),yscale=c(0,12)))#)NEW
#   grid.rect()
#   #grid.yaxis()
#   #grid.xaxis()
#   grid.yaxis(at=seq(0,12,2), 
#             label=format(as.character(round(2^(seq(0,12,2)))), big.mark = ",", scientific = FALSE))
#   grid.xaxis(at=seq(8,18,2), 
#              label=format(round(2^(seq(8,18,2))), big.mark=",", scientific=FALSE))
#   grid.text(paste0('GDP per capita in 2022 (current US$)'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=10.5,fontface='bold',col = 'black'))
#   grid.text(paste0('Household OOPS per capita \nin 2021 (current US$)'),x = unit(0,'npc')+unit(-3.8,'lines'),rot=90,gp = gpar(fontsize=10.5,fontface='bold',col = 'black'))
#   #grid.lines()
#   grid.points(x=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,2],base=2),
#               y=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,3],base=2),
#               default.units = 'native',pch=16,gp=gpar(alpha=0.8,cex = 0.8, col="#FD1813"))
#   grid.points(x=log(x=gdp_oop[!gdp_oop$Country.Code %in% asean_iso,][,2],base=2),
#               y=log(x=gdp_oop[!gdp_oop$Country.Code %in% asean_iso,][,3],base=2),
#               default.units = 'native',pch=16,gp=gpar(alpha=0.2,cex = 0.8))
# 
#   #grid.text(paste0('r = ',round(cortest$estimate,2), ', 95% CI: (',round(cortest$conf.int[1],2),'–',round(cortest$conf.int[2],2),')'),y = unit(1,'npc')+unit(-2,'lines'),gp = gpar(fontsize=10,fontface='plain',col = 'black'))
#   #popViewport()
#   popViewport()
# }
# dev.off()



model_oops <- lm(log(x=OOP_2021,base=10)~log(x=GDP_2022,base=10), data = gdp_oop_exp)
model_exp <- lm(log(x=EXP_2021,base=10)~log(x=GDP_2022,base=10), data = gdp_oop_exp)



png(paste0("output/plots/plot_gdp_oop_exp_all.png"),height=8,width=12,units='cm',res=700,pointsize=10)
if(1)
{
  #grid.newpage()
  #pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1,width=rep(1,1),height=rep(1,1))))
  
  #pushViewport(viewport(layout.pos.row=1,layout.pos.col=1))
  #NEW(
  pushViewport(plotViewport(c(4,5,1,2),xscale=c(2,6),yscale=c(0,5)))#)NEW
  grid.rect()
  #grid.yaxis()
  #grid.xaxis()

  
  grid.yaxis(at=seq(0,5), 
             label=format(c(1,10,100,1e3,1e4,1e5), big.mark = ",", scientific = FALSE),gp=gpar(fontsize=8))
  grid.xaxis(at=seq(2,6), 
             label=format(c(100,1000,1e4,1e5,1e6), big.mark=",", scientific=FALSE),gp=gpar(fontsize=8))
  
  grid.function(function(input){return (list(x=input,
                                             y=model_oops$coefficients[1]+model_oops$coefficients[2]*input))},
                gp=gpar(lty=2, lwd=0.8))
  
  grid.function(function(input){return (list(x=input,
                                             y=model_exp$coefficients[1]+model_exp$coefficients[2]*input))},
                gp=gpar(lty=2, lwd=0.8))
  
  grid.text(paste0('GDP per capita in 2022 (current US$)'),y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  grid.text(paste0('Healthcare expenditure \nin 2021 (current US$)'),x = unit(0,'npc')+unit(-4.2,'lines'),rot=90,gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  #grid.lines()
  grid.points(x=log(x=gdp_oop[!gdp_oop$Country.Code %in% asean_iso,][,2],base=10),
              y=log(x=gdp_oop[!gdp_oop$Country.Code %in% asean_iso,][,3],base=10),
              default.units = 'native',pch=16,gp=gpar(alpha=0.5,cex = 0.5, lwd=0.8, col="#0047AB"))
  
  
  grid.points(x=log(x=gdp_oop_exp[!gdp_oop_exp$Country.Code %in% asean_iso,][,2],base=10),
              y=log(x=gdp_oop_exp[!gdp_oop_exp$Country.Code %in% asean_iso,][,4],base=10),
              default.units = 'native',pch=18,gp=gpar(alpha=0.5,cex = 0.6,lwd=0.8, col="#40E0D0"))
  

  grid.points(x=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,2],base=10),
              y=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,3],base=10),
              default.units = 'native',pch=16,gp=gpar(alpha=0.8,cex = 0.5, col="#FD1813"))
  grid.points(x=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,2],base=10),
              y=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,3],base=10),
              default.units = 'native',pch=1,gp=gpar(cex = 0.5, lwd=1, col="#0047AB"))
  

  grid.points(x=log(x=gdp_oop_exp[gdp_oop_exp$Country.Code %in% asean_iso,][,2],base=10),
              y=log(x=gdp_oop_exp[gdp_oop_exp$Country.Code %in% asean_iso,][,4],base=10),
              default.units = 'native',pch=18,gp=gpar(alpha=0.8, cex = 0.6, col="#FD1813"))
  grid.points(x=log(x=gdp_oop_exp[gdp_oop_exp$Country.Code %in% asean_iso,][,2],base=10),
              y=log(x=gdp_oop_exp[gdp_oop_exp$Country.Code %in% asean_iso,][,4],base=10),
              default.units = 'native',pch=5,gp=gpar(cex = 0.6,lwd=1, col="#40E0D0"))
  
  
  
  
  legend_box <- rectGrob(x = 0.215, y = 0.875, width = 0.36, height = 0.15,
                         gp = gpar(col = "#A9A9A9", alpha=0.8, lty = 1, lwd=0.8))
  
  vax_1_box <- rectGrob(x = 0.06, y = 0.93, width = 0.025, height = 0.0125,
                        gp = gpar(col = "#0047AB", fill = "navy", lty = 1, lwd=0.8))
  vax_2_box <- rectGrob(x = 0.06, y = 0.88, width = 0.025, height = 0.0125,
                        gp = gpar(col = "#40E0D0", fill = "#40E0D0", lty = 1, lwd=0.8))
  vax_3_box <- rectGrob(x = 0.06, y = 0.83, width = 0.025, height = 0.0125,
                        gp = gpar(col = "#FD1813", fill = "#FD1813", lty = 1, lwd=0.8))
  
  grid.draw(legend_box)
  grid.draw(vax_1_box)
  grid.draw(vax_2_box)
  grid.draw(vax_3_box)
  
  grid.text("                   Household OOPS per capita", x=unit(6.1,"lines"),y=unit(21.5,"lines"),gp=gpar(fontsize=6))
  grid.text("                    Health expenditure per capita", x=unit(6.1,"lines"),y=unit(20.4, "lines"),gp=gpar(fontsize=6))
  grid.text("ASEAN countries", x=unit(6.1,"lines"),y=unit(19.3, "lines"),gp=gpar(fontsize=6))
 

  popViewport()
}
dev.off()

