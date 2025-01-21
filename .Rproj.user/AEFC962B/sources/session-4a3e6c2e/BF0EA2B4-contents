library(grid)

png(paste0("output/plots/plot_indicators.png"),height=8,width=16.5,units='cm',res=700,pointsize=10)
if(1)
{
  #grid.newpage()
  #pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1,width=rep(1,1),height=rep(1,1))))
  
  #pushViewport(viewport(layout.pos.row=1,layout.pos.col=1))
  #NEW(
  pushViewport(plotViewport(c(4,5,1,2),xscale=c(10,20),yscale=c(1,11)))#)NEW
  grid.rect()
  #grid.yaxis()
  #grid.xaxis()
  grid.yaxis(at=seq(from=1, to=11, by=2), label=as.character(round(2^(seq(from=1, to=11, by=2)))))
  grid.xaxis(at=log(x=c(2000,4000,16000,64000,256000,1024000),base=2), 
             label=as.character(c(2000,4000,16000,64000,256000,1024000)))
  grid.text(paste0('Gross Domestic Product (GDP) per capita in US$'),y = unit(0,'npc')+unit(-3,'lines'),gp = gpar(fontsize=10,fontface='bold',col = 'black'))
  grid.text(paste0('Household out-of-pocket payments \nin US$ per capita'),x = unit(0,'npc')+unit(-4,'lines'),rot=90,gp = gpar(fontsize=10,fontface='bold',col = 'black'))
  grid.lines()
  grid.points(x=log(x=vgb$`Gross Domestic Product (GDP) per Capita in US$ in 2022`,base=2),
              y=log(x=vgb$`Household out-of-pocket payments (OOPS) in current US$ per capita in 2022`,base=2),
              default.units = 'native',pch=16,gp=gpar(alpha=0.7,cex = 0.8))
  #grid.text(paste0('r = ',round(cortest$estimate,2), ', 95% CI: (',round(cortest$conf.int[1],2),'–',round(cortest$conf.int[2],2),')'),y = unit(1,'npc')+unit(-2,'lines'),gp = gpar(fontsize=10,fontface='plain',col = 'black'))
  #popViewport()
  popViewport()
}
dev.off()
