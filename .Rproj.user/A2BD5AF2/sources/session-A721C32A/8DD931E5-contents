

png(paste0("output/plots/scatter_actual_model.png"),height=12,width=12,units='cm',res=700,pointsize=10)
if(1)
{
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1,width=rep(1,1),height=rep(1,1))))
  
  pushViewport(viewport(layout.pos.row=1,layout.pos.col=1))
  #NEW(
  pushViewport(plotViewport(c(4,4,1,1),xscale=c(5,17),yscale=c(5,17)))#)NEW
  grid.rect()
  grid.yaxis()
  grid.xaxis()
  grid.text(paste0('WHO Global Data Costs in US$'),y = unit(0,'npc')+unit(-2.65,'lines'),gp = gpar(fontsize=12,fontface='bold',col = 'black'))
  grid.text(paste0('Modelled Costs in US$'),x = unit(0,'npc')+unit(-2.65,'lines'),rot=90,gp = gpar(fontsize=12,fontface='bold',col = 'black'))
  grid.lines()
  grid.points(x=hpv_internal$y,y=hpv_internal$y_pred,default.units = 'native',pch=16,gp=gpar(alpha=0.7,cex = 0.8))
  grid.text(paste0('r = ',round(cortest$estimate,2), ', 95% CI: (',round(cortest$conf.int[1],2),'–',round(cortest$conf.int[2],2),')'),y = unit(1,'npc')+unit(-2,'lines'),gp = gpar(fontsize=10,fontface='plain',col = 'black'))
  popViewport()
  popViewport()
}
dev.off()
