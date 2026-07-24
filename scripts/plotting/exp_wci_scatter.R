source('scripts/1_examine_psa_results.R')
library(grid)

exp_wci <- function(mechanism, psa_dataset, affordability_metric){
  total_exp_name <- paste0(mechanism,'_total')
  total_exp <- unlist(psa_dataset[total_exp_name,])
  
  wci_name <- paste0(mechanism,'_',affordability_metric,'_ci')
  wci_vals <- unlist(psa_dataset[as.character(wci_name),])
  pushViewport(plotViewport(c(1,3,1,1),xscale=c(-1,1),yscale=c(0,2*1e10)))
  grid.rect()
  
  grid.text(paste0('Total expenditure'),x = unit(0,'npc')+unit(-4.2,'lines'),rot=90,
              gp = gpar(fontsize=6,fontface='bold',col = 'black'))
  grid.points(y = total_exp, x = wci_vals, default.units = "native",
              gp=gpar(col="grey50", cex=0.4,alpha=0.7),pch=16)
  
  grid.yaxis(gp=gpar(fontsize=5))
  
  grid.xaxis(gp=gpar(fontsize=4.5))
  popViewport()
}

exp_wci(mechanism = 'bilateral', psa_dataset = alpha_psa_ci, affordability_metric = 'gdppc')
exp_wci(mechanism = 'bilateral', psa_dataset = alpha_psa_ci, affordability_metric = 'chepc')

exp_wci(mechanism = 'uniform', psa_dataset = alpha_psa_ci, affordability_metric = 'gdppc')
exp_wci(mechanism = 'uniform', psa_dataset = alpha_psa_ci, affordability_metric = 'chepc')

exp_wci(mechanism = 'adj_gdppc', psa_dataset = alpha_psa_ci, affordability_metric = 'gdppc')
exp_wci(mechanism = 'adj_gdppc', psa_dataset = alpha_psa_ci, affordability_metric = 'chepc')

exp_wci(mechanism = 'adj_chepc', psa_dataset = alpha_psa_ci, affordability_metric = 'gdppc')
exp_wci(mechanism = 'adj_chepc', psa_dataset = alpha_psa_ci, affordability_metric = 'chepc')
