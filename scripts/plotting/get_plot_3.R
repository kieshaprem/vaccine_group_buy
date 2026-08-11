source('scripts/plotting/waterfall_plot_only.R')
source('scripts/plotting/5_unaffordability_index_plot.R')
library(countrycode)
library(grid)
library(rineq)

png(paste0('output/plots/final_paper/equity/noother_fullpage_',coverage,'.png'),
    height=18,width=18,res=600, units='cm',pointsize=8)
grid.newpage()
pushViewport(plotViewport(c(1,1,1,1)))
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1,
                                           width = c(rep(1,2)),
                                           height =c(rep(1,2)))))

pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
pushViewport(plotViewport(c(2,4,1,2)))
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 4,
                                           width = c(rep(1,4)),
                                           height =c(rep(1,4)))))
#grid.rect()
if(1)
{
  bilateral <- bilateral[order(bilateral[,'gdppc']),]
  uniform <- uniform[order(uniform[,'gdppc']),]
  adj_gdppc <- adj_gdppc[order(adj_gdppc[,'gdppc']),]
  adj_chepc <- adj_chepc[order(adj_chepc[,'gdppc']),]
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
  grid.text('Price per dose (in US$)',
            x = unit(-2,'lines'), rot = 90, gp = gpar(fontsize = 6, fontface = 'plain'))
  
  grid.text(paste0('$'),
            x = unit(7.7,'lines'),y=unit(18.8,'lines'),gp = gpar(fontsize=4.5))
  
  grid.text(paste0('% of\nGDP per capita'),x = unit(10.9,'lines'),
            y = unit(18.1,'lines'),gp = gpar(fontsize=4.5))
  
  waterfall_plot_only(mechanism = 'bilateral', dataset = bilateral,
                      affordability_metric = 'perdose_gdppc_percent')
  popViewport()

  
  pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1))
  # grid.rect()
  waterfall_plot_only(mechanism = 'uniform', dataset = uniform,
                      affordability_metric = 'perdose_gdppc_percent')
  popViewport()
  pushViewport(viewport(layout.pos.col = 3, layout.pos.row = 1))
  # grid.rect()
  waterfall_plot_only(mechanism = 'gdp_adj', dataset = adj_gdppc,
                      affordability_metric = 'perdose_gdppc_percent')
  popViewport()
  
  pushViewport(viewport(layout.pos.col = 4, layout.pos.row = 1))
  # grid.rect()
  waterfall_plot_only(mechanism = 'chepc_adj', dataset = adj_chepc,
                      affordability_metric = 'perdose_gdppc_percent')
  popViewport()
}

if(1)
{
  ci_val_bilateral <- get_ci(df = bilateral,
                             perspective = 'gdppc')
  ci_val_uniform <- get_ci(df = uniform,
                           perspective = 'gdppc')
  ci_val_adj_gdppc <- get_ci(df = adj_gdppc,
                             perspective = 'gdppc')
  ci_val_adj_chepc <- get_ci(df = adj_chepc,
                             perspective = 'gdppc')
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
  # grid.rect()
  unaffordability_plot(mechanism = "bilateral", 
                      spline_df_bilateral = spline_gdppc_percent_bilateral, 
                      spline_df_main = spline_gdppc_percent_bilateral,
                      CI_val_bilateral = ci_val_bilateral,
                      CI_val = ci_val_bilateral,
                      affordability_metric = 'perdose_gdppc_percent')


  popViewport()
  
  pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 2))
  # grid.rect()
  
  
  unaffordability_plot(mechanism = "uniform", 
                       spline_df_bilateral = spline_gdppc_percent_bilateral, 
                       spline_df_main = spline_gdppc_percent_uniform,
                       CI_val_bilateral = ci_val_bilateral,
                       CI_val = ci_val_uniform,
                       affordability_metric = 'perdose_gdppc_percent')
  popViewport()
  pushViewport(viewport(layout.pos.col = 3, layout.pos.row = 2))
  # grid.rect()
  unaffordability_plot(mechanism = "gdp_adj", 
                       spline_df_bilateral = spline_gdppc_percent_bilateral, 
                       spline_df_main = spline_gdppc_percent_gdppc_adj,
                       CI_val_bilateral = ci_val_bilateral,
                       CI_val = ci_val_adj_gdppc,
                       affordability_metric = 'perdose_gdppc_percent')
  popViewport()
  
  pushViewport(viewport(layout.pos.col = 4, layout.pos.row = 2))
  # grid.rect()
  unaffordability_plot(mechanism = "chepc_adj", 
                       spline_df_bilateral = spline_gdppc_percent_bilateral, 
                       spline_df_main = spline_gdppc_percent_chepc_adj,
                       CI_val_bilateral = ci_val_bilateral,
                       CI_val = ci_val_adj_chepc,
                       affordability_metric = 'perdose_gdppc_percent')
  
  popViewport()
  
}
popViewport()
popViewport()
popViewport()
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
#grid.rect()

pushViewport(plotViewport(c(2,4,1,2)))
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 4,
                                           width = c(rep(1,4)),
                                           height =c(rep(1,4)))))

if(1)
{ bilateral <- bilateral[order(bilateral[,'chepc']),]
uniform <- uniform[order(uniform[,'chepc']),]
adj_gdppc <- adj_gdppc[order(adj_gdppc[,'chepc']),]
adj_chepc <- adj_chepc[order(adj_chepc[,'chepc']),]

  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
  # grid.rect()
  grid.text('Price per dose (in US$)',
            x = unit(-2,'lines'), rot = 90, gp = gpar(fontsize = 6, fontface = 'plain'))
  grid.text(paste0('$'),x = unit(0,'npc')+unit(7.7,'lines'),
            y = unit(0,'npc')+unit(18.8,'lines'),gp = gpar(fontsize=4.5))
  
  grid.text(paste0('% of\nCHE per capita'),x = unit(0,'npc')+unit(10.9,'lines'),
            y = unit(0,'npc')+unit(18.1,'lines'),gp = gpar(fontsize=4.5))
  
   waterfall_plot_only(mechanism = 'bilateral', dataset = bilateral,
                      affordability_metric = 'perdose_chepc_percent')
  popViewport()
  
  pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1))
  # grid.rect()
  waterfall_plot_only(mechanism = 'uniform', dataset = uniform,
                      affordability_metric = 'perdose_chepc_percent')
  popViewport()
  pushViewport(viewport(layout.pos.col = 3, layout.pos.row = 1))
  # grid.rect()
  waterfall_plot_only(mechanism = 'adj_gdp', dataset = adj_gdppc,
                      affordability_metric = 'perdose_chepc_percent')
  popViewport()
  
  pushViewport(viewport(layout.pos.col = 4, layout.pos.row = 1))
  # grid.rect()
  waterfall_plot_only(mechanism = 'adj_chepc', dataset = adj_chepc,
                      affordability_metric = 'perdose_chepc_percent')
  popViewport()
  
}

if(1)
{

ci_val_bilateral <- get_ci(df = bilateral,
                           perspective = 'chepc')
ci_val_uniform <- get_ci(df = uniform,
                         perspective = 'chepc')
ci_val_adj_gdppc <- get_ci(df = adj_gdppc,
                           perspective = 'chepc')
ci_val_adj_chepc <- get_ci(df = adj_chepc,
                           perspective = 'chepc')

pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
# grid.rect()
unaffordability_plot(mechanism = "bilateral", 
                     spline_df_bilateral = spline_chepc_percent_bilateral, 
                     spline_df_main = spline_chepc_percent_bilateral,
                     CI_val_bilateral = ci_val_bilateral,
                     CI_val = ci_val_bilateral,
                     affordability_metric = 'perdose_chepc_percent')


popViewport()

pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 2))
# grid.rect()


unaffordability_plot(mechanism = "uniform", 
                     spline_df_bilateral = spline_chepc_percent_bilateral, 
                     spline_df_main = spline_chepc_percent_uniform,
                     CI_val_bilateral = ci_val_bilateral,
                     CI_val = ci_val_uniform,
                     affordability_metric = 'perdose_chepc_percent')
popViewport()
pushViewport(viewport(layout.pos.col = 3, layout.pos.row = 2))
# grid.rect()
unaffordability_plot(mechanism = "gdp_adj", 
                     spline_df_bilateral = spline_chepc_percent_bilateral, 
                     spline_df_main = spline_chepc_percent_gdppc_adj,
                     CI_val_bilateral = ci_val_bilateral,
                     CI_val = ci_val_adj_gdppc,
                     affordability_metric = 'perdose_chepc_percent')
popViewport()

pushViewport(viewport(layout.pos.col = 4, layout.pos.row = 2))
# grid.rect()
unaffordability_plot(mechanism = "chepc_adj", 
                     spline_df_bilateral = spline_chepc_percent_bilateral, 
                     spline_df_main = spline_chepc_percent_chepc_adj,
                     CI_val_bilateral = ci_val_bilateral,
                     CI_val = ci_val_adj_chepc,
                     affordability_metric = 'perdose_chepc_percent')

popViewport()

}
popViewport()
popViewport()
popViewport()
popViewport()
popViewport()

#pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
grid.text('Gross Domestic Product per capita',
          x = unit(0.4, 'cm'), y = unit(0.75, 'npc'),
          rot = 90, gp = gpar(fontsize = 8, fontface = 'bold'))
#popViewport()
grid.text('GDP per capita in 2023, log10 scale',
           y = unit(28, 'lines'),
          gp = gpar(fontsize = 8, fontface = 'bold'))

#pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
grid.text('Current Health Expenditure per capita',
          x = unit(0.4, 'cm'), y = unit(0.25, 'npc'),
          rot = 90, gp = gpar(fontsize = 8, fontface = 'bold'))
#popViewport()
grid.text('CHE per capita in 2023, log10 scale',
          y = unit(2, 'lines'),
          gp = gpar(fontsize = 8, fontface = 'bold'))

grid.text('Price as % of GDPPC (in US$ 2023)',
          x = unit(68,'lines'), y = unit(53,'lines'),rot = 270, gp = gpar(fontsize = 6, fontface = 'plain'))


grid.text('Price as % of CHEPC (in US$ 2023)',
          x = unit(68,'lines'), y = unit(21,'lines'),rot = 270, gp = gpar(fontsize = 6, fontface = 'plain'))

grid.text('a) Bilateral price',
          x = unit(10,'lines'), y = unit(68,'lines'),gp = gpar(fontsize = 6, fontface = 'bold'))
grid.text('b) Fixed price',
          x = unit(24,'lines'), y = unit(68,'lines'),gp = gpar(fontsize = 6, fontface = 'bold'))
grid.text('c) Tiered price (GDPPC-adjusted)',
          x = unit(43,'lines'), y = unit(68,'lines'),gp = gpar(fontsize = 6, fontface = 'bold'))
grid.text('d) Tiered price (CHEPC-adjusted)',
          x = unit(58,'lines'), y = unit(68,'lines'),gp = gpar(fontsize = 6, fontface = 'bold'))


grid.text('i) Bilateral price',
          x= unit(10,'lines'), y = unit(34,'lines'),gp = gpar(fontsize = 6, fontface = 'bold'))
grid.text('j) Fixed price',
          x = unit(24,'lines'), y = unit(34,'lines'),gp = gpar(fontsize = 6, fontface = 'bold'))
grid.text('k) Tiered price (GDPPC-adjusted)',
          x = unit(43,'lines'), y = unit(34,'lines'),gp = gpar(fontsize = 6, fontface = 'bold'))
grid.text('l) Tiered price (CHEPC-adjusted)',
          x = unit(58,'lines'), y = unit(34,'lines'),gp = gpar(fontsize = 6, fontface = 'bold'))

#popViewport()

#popViewport()

dev.off()