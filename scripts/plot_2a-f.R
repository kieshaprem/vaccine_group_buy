library(grid)
source('scripts/6-3_waterfall_per_dose.R')
source('scripts/6-4_waterfall_total_per_dose.R')
source('scripts/plot_sens_analysis.R')
source('scripts/total_expenditure.R')

paneller=function(row,mechanism,coverage_type)
{
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=row))
  if (mechanism=="uniform"){
    if (row==1){
      waterfall_volume_per_dose("uniform",coverage=coverage_type)
    }
    if (row==2){
      waterfall_volume_total_per_dose("uniform",coverage=coverage_type)
    }
  }
  if (mechanism=="gdp_adj"){
    if (row==1){
      waterfall_volume_per_dose("gdp_adj",coverage=coverage_type)
    }
    if (row==2){
      waterfall_volume_total_per_dose("gdp_adj",coverage=coverage_type)
    }
  }
  
  if (mechanism=="sens_analysis"){
    if (row==1){
      percent_share(coverage=coverage_type)
    }
    if (row==2){
      total_expenditure(coverage_type)
    }
  }
  
  popViewport()
  #print(paste0("Column",column))
  #print(paste0("Row",row))
}

plot_coverage <- function(coverage){
  png(paste0('output/plots/final_paper/2a-f_updated2_',coverage,'.png'),
             height=10,width=15,res=300, units='cm',pointsize=10)
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3)))
  for (column in (1:3)){
    print(paste0("Column",column))
    pushViewport(viewport(layout.pos.col=column,layout.pos.row=1))
    if (column==1){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller(row=1, mechanism = "uniform",coverage_type=coverage)
      paneller(row=2, mechanism = "uniform",coverage_type=coverage)
      popViewport()
      popViewport()
    }
    if (column==2){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller(row=1, mechanism = "gdp_adj",coverage_type=coverage)
      paneller(row=2, mechanism = "gdp_adj",coverage_type=coverage)
      popViewport()
      popViewport()
    }
    if (column==3){
      pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1)))
      paneller(row=1, mechanism = "sens_analysis",coverage_type = coverage)
      paneller(row=2, mechanism = "sens_analysis",coverage_type = coverage)
      popViewport()
      popViewport()
    }
  }  
  popViewport()
  popViewport()
  dev.off()
}

plot_coverage('full')
plot_coverage('partial')

bilateral_full <- bilateral_full[order(bilateral_full$gdp_percapita),]
uniform_full <- uniform_full[order(uniform_full$gdp_percapita),]
adj_full_gdppc <- adj_full_gdppc[order(adj_full_gdppc$gdp_percapita),]

#bilateral_partial <- bilateral_partial[order(main_sheet$gdp_percapita),]
#uniform_partial <- uniform_partial[order(main_sheet$gdp_percapita),]
#adj_partial_gdppc <- adj_partial_gdppc[order(main_sheet$gdp_percapita),]

bilateral_full$share <- bilateral_full_analysis_price$percent_share_median
uniform_full$share <- uniform_full_analysis_price$percent_share_median
adj_full_gdppc$share <- gdp_adj_full_analysis_price$percent_share_median

bilateral_partial$share <- bilateral_partial_analysis_price$percent_share_median
uniform_partial$share <- uniform_partial_analysis_price$percent_share_median
adj_partial_gdppc$share <- gdp_adj_partial_analysis_price$percent_share_median

library(writexl)
write_xlsx(list(bilateral_full=bilateral_full,
                bilateral_partial=bilateral_partial,
                uniform_full=uniform_full,
                uniform_partial=uniform_partial,
                gdp_adj_full=adj_full_gdppc,
                gdp_adj_partial=adj_partial_gdppc),path = 'output/modelling_data_update2.xlsx')

share_comparison <- data.frame(cbind('Country_Code'=bilateral_full$iso3c,
                                     'Bilateral_Full_Coverage'=round(bilateral_full$share,2),
                          'Bilateral_Real_Coverage'=round(bilateral_partial$share,2),
                          'Uniform_Full_Coverage'=round(uniform_full$share,2),
                          'Uniform_Real_Coverage'=round(uniform_partial$share,2),
                          'GDP_Adjusted_Full_Coverage'=round(adj_full_gdppc$share,2),
                          'GDP_Adjusted_Real_Coverage'=round(adj_partial_gdppc$share,2)))
write_xlsx(share_comparison,path = 'output/share_comparison.xlsx')
