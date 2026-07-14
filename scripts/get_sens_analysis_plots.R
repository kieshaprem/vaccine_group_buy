# get bar plot showing all uncertainties
# price per dose only (country wise)
# total expenditure (country wise)
# total expenditure (SEA)

source('scripts/psa_total_expenditure.R')
plot_coverage <- function(coverage){
  png(paste0('output/plots/final_paper/psa_total_exp_',coverage,".png"),
      height=10,width=10,res=300, units='cm',pointsize=10)
  #pushViewport(plotViewport(c(1,1,1,1)))
  total_expenditure_ranges(coverage)
  dev.off()
}

plot_coverage('full')
plot_coverage('real')
