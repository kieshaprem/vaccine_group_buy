# Helper script that runs additional data needed for Main paper plot 2
source('scripts/4_plotting/mainpaper_plot2_helper.R')

# Scripts for plotting functions relevant to each panel
# Function name: watervolume_volume_per_dose
source('scripts/4_plotting/mainpaper_plot2a-c.R') 
# Function name: watervolume_volume_total_per_dose
source('scripts/4_plotting/mainpaper_plot2d-f.R')
# Function name: percent_share
source('scripts/4_plotting/mainpaper_plot2g.R')
# Function name: get_exp_across
source('scripts/4_plotting/mainpaper_plot2h.R')

# Create plot
library(grid)
png(paste0('output/plots/2ag_full.png'),
    height=18,width=18,res=600, units='cm',pointsize=10)
grid.newpage()
pushViewport(plotViewport(c(1,1,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=3)))

# 1st column
pushViewport(viewport(layout.pos.col=1,layout.pos.row=1:3))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=1)))
# 2a
pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
waterfall_volume_per_dose(mechanism = "uniform")
popViewport()

# 2d
pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
waterfall_volume_total_per_dose(mechanism = "uniform")
popViewport()

# 2g
pushViewport(viewport(layout.pos.col=1,layout.pos.row=3))
percent_share()
popViewport()

popViewport()
popViewport()

# 2nd column
pushViewport(viewport(layout.pos.col=2,layout.pos.row=1:3))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=1)))
# 2b
pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
waterfall_volume_per_dose(mechanism = 'gdp_adj')
popViewport()

# 2e
pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
waterfall_volume_total_per_dose(mechanism = 'gdp_adj')
popViewport()

# 2h
pushViewport(viewport(layout.pos.col=1,layout.pos.row=3))
get_exp_across(total_exp_df)
popViewport()

popViewport()
popViewport()

# 3rd column
pushViewport(viewport(layout.pos.col=3,layout.pos.row=1:3))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=1)))
# 2c
pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
waterfall_volume_per_dose(mechanism = 'chepc_adj')
popViewport()

# 2f
pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
waterfall_volume_total_per_dose(mechanism = 'chepc_adj')
popViewport()

popViewport()
popViewport()

popViewport()
popViewport()
dev.off()