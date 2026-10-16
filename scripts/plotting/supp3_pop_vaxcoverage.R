# The following codes are for creating Supplementary Figure 3 
#
# ------------------------------------------------------------------------------
# Read in data for plotting
# ------------------------------------------------------------------------------
library(countrycode)
source('scripts/plotting/general_helper_plotting_functions.R')
load('output/intermediate/main_data.rdata')

data = main_data[,c('iso3c','population_size','coverage_covid_1dose_percent')]
data$pop_covered <- round(data$population_size*
                            as.numeric(data$coverage_covid_1dose_percent)/100)
data$coverage_covid_1dose_percent <- as.numeric(data$coverage_covid_1dose_percent)
data$population_size_log <- log(data$population_size,base=10)
data$population_covered_log <- log(data$pop_covered,base=10)
data$iso2c <- countrycode(data$iso3c,origin = 'iso3c',destination = 'iso2c')

val_col <- 'population_size_log'
group_col <- 'iso2c'
val_stacked <- 'population_covered_log'
point_col <- 'coverage_covid_1dose_percent'

order_magnitude <- order_of_magnitude(max(data[,val_col]))
val_scale <- c(4,ceiling(max(data[,val_col])/order_magnitude)*order_magnitude)

#n_points <- table(data[,group_col])
group_terms <- unique(data[,group_col])
x_scale_length <- length(group_terms)
x_scale <- c(0.5,x_scale_length+0.5)

y_axis_name <- 'Population (log10 scale)'
secondary_y_name <- 'Vaccination coverage (%)'

plot_name <- 'Total population vs Population covered by vaccination'

# Calculate dynamic spacing and offsets
group_spacing <- 1  # Distance between group centers
box_width <- 0.4 * group_spacing  # Box width is 50% of spacing
whisker_cap_width <- 0.15 * group_spacing  # Whisker cap width is 15% of spacing
text_offset_x <- 0.425 * group_spacing  # Text offset from box center


library(grid)

png(paste0('output/plots/supp3_pop_covered.png'),
  height=10,width=10,res=300, units='cm',pointsize=10)
grid.newpage()
pushViewport(plotViewport(c(2,5,2,4),xscale=x_scale,yscale=val_scale))

# Axes locations and labels
y_axis_locations <- axisTicks(val_scale,log=FALSE)
y_axis_labels <- sapply(y_axis_locations,
                      function(x){get_formatted_number_millions(10^x)})

secondary_val_scale <- c(50,ceiling(max(data[,point_col]))+3)
secondary_y_axis <- axisTicks(secondary_val_scale,log = FALSE)
secondary_y_labels <- sapply(secondary_y_axis, function(x){paste0(x,'%')})

# Obtaining transformation from primary to secondary axes
scaling <- (val_scale[2]-val_scale[1])/
(secondary_val_scale[2]-secondary_val_scale[1])
intercept <- 4-50*scaling

# Draw primary and secondary y-axes
grid.yaxis(at = y_axis_locations, label = y_axis_labels, 
         gp=gpar(fontsize=8,col='black'))
grid.yaxis(at = secondary_y_axis[-7]*scaling+intercept, 
         label = secondary_y_labels[-7], 
         gp=gpar(fontsize=8,col='black'),main = F)
grid.rect()

# Write axes labels
grid.text(y_axis_name
        ,gp=gpar(fontsize=10,fontface='bold'),
        ,x = unit(0,'npc')+unit(-4,'lines'),
        y = unit(0,'npc')+unit(11,'lines'),rot =90)

grid.text(secondary_y_name
        ,gp=gpar(fontsize=10,fontface='bold'),
        ,x = unit(0,'npc')+unit(17.8,'lines'),
        y = unit(0,'npc')+unit(11,'lines'),rot =270)

# Draw bar chart
for(i in 1:x_scale_length){
grp <- group_terms[i]
grid.polygon(y = c(4,data[i,val_col],
                   rev(c(4,data[i,val_col]))),
             x = c(i-box_width,i-box_width,i+box_width,i+box_width),
             default.units = 'native',
             gp = gpar(col = 'grey60', fill = 'grey60'))

grid.polygon(y = c(4,data[i,val_stacked],
                   rev(c(4,data[i,val_stacked]))),
             x = c(i-box_width,i-box_width,i+box_width,i+box_width),
             default.units = 'native',
             gp = gpar(col = 'grey90', fill = 'grey90'))
grid.text(paste0(data[i,point_col],'%'),
            x = i, y = data[i,point_col]*scaling+intercept-0.13,
            default.units = 'native',
            gp = gpar(col = 'black', fontsize=6.5))
}
grid.points(y = (data[,point_col]*scaling+intercept),
           x = 1:x_scale_length,
           default.units = 'native',
           gp = gpar(col = 'black', fill = 'black',cex=0.5),pch=16)
if (!is.na(plot_name)){
  grid.text(plot_name,x = unit(0,'npc')+unit(7,'lines'),
            y = unit(0,'npc')+unit(21,'lines'),
            gp = gpar(fontsize=10,fontface='bold'))
}

# Draw x axis
grid.xaxis(label = group_terms,at = c(1:x_scale_length),
                                    gp=gpar(fontsize=8))

# Draw legend boxes and their labels
grid.polygon(y = c(8.8,8.9,8.9,8.8),
             x = c(1-0.750*box_width,1-0.750*box_width,
                   1-0.250*box_width,1-0.250*box_width),
             default.units = 'native',
             gp = gpar(col = 'grey60', fill = 'grey60'))
grid.text('Total population in 2023 (World Bank)',
          ,x = unit(0,'npc')+unit(8.2,'lines'),
          y = unit(0,'npc')+unit(34.7,'lines'),
          gp = gpar(fontsize=5.5))

grid.polygon(y = c(8.6,8.7,8.7,8.6),
             x = c(1-0.750*box_width,1-0.750*box_width,
                   1-0.250*box_width,1-0.250*box_width),
             default.units = 'native',
             gp = gpar(col = 'grey90', fill = 'grey90'))
grid.text('Population covered by vaccine',
        x = unit(0,'npc')+unit(7,'lines'),
          y = unit(0,'npc')+unit(33.2,'lines'),
          gp = gpar(fontsize=5.5))

grid.points(y = c(8.45),
             x = c(1-0.5*box_width),
             default.units = 'native',
             gp = gpar(col = 'black', fill = 'black',cex=0.5),pch=16)

grid.text(
'% of total population covered by\nat least 1 dose of COVID-19 vaccine',
          x = unit(0,'npc')+unit(7.1,'lines'),
          y = unit(0,'npc')+unit(31.3,'lines'),
          gp = gpar(fontsize=5.5))


grid.rect(gp=gpar(fill=NA))

popViewport()
dev.off()
