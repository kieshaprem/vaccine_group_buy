library(grid)
source('scripts/plotting/general_helper_plotting_functions.R')

# get basic box plot
get_box_plot <- function(data, group_col, val_col, y_axis_name, x_axis_name,
                         group_order_col, bar_colour, line_colour, y_factor, 
                         n_flag, plot_name){
  data <- data[order(data[,group_order_col]),]
  order_magnitude <- order_of_magnitude(max(data[,val_col]))
  val_scale <- c(0,ceiling(max(data[,val_col])/order_magnitude)*order_magnitude)
  
  if (n_flag==TRUE){
    val_scale <- c(0,100)
  }

  n_points <- table(data[,group_col])
  group_terms <- unique(data[,group_col])
  if (n_flag==T){
    group_terms_wn <- sapply(group_terms, function(x){paste0(x, "\nn=",n_points[x])})
  } else {
    group_terms_wn <- group_terms
  }
  x_scale_length <- length(group_terms)
  x_scale <- c(0.5,x_scale_length+0.5)
  
  
  # Calculate dynamic spacing and offsets
  group_spacing <- 1  # Distance between group centers
  box_width <- 0.2 * group_spacing  # Box width is 50% of spacing
  whisker_cap_width <- 0.15 * group_spacing  # Whisker cap width is 15% of spacing
  text_offset_x <- 0.425 * group_spacing  # Text offset from box center
  
  cat(val_scale[2])
  pushViewport(plotViewport(c(2,4,1,1),xscale=x_scale,yscale=val_scale))
  # X AXIS
  for (i in 1:x_scale_length){
    grid.text(group_terms_wn[i],x=i,y=-7.5*y_factor, default.units = 'native',
              gp=gpar(fontsize=8))
  }
  y_axis_locations <- axisTicks(val_scale,log=FALSE)
  y_axis_labels <- sapply(y_axis_locations,function(x){get_formatted_number(x)})

  #grid.xaxis(gp=gpar(fontsize=6,col='black'),at = c(0,seq(1:x_scale_length),x_scale_length+1),
             #label = c(" ",group_terms_wn," "))
  # Y AXIS
  #if (!plot_name %in% c('b) UMIC','c) HIC')){
    grid.yaxis(at = y_axis_locations, label = y_axis_labels, gp=gpar(fontsize=8,col='black'))
  #}
  # for (i in 1:length(y_axis_locations)){
  #  grid.text(y_axis_labels[i],x=0.35,y=y_axis_locations[i], default.units = 'native',gp=gpar(fontsize=8))
  # }
  #grid.yaxis(gp=gpar(fontsize=8))
  print(y_axis_name)
  grid.text(y_axis_name
            ,gp=gpar(fontsize=8,fontface='bold'),
            ,x = unit(0,'npc')+unit(-5,'lines'),
            y = unit(0,'npc')+unit(11,'lines'),rot =90)
  grid.text(x_axis_name
            ,gp=gpar(fontsize=8,fontface='bold'),
            y = unit(0,'npc')+unit(-3,'lines'))
  for(i in 1:x_scale_length){
    grp <- group_terms[i]
    sub_data <- data[data[,group_col]==grp,val_col]
    min_val <- min(sub_data)
    median_val <- median(sub_data)
    max_val <- max(sub_data)
    first_q <- quantile(sub_data,probs = 0.25)
    third_q <- quantile(sub_data,probs = 0.75)
    iqr_range <- third_q-first_q
    upper_whisker <- third_q + 1.5*iqr_range
    lower_whisker <- first_q - 1.5*iqr_range
    outliers <- sub_data[sub_data < lower_whisker | sub_data > upper_whisker]
    if (max_val<upper_whisker){
      upper_whisker <- max_val
    }
    if (min_val>lower_whisker){
      lower_whisker <- min_val
    }
    median_val <- median(sub_data)
    # tails
    grid.lines(y=c(lower_whisker,upper_whisker),
               x = c(i,i),default.units = 'native',gp=gpar(col=line_colour,lwd=1))
    grid.lines(y=c(lower_whisker,lower_whisker),
               x = c(i-(whisker_cap_width),i+(whisker_cap_width)),
                     default.units = 'native',gp=gpar(col='black',lwd=1))
    grid.lines(y=c(upper_whisker,upper_whisker),
               x = c(i-(whisker_cap_width),i+(whisker_cap_width)),default.units = 'native',gp=gpar(col='black',lwd=1))
    if (length(outliers)>0){
      grid.points(x=rep(i,length(outliers)),y=outliers,default.units='native',
                 gp=gpar(col='black',lwd=1,cex=0.4),pch = 16)
    }
    grid.text(get_formatted_number(median_val), x = i + text_offset_x,
              y = median_val, default.units = 'native',
              gp = gpar(fontsize = 8, fontface = 'italic'))

      grid.polygon(y = c(first_q,third_q,
                         rev(c(first_q,third_q))),
                   x = c(i-box_width,i-box_width,i+box_width,i+box_width),
                   default.units = 'native',
                   gp = gpar(col = bar_colour, fill = bar_colour))
      grid.lines(y=c(median_val,median_val),
                 x = c(i-box_width,i+box_width),
                 default.units = 'native',gp=gpar(col='black',lwd=1))

  }
  if (!is.na(plot_name)){
    grid.text(plot_name,x = unit(0,'npc')+unit(3,'lines'),
              y = unit(0,'npc')+unit(25,'lines'),
              gp = gpar(fontsize=8,fontface='bold'))
  }
  popViewport()
}
