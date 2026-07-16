library(grid)

# get 2 dp
get_2dp <- function(number){return(format(round(number,2),nsmall=2))}

# get basic box plot
get_box_plot <- function(data, group_col, val_col, y_axis_name, x_axis_name,
                         group_order_col, bar_colour, line_colour, x_factor, y_factor,
                         bilateral_flag){
  data <- data[order(data[,group_order_col]),]
  val_scale <- c(0,max(data[,val_col])+5)
  if (val_scale[2]>=50){
    val_scale[2] <- 100
  } else if (val_scale[2]<=20){
    val_scale[2] <- 20
  } else {
    val_scale[2] <- 50
  }
  if (bilateral_flag==TRUE){
    val_scale[2] <- 50
  }
  n_points <- table(data[,group_col])
  group_terms <- unique(data[,group_col])
  group_terms_wn <- sapply(group_terms, function(x){paste0(x, "\nn=",n_points[x])})
  x_scale_length <- length(group_terms)
  x_scale <- c(0.5,x_scale_length+1)
  
  
  # Calculate dynamic spacing and offsets
  group_spacing <- 1  # Distance between group centers
  box_width <- 0.2 * group_spacing  # Box width is 50% of spacing
  whisker_cap_width <- 0.15 * group_spacing  # Whisker cap width is 15% of spacing
  text_offset_x <- 0.5 * group_spacing  # Text offset from box center
  
  pushViewport(plotViewport(c(4,3,1,1),xscale=x_scale,yscale=val_scale))
  # X AXIS
  for (i in 1:x_scale_length){
    grid.text(group_terms_wn[i],x=i,y=-7.5*y_factor, default.units = 'native',
              gp=gpar(fontsize=8))
  }
  # Y AXIS
  #for (i in seq(0,40,10)){
   # grid.text(i,x=0.35,y=i, default.units = 'native',gp=gpar(fontsize=8))
  #}
  
  grid.yaxis(gp=gpar(fontsize=8,col='black'))
  #grid.xaxis(gp=gpar(fontsize=6,col='black'),at = c(0,seq(1:x_scale_length),x_scale_length+1),
             #label = c(" ",group_terms_wn," "))
  grid.text(y_axis_name
            ,gp=gpar(fontsize=8,fontface='bold'),
            x = unit(0,'npc')+unit(-3,'lines'),
            rot=90)
  grid.text(x_axis_name
            ,gp=gpar(fontsize=8,fontface='bold'),
            y = unit(0,'npc')+unit(26,'lines'))
  for(i in 1:x_scale_length){
    grp <- group_terms[i]
    sub_data <- data[data[,group_col]==grp,val_col]
    min_val <- min(sub_data)
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
    if (length(sub_data)!=1){
      if (min_val==max_val){
        grid.text(paste0("Min, Median, \nMax: ",get_2dp(min_val)),
                  x=i+text_offset_x,y=min_val, 
                  default.units = 'native',gp=gpar(fontsize=6,fontface='italic'))
      } else if (min_val==median_val){
        grid.text(paste0("Min, Median:\n",get_2dp(min_val)),
                  x=i+text_offset_x,y=min_val-y_factor, 
                  default.units = 'native',gp=gpar(fontsize=6,fontface='italic'))
        grid.text(paste0("Max: ",get_2dp(max_val)),
                  x=i+text_offset_x,y=max_val+y_factor, 
                  default.units = 'native',gp=gpar(fontsize=6,fontface='italic'))
      } else if (max_val==median_val){
        grid.text(paste0("Median, Max:\n",get_2dp(max_val)),
                  x=i+text_offset_x,y=max_val+y_factor, 
                  default.units = 'native',gp=gpar(fontsize=6,fontface='italic'))
        grid.text(paste0("Min: ",get_2dp(min_val)),
                  x=i+text_offset_x,y=min_val-y_factor, 
                  default.units = 'native',gp=gpar(fontsize=6,fontface='italic'))
      } else{
        grid.text(paste0("Min: ",get_2dp(min_val)),
                  x=i+text_offset_x,y=min_val-y_factor, 
                  default.units = 'native',gp=gpar(fontsize=6,fontface='italic'))
        grid.text(paste0("Max: ",get_2dp(max_val)),
                  x=i+text_offset_x,y=max_val+y_factor, 
                  default.units = 'native',gp=gpar(fontsize=6,fontface='italic'))
      }
    }
      grid.polygon(y = c(first_q,third_q,
                         rev(c(first_q,third_q))),
                   x = c(i-box_width,i-box_width,i+box_width,i+box_width),
                   default.units = 'native',
                   gp = gpar(col = bar_colour, fill = bar_colour))
      grid.lines(y=c(median_val,median_val),
                 x = c(i-box_width,i+box_width),
                 default.units = 'native',gp=gpar(col='black',lwd=1))
      if (min_val!=median_val & max_val!=median_val){
        grid.text(paste0("Median: ",get_2dp(median_val)),x=i+text_offset_x*0.95,y=median_val, 
                  default.units = 'native',gp=gpar(fontsize=6,fontface='italic'))
      }
      if(length(sub_data)==1){
        grid.text(paste0("Min, Median\nMax: ",get_2dp(median_val)),x=i+text_offset_x,y=median_val, 
                  default.units = 'native',gp=gpar(fontsize=6,fontface='italic'))
    }
    }
  popViewport()
}
