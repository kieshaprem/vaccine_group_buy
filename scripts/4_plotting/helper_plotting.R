
get_selected_grp_order <- function(data, group_col, desired_order){
  data$selected_grp_order <- character(nrow(data))
  for (i in 1:length(desired_order)){
    if (nrow(data[data[,group_col]==desired_order[i],]>0)){
      data[data[,group_col]==desired_order[i],]$selected_grp_order <- i
    }
  }
  return(data)
}

make_price_numeric <- function(data){
  data$Price.per.Dose.in.USD <- sapply(data$Price.per.Dose.in.USD,
                                       function(x){substr(x,3,nchar(x)-1)})
  data$Price.per.Dose.in.USD <- as.numeric(data$Price.per.Dose.in.USD)
  return(data)
}


# get 2 dp
get_2dp <- function(number){return(format(round(number,2),nsmall=2))}


order_of_magnitude <- function(x) {
  if (x == 0) return(0)
  
  magnitude <- floor(log10(abs(x)))
  return(10^magnitude)
}

get_formatted_number_millions <- function(x,others=NA){
  options(scipen=999)
  output_text <- paste0(x/1e6,'M')
  return (prettyNum(output_text, big.mark = ",", preserve.width = "none",nsmall=2))
}

get_formatted_number <- function(x,others=NA){
  options(scipen=999)
  order_magnitude <- order_of_magnitude(x)
  if (!is.na(others) & others == 'B'){
   x <- abs(x) 
  }
  if (order_magnitude>=1e9){
    output_text <- paste0(round(x/1e9),'B')
  } else if (order_magnitude>=1e6){
    output_text <- paste0(round(x/1e6),'M')
  } else if (order_magnitude>=1e6){
    output_text <- paste0(round(x/1e3),'K')
  } else {
    if (is.na(others)){
      output_text <- round(x,2)
    } else {
      output_text <- x
    }
  }
  return (prettyNum(output_text, big.mark = ",", preserve.width = "none",nsmall=2))
}