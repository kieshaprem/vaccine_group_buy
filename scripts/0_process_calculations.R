library(rineq)

get_vaccine_price_value <- function(vaccine_prices){
  return(as.numeric(sapply(vaccine_prices,function(x){substr(x,3,nchar(x))})))
}

get_relative_var <- function(df, var){
  return((df[,var]-min(df[,var]))/(max(df[,var])-min(df[,var])))
}

get_adjusted_price <- function(relative_country_income,p_min,alpha){
  return(round(p_min*(1+alpha*relative_country_income),2))
}


process_mechanism <- function(df, mechanism, bilateral_df){
  df$total_perdose <- as.numeric(df$pop_covered*df$perdose)
  if (mechanism!='bilateral'){
    #df$total_savings_perdose <- df$pop_covered*df$savings_perdose
    df$total_savings_perdose <- as.numeric(bilateral_df$total_perdose-df$total_perdose)
  }
  df$percent_share <- as.numeric(df$total_perdose*100/sum(df$total_perdose))
  return(df)
}

round_up_millions <- function(x){
  return (ceiling(x/1e6)*1e6)
}

round_up_thousands <- function(x){
  return (ceiling(x/1e3)*1e3)
}

add_chepc <- function(df){
  new_df <- merge(df, main_sheet[,c('iso3c','chepc')],
                  by.x='iso3c',by.y='iso3c',all.x=T)
  new_df$perdose_chepc_percent <- new_df$perdose*100/new_df$chepc
  new_df <- new_df[order(new_df$gdp_percapita),]
  return(new_df)
}

add_affordability_percent <- function(df, perspective){
  affordability_var <- paste0('perdose_',perspective,'_percent')
  df[,affordability_var] <- df$perdose*100/df[,perspective]
  return(df)
}

# the 2 following functions assume that the function above has been run

get_spline <- function(df, perspective){
  df <- df[order(df[,perspective])]
  affordability_var <- paste0('perdose_',perspective,'_percent')
  return(spline(x=log(df[,perspective],base = 10), y = df[,affordability_var]),
         n=1000)
}

get_wci <- function(df, perspective){
  affordability_var <- paste0('perdose_',perspective,'_percent')
  return(ci(ineqvar = df[,perspective],
     outcome = df[,affordability_var],
     type = "CIw"))
}