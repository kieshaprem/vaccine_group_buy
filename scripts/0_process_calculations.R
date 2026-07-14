process_mechanism <- function(df, mechanism, bilateral_df){
  df$total_perdose <- df$pop_covered*df$perdose
  if (mechanism!='bilateral'){
    #df$total_savings_perdose <- df$pop_covered*df$savings_perdose
    df$total_savings_perdose <- bilateral_df$total_perdose-df$total_perdose
  }
  df$percent_share <- df$total_perdose*100/sum(df$total_perdose)
  return(df)
}

get_adjusted_price <- function(relative_country_income,p_min,alpha){
  return(round(p_min*(1+alpha*relative_country_income),2))
}

add_chepc <- function(df){
  new_df <- merge(df, main_sheet[,c('iso3c','chepc')],
                  by.x='iso3c',by.y='iso3c',all.x=T)
  new_df$perdose_chepc_percent <- new_df$perdose*100/new_df$chepc
  new_df <- new_df[order(new_df$gdp_percapita),]
  return(new_df)
}

add_affordability_percent <- function(df){
  df$perdose_gdppc_percent <- df$perdose*100/df$gdp_percapita
  df$perdose_chepc_percent <- df$perdose*100/df$chepc
  return(df)
}
