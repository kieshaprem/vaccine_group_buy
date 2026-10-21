# ------------------------------------------------------------------------------
# The script consists of functions used to aid calculations of expenditure, savings
# and concentration index at the individual country level & regional level, 
# along with other useful functions
# ------------------------------------------------------------------------------

library(rineq)

# get vaccine price value from a string like "$X"
get_vaccine_price_value <- function(vaccine_prices){
  return(as.numeric(sapply(vaccine_prices,function(x){substr(x,3,nchar(x))})))
}

# calculate adjustment factor relative to each country's GDP or CHE per capita
get_relative_var <- function(df, var){
  return((df[,var]-min(df[,var]))/(max(df[,var])-min(df[,var])))
}

# get tiered pricing value
get_adjusted_price <- function(relative_country_income,p_min,alpha){
  return(round(p_min*(1+alpha*relative_country_income),2))
}

# calculate savings per dose; used for plots
get_savings_per_dose <- function(alternate_pricing_df, bilateral_pricing_df){
  alternate_pricing_df$savings_perdose <- bilateral_pricing_df$perdose - alternate_pricing_df$perdose
  return(alternate_pricing_df)
}

# run calculations for expenditure & savings of the region, 
# and % share of each country
process_mechanism <- function(df, mechanism, bilateral_df=NA){
  df$total_perdose <- as.numeric(df$pop_covered*df$perdose)
  df$total_perdose <- round_up_thousands(df$total_perdose)
  if (mechanism!='bilateral' & is.data.frame(bilateral_df)){
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

# add CHE per capita variable and % affordability (burden of vaccine price per dose)
# according to CHE per capita, in case the variable is missing
add_chepc <- function(df){
  new_df <- merge(df, main_sheet[,c('iso3c','chepc')],
                  by.x='iso3c',by.y='iso3c',all.x=T)
  new_df$perdose_chepc_percent <- new_df$perdose*100/new_df$chepc
  new_df <- new_df[order(new_df$gdp_percapita),]
  return(new_df)
}

# calculate % affordability (burden of vaccine price per dose) according to selected
# perspective (GDP per capita or CHE per capita), used in plots
add_affordability_percent <- function(df, perspective){
  affordability_var <- paste0('perdose_',perspective,'_percent')
  df[,affordability_var] <- df$perdose*100/df[,perspective]
  return(df)
}

# the 2 following functions assume that the function above has been run

# get smooth spline from df and perspective 
# (price per dose as % of GDP per capita or CHE per capita)
get_spline <- function(df, perspective){
  df <- df[order(df[,perspective])]
  affordability_var <- paste0('perdose_',perspective,'_percent')
  return(spline(x=log(df[,perspective],base = 10), y = df[,affordability_var]),
         n=1000)
}

# get Wagstaff concentration index (CI) for affordability (burden of vaccine 
# price per dose) accordingto selected perspective (GDP per capita or CHE per 
# capita) across the region
get_wci <- function(df, perspective){
  df <- add_affordability_percent(df =df, perspective = perspective)
  affordability_var <- paste0('perdose_',perspective,'_percent')
  return(ci(ineqvar = df[,perspective],
     outcome = df[,affordability_var],
     type = "CIw"))
}

# get relative CI for affordability (burden of vaccine price per dose) according
# to selected perspective (GDP per capita or CHE per capita) across the region
get_ci <- function(df, perspective){
  df <- add_affordability_percent(df =df, perspective = perspective)
  affordability_var <- paste0('perdose_',perspective,'_percent')
  return(round(ci(ineqvar = df[,perspective],
            outcome = df[,affordability_var],
            type = "CI")$concentration_index,3))
}

# get general CI for affordability (burden of vaccine price per dose) according
# to selected perspective (GDP per capita or CHE per capita) across the region
get_cig <- function(df, perspective){
  df <- add_affordability_percent(df =df, perspective = perspective)
  affordability_var <- paste0('perdose_',perspective,'_percent')
  return(ci(ineqvar = df[,perspective],
            outcome = df[,affordability_var],
            type = "CIg"))
}
