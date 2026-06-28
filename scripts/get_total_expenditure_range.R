

# Total expenditure ranges #####################################################
get_total_expenditure <- function(sens_analysis_df,constant_col,alpha){
  analysis <- data.frame(array(NA,dim=c(length(main_sheet$iso3c),5)))
  colnames(analysis) <- c("Country","Population","min_total_expenditure",
                          "median_total_expenditure",
                          "max_total_expenditure")
  analysis$Country <- main_sheet$iso3c
  analysis$Population <- main_sheet$population_size
  analysis$gdp_percapita <- main_sheet$gdp_percapita
  
  for (co in analysis$Country){
    if (!alpha){
      analysis[analysis$Country==co,]$min_total_expenditure <- sens_analysis_df[[co]][1,constant_col]*
        analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
      analysis[analysis$Country==co,]$median_total_expenditure <- sens_analysis_df[[co]][2,constant_col]*
        analysis[analysis$Country==co,]$Population
      analysis[analysis$Country==co,]$max_total_expenditure <- sens_analysis_df[[co]][3,constant_col]*
        analysis[analysis$Country==co,]$Population
    }
    else{
      if (ncol(sens_analysis_df[[1]])>1){
        analysis[analysis$Country==co,]$min_total_expenditure <- sens_analysis_df[[co]][constant_col,1]*
          analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
        analysis[analysis$Country==co,]$median_total_expenditure <- sens_analysis_df[[co]][constant_col,11]*
          analysis[analysis$Country==co,]$Population
        analysis[analysis$Country==co,]$max_total_expenditure <- sens_analysis_df[[co]][constant_col,21]*
          analysis[analysis$Country==co,]$Population
      }
      else{
        analysis[analysis$Country==co,]$min_total_expenditure <- sens_analysis_df[[co]][constant_col,1]*
          analysis[analysis$Country==co,]$Population # temp values only, will convert to percentages later
        analysis[analysis$Country==co,]$median_total_expenditure <- sens_analysis_df[[co]][constant_col,1]*
          analysis[analysis$Country==co,]$Population
        analysis[analysis$Country==co,]$max_total_expenditure <- sens_analysis_df[[co]][constant_col,1]*
          analysis[analysis$Country==co,]$Population
        
      }
    }
  }
  
  return(analysis)
}


bilateral_analysis_expenditure <- get_total_expenditure(bilateral_sens_analysis,constant_col=1,
                                                 alpha=FALSE)
uniform_analysis_expenditure <- get_total_expenditure(uniform_sens_analysis,constant_col=1,
                                               alpha = FALSE)
gdp_adj_analysis_price_expenditure <- get_total_expenditure(gdp_adj_sens_analysis, constant_col = 11,
                                               alpha = FALSE)


gdp_adj_analysis_alpha_expenditure <- get_total_expenditure(gdp_adj_sens_analysis, constant_col=2,
                                               alpha=TRUE)

# gdp_adj_alphabetical <- gdp_adj_analysis_price_expenditure[order(gdp_adj_analysis_price_expenditure$Country),]
# gdp_adj_alphabetical$price_perdose <- gdp_adj[order(gdp_adj$Country),]$perdose
# gdp_adj_alphabetical <- gdp_adj_alphabetical[,c('Country','Population',
#                                                 'price_perdose','min_total_expenditure',
#                                                 'median_total_expenditure','max_total_expenditure')]
# bilateral_analysis_expenditure <- bilateral_analysis_expenditure[order(bilateral_analysis_expenditure$Country),]

# Getting table for supplementary
gdp_adj_analysis_price_expenditure$min_total_savings <- bilateral_analysis_expenditure$min_total_expenditure-gdp_adj_analysis_price_expenditure$min_total_expenditure
gdp_adj_analysis_price_expenditure$median_total_savings <- bilateral_analysis_expenditure$median_total_expenditure-gdp_adj_analysis_price_expenditure$median_total_expenditure
gdp_adj_analysis_price_expenditure$max_total_savings <- bilateral_analysis_expenditure$max_total_expenditure-gdp_adj_analysis_price_expenditure$max_total_expenditure
gdp_adj_analysis_price_expenditure$price_perdose_max <- NA
gdp_adj_analysis_price_expenditure$price_perdose_min <- NA
gdp_adj_analysis_price_expenditure$price_perdose <- gdp_adj$perdose

for (co in gdp_adj_analysis_price_expenditure$Country){
    gdp_adj_analysis_price_expenditure[gdp_adj_analysis_price_expenditure$Country==co,]$price_perdose_min <- gdp_adj_sens_analysis[[co]][1,11]
    gdp_adj_analysis_price_expenditure[gdp_adj_analysis_price_expenditure$Country==co,]$price_perdose_max <- gdp_adj_sens_analysis[[co]][3,11]
}

gdp_adj_supp_table <- gdp_adj_analysis_price_expenditure[,c('Country','Population','price_perdose',
                                                'price_perdose_min','price_perdose_max',
                                                colnames(gdp_adj_analysis_price_expenditure)[3:8])]

gdp_adj_supp_table$perdose_final <- paste0(round(gdp_adj_supp_table$price_perdose,2)," (",
                                             round(gdp_adj_supp_table$price_perdose_min,2), " - ",
                                             round(gdp_adj_supp_table$price_perdose_max,2),")")

gdp_adj_supp_table$expenditure_final <- paste0(round(gdp_adj_supp_table$median_total_expenditure,2)," (",
                                             round(gdp_adj_supp_table$min_total_expenditure,2), " - ",
                                             round(gdp_adj_supp_table$max_total_expenditure,2),")")

gdp_adj_supp_table$savings_final <- paste0(round(gdp_adj_supp_table$median_total_savings,2)," (",
                                                 round(gdp_adj_supp_table$min_total_savings,2), " - ",
                                                 round(gdp_adj_supp_table$max_total_savings,2),")")

to_save <- gdp_adj_supp_table[,c('Country','perdose_final','expenditure_final','savings_final')]
library(writexl)
write_xlsx(to_save,path='output/gdp_adj_price_table.xlsx')
