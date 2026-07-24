gdp_adj_alpha_alphabetical <- gdp_adj_analysis_alpha_expenditure[order(gdp_adj_analysis_alpha_expenditure$Country),]
gdp_adj_alpha_alphabetical$price_perdose <- gdp_adj[order(gdp_adj$Country),]$perdose

gdp_adj_alpha_alphabetical$min_total_savings <- bilateral_analysis_expenditure$median_total_expenditure-gdp_adj_alpha_alphabetical$min_total_expenditure
gdp_adj_alpha_alphabetical$median_total_savings <- bilateral_analysis_expenditure$median_total_expenditure-gdp_adj_alpha_alphabetical$median_total_expenditure
gdp_adj_alpha_alphabetical$max_total_savings <- bilateral_analysis_expenditure$median_total_expenditure-gdp_adj_alpha_alphabetical$max_total_expenditure
gdp_adj_alpha_alphabetical$price_perdose_max <- NA
gdp_adj_alpha_alphabetical$price_perdose_min <- NA


for (co in gdp_adj_alphabetical$Country){
  gdp_adj_alpha_alphabetical[gdp_adj_alpha_alphabetical$Country==co,]$price_perdose_min <- gdp_adj_sens_analysis[[co]][2,1]
  gdp_adj_alpha_alphabetical[gdp_adj_alpha_alphabetical$Country==co,]$price_perdose_max <- gdp_adj_sens_analysis[[co]][2,21]
}

gdp_adj_alpha_alphabetical <- gdp_adj_alpha_alphabetical[,c('Country','Population','price_perdose',
                                                'price_perdose_min','price_perdose_max',
                                                colnames(gdp_adj_alpha_alphabetical)[3:5],
                                                colnames(gdp_adj_alpha_alphabetical)[7:9])]

gdp_adj_alpha_alphabetical$perdose_final <- paste0(round(gdp_adj_alpha_alphabetical$price_perdose,2)," (",
                                             round(gdp_adj_alpha_alphabetical$price_perdose_min,2), " - ",
                                             round(gdp_adj_alpha_alphabetical$price_perdose_max,2),")")

gdp_adj_alpha_alphabetical$expenditure_final <- paste0(round(gdp_adj_alpha_alphabetical$median_total_expenditure,2)," (",
                                                 round(gdp_adj_alpha_alphabetical$min_total_expenditure,2), " - ",
                                                 round(gdp_adj_alpha_alphabetical$max_total_expenditure,2),")")

gdp_adj_alpha_alphabetical$savings_final <- paste0(round(gdp_adj_alpha_alphabetical$median_total_savings,2)," (",
                                             round(gdp_adj_alpha_alphabetical$min_total_savings,2), " - ",
                                             round(gdp_adj_alpha_alphabetical$max_total_savings,2),")")

to_save <- gdp_adj_alpha_alphabetical[,c('Country','perdose_final','expenditure_final','savings_final')]
library(writexl)
write_xlsx(to_save,path='output/gdp_adj_alpha_table.xlsx')

gdp_adj_analysis_alpha <- gdp_adj_analysis_alpha[order(gdp_adj_analysis_alpha$Country),]
gdp_adj_analysis_alpha$share_final <- paste0(round(gdp_adj_analysis_alpha$percent_share_median,2)," (",
                                             round(gdp_adj_analysis_alpha$percent_share_min,2), " - ",
                                             round(gdp_adj_analysis_alpha$percent_share_max,2),")")
write_xlsx(gdp_adj_analysis_alpha,path='output/gdp_adj_alpha_share.xlsx')

(sum(bilateral$total_perdose)-sum(uniform_analysis_expenditure$median_total_expenditure))/sum(bilateral$total_perdose)
(sum(bilateral_analysis_expenditure$median_total_expenditure)-sum(uniform_analysis_expenditure$min_total_expenditure))/sum(bilateral_analysis_expenditure$min_total_expenditure)
(sum(bilateral_analysis_expenditure$max_total_expenditure)-sum(uniform_analysis_expenditure$max_total_expenditure))/sum(bilateral_analysis_expenditure$max_total_expenditure)


# When baseline vaccine procurement price is varied between 50% and 150%, the total expenditure on COVID-19 vaccines 
# Uniform sensitivity analysis (price varied from 50% to 150% of set bilateral price)
# min: 62.6%
# Max: -12.2%
# GDP-adjusted sensitivity analysis (price varied from 50% to 150% of set bilateral price)
# Min: 63.0%
# Max: -11.1%
# GDP-adjusted sensitivity analysis (alpha, varied from 0 to 20)
# min: 50.1%
# Max: 1.7%
# regional pooled procurement could have reduced the total expenditure on COVID-19 vaccines by an estimated 25-26%    (25.1% under uniform pricing model and 25.9% under GDP-adjusted pricing model).
