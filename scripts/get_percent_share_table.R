bilateral_full$share <- bilateral_full_analysis_price$percent_share_median
uniform_full$share <- uniform_full_analysis_price$percent_share_median
adj_full_gdppc$share <- gdp_adj_full_analysis_price$percent_share_median

bilateral_partial$share <- bilateral_partial_analysis_price$percent_share_median
uniform_partial$share <- uniform_partial_analysis_price$percent_share_median
adj_partial_gdppc$share <- gdp_adj_partial_analysis_price$percent_share_median

library(writexl)
write_xlsx(list(bilateral_full=bilateral_full,
                bilateral_partial=bilateral_partial,
                uniform_full=uniform_full,
                uniform_partial=uniform_partial,
                gdp_adj_full=adj_full_gdppc,
                gdp_adj_partial=adj_partial_gdppc),path = 'output/modelling_data_update2.xlsx')

share_comparison <- data.frame(cbind('Country_Code'=bilateral_full$iso3c,
                                     'Bilateral_Full_Coverage'=round(bilateral_full$share,2),
                                     'Bilateral_Real_Coverage'=round(bilateral_partial$share,2),
                                     'Uniform_Full_Coverage'=round(uniform_full$share,2),
                                     'Uniform_Real_Coverage'=round(uniform_partial$share,2),
                                     'GDP_Adjusted_Full_Coverage'=round(adj_full_gdppc$share,2),
                                     'GDP_Adjusted_Real_Coverage'=round(adj_partial_gdppc$share,2)))
write_xlsx(share_comparison,path = 'output/share_comparison.xlsx')
