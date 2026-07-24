bilateral_full$share <- bilateral_full_analysis_price$percent_share_median
uniform_full$share <- uniform_full_analysis_price$percent_share_median

adj_gdppc_full$share <- gdp_adj_full_analysis_price$percent_share_median
adj_gdppc_full$share_alpha_min <- gdp_adj_full_analysis_alpha$percent_share_min
adj_gdppc_full$share_alpha_max <- gdp_adj_full_analysis_alpha$percent_share_max

adj_gov_exp_full$share <- gov_exp_adj_full_analysis_price$percent_share_median
adj_gov_exp_full$share_alpha_min <- gov_exp_adj_full_analysis_alpha$percent_share_min
adj_gov_exp_full$share_alpha_max <- gov_exp_adj_full_analysis_alpha$percent_share_max

bilateral_real$share <- bilateral_real_analysis_price$percent_share_median
uniform_real$share <- uniform_real_analysis_price$percent_share_median

adj_gdppc_real$share <- gdp_adj_real_analysis_price$percent_share_median
adj_gdppc_real$share_alpha_min <- gdp_adj_real_analysis_alpha$percent_share_min
adj_gdppc_real$share_alpha_max <- gdp_adj_real_analysis_alpha$percent_share_max

adj_gov_exp_real$share <- gov_exp_adj_real_analysis_price$percent_share_median
adj_gov_exp_real$share_alpha_min <- gov_exp_adj_real_analysis_alpha$percent_share_min
adj_gov_exp_real$share_alpha_max <- gov_exp_adj_real_analysis_alpha$percent_share_max

# 
# library(writexl)
# write_xlsx(list(bilateral_full=bilateral_full,
#                 bilateral_real=bilateral_real,
#                 uniform_full=uniform_full,
#                 uniform_real=uniform_real,
#                 gdp_adj_full=adj_gdppc_full,
#                 gdp_adj_real=adj_gdppc_real),path = 'output/modelling_data_update2.xlsx')

share_comparison <- data.frame(cbind('Country Code'=bilateral_full$iso3c,
'Bilateral Full Coverage'=round(bilateral_full$share,2),
'Bilateral Real Coverage'=round(bilateral_real$share,2),
'Uniform Full Coverage'=round(uniform_full$share,2),
'Uniform Real Coverage'=round(uniform_real$share,2),
'GDP Adjusted Full Coverage (Min Alpha)'= round(adj_gdppc_full$share_alpha_min,2),
'GDP Adjusted Full Coverage (Median)'=round(adj_gdppc_full$share,2),
'GDP Adjusted Full Coverage (Max Alpha)'=round(adj_gdppc_full$share_alpha_max,2),
'GDP Adjusted Real Coverage (Min Alpha)'=round(adj_gdppc_real$share_alpha_min,2),
'GDP Adjusted Real Coverage (Median)'=round(adj_gdppc_real$share,2)),
'GDP Adjusted Real Coverage (Max Alpha)'=round(adj_gdppc_real$share_alpha_max,2),
'gov_exp Adjusted Full Coverage (Min Alpha)'=round(adj_gov_exp_full$share_alpha_min,2),
'gov_exp Adjusted Full Coverage (Median)'=round(adj_gov_exp_full$share,2),
'gov_exp Adjusted Full Coverage (Max Alpha)'=round(adj_gov_exp_full$share_alpha_max,2),
'gov_exp Adjusted Real Coverage (Min Alpha)'=round(adj_gov_exp_real$share_alpha_min,2),
'gov_exp Adjusted Real Coverage (Median)'=round(adj_gov_exp_real$share,2),
'gov_exp Adjusted Real Coverage (Max Alpha)'=round(adj_gov_exp_real$share_alpha_max,2))

names(share_comparison) <- c('Country Code','Bilateral Full Coverage','Bilateral Real Coverage',
                             'Uniform Full Coverage','Uniform Real Coverage',
                             'GDP Adjusted Full Coverage (Min Alpha)',
                             'GDP Adjusted Full Coverage (Median)',
                             'GDP Adjusted Full Coverage (Max Alpha)',
                             'GDP Adjusted Real Coverage (Min Alpha)',
                             'GDP Adjusted Real Coverage (Median)',
                             'GDP Adjusted Real Coverage (Max Alpha)',
                             'gov_exp Adjusted Full Coverage (Min Alpha)',
                             'gov_exp Adjusted Full Coverage (Median)',
                             'gov_exp Adjusted Full Coverage (Max Alpha)',
                             'gov_exp Adjusted Real Coverage (Min Alpha)',
                             'gov_exp Adjusted Real Coverage (Median)',
                             'gov_exp Adjusted Real Coverage (Max Alpha)' )
                               
write_xlsx(share_comparison,path = 'output/share_comparison_2.xlsx')
