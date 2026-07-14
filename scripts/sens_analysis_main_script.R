source('scripts/sens_analysis_function.R')
library(rineq)
# As sens analysis only modifies price per dose, we only do it once when gov_expcking
# for the effect of switching from full to real coverage

bilateral_sens_analysis <- general_sens_analysis(bilateral_full,price_range = base_price_tested)
uniform_sens_analysis <- general_sens_analysis(uniform_full,price_range = base_price_tested)
gdp_adj_sens_analysis <- tiered_pricing_selection(P_min = 5,
                                                  price_range = base_price_tested,
                                                  alphas_range = alphas_tested,
                                                  country_wealth_var = 'GDPPC')
chepc_adj_sens_analysis <- tiered_pricing_selection(P_min = 5,
                                                  price_range = base_price_tested,
                                                  alphas_range = alphas_tested,
                                                  country_wealth_var = 'CHEPC')
# gov_exp_adj_sens_analysis <- tiered_pricing_selection(P_min = 5,
#                                                         price_range = base_price_tested,
#                                                         alphas_range = alphas_tested,
#                                                         country_wealth_var = 'gov_exp')

bilateral_full_analysis_price <- get_percent_share_df(base_df = bilateral_full,
                                                      sens_analysis_df = bilateral_sens_analysis,
                                                      constant_col = 1, alpha = FALSE)
bilateral_real_analysis_price <- get_percent_share_df(base_df = bilateral_real,
                                                      sens_analysis_df = bilateral_sens_analysis,
                                                      constant_col = 1, alpha = FALSE)

uniform_full_analysis_price <- get_percent_share_df(base_df = uniform_full,
                                                      sens_analysis_df = uniform_sens_analysis,
                                                      constant_col = 1, alpha = FALSE)
uniform_real_analysis_price <- get_percent_share_df(base_df = uniform_real,
                                                         sens_analysis_df = uniform_sens_analysis,
                                                         constant_col = 1, alpha = FALSE)
gdp_adj_full_analysis_price <-  get_percent_share_df(base_df = adj_gdppc_full,
                                                     sens_analysis_df = gdp_adj_sens_analysis,
                                                     constant_col = 11, alpha = FALSE)
gdp_adj_real_analysis_price <-  get_percent_share_df(base_df = adj_gdppc_real,
                                                     sens_analysis_df = gdp_adj_sens_analysis,
                                                     constant_col = 11, alpha = FALSE)
gdp_adj_full_analysis_alpha <-  get_percent_share_df(base_df = adj_gdppc_full,
                                                     sens_analysis_df = gdp_adj_sens_analysis,
                                                     constant_col = 2, alpha = TRUE)
gdp_adj_real_analysis_alpha <-  get_percent_share_df(base_df = adj_gdppc_real,
                                                     sens_analysis_df = gdp_adj_sens_analysis,
                                                     constant_col = 2, alpha = TRUE)
gdp_adj_full_analysis_alpha$iso2c <- countrycode(gdp_adj_full_analysis_alpha$Country,origin = 'iso3c',
                                            destination = 'iso2c')
# gov_exp_adj_full_analysis_price <- get_percent_share_df(base_df = adj_gov_exp_full,
#                                                     sens_analysis_df = gov_exp_adj_sens_analysis,
#                                                     constant_col = 11, alpha=FALSE)
# gov_exp_adj_full_analysis_alpha <- get_percent_share_df(base_df = adj_gov_exp_full,
#                                                     sens_analysis_df = gov_exp_adj_sens_analysis,
#                                                     constant_col = 2, alpha=TRUE)
chepc_adj_full_analysis_alpha <- get_percent_share_df(base_df = adj_chepc_full,
                                                        sens_analysis_df = chepc_adj_sens_analysis,
                                                        constant_col = 2, alpha=TRUE)
# gov_exp_adj_real_analysis_price <- get_percent_share_df(base_df = adj_gov_exp_real,
#                                                     sens_analysis_df = gov_exp_adj_sens_analysis,
#                                                     constant_col = 11, alpha=FALSE)
# gov_exp_adj_real_analysis_alpha <- get_percent_share_df(base_df = adj_gov_exp_real,
#                                                     sens_analysis_df = gov_exp_adj_sens_analysis,
#                                                    constant_col = 2, alpha=TRUE)
chepc_adj_real_analysis_alpha <- get_percent_share_df(base_df = adj_chepc_real,
                                                      sens_analysis_df = chepc_adj_sens_analysis,
                                                      constant_col = 2, alpha=TRUE)


# wci range
gdppc_adj_alpha_wci <- get_wci_range(base_df = adj_gdppc_full,
                                     sens_analysis_df = gdp_adj_sens_analysis,
                                     constant_col = 2, wealth_metric = 'gdp_percapita',
                                     affordability_metric = 'perdose_gdppc_percent')
gdppc_adj_alpha_gdppc_min <- gdppc_adj_alpha_wci[[2]]$wci_min
gdppc_adj_alpha_gdppc_max <- gdppc_adj_alpha_wci[[2]]$wci_max

# gdppc_adj_alpha_wci_gov_exp <- get_wci_range(base_df = adj_gdppc_full,
#                                      sens_analysis_df = gdp_adj_sens_analysis,
#                                      constant_col = 2, wealth_metric = 'gov_exp',
#                                      affordability_metric = 'perdose_gov_exp_percent')
# gdppc_adj_alpha_gov_exp_min <- gdppc_adj_alpha_wci_gov_exp[[2]]$wci_min
# gdppc_adj_alpha_gov_exp_max <- gdppc_adj_alpha_wci_gov_exp[[2]]$wci_max
# 
# gov_exp_adj_alpha_wci_gdppc <- get_wci_range(base_df = adj_gov_exp_full,
#                                      sens_analysis_df = gov_exp_adj_sens_analysis,
#                                      constant_col = 2, wealth_metric = 'gdp_percapita',
#                                      affordability_metric = 'perdose_gdppc_percent')
# gov_exp_adj_alpha_gdppc_min <- gov_exp_adj_alpha_wci_gdppc[[2]]$wci_min
# gov_exp_adj_alpha_gdppc_max <- gov_exp_adj_alpha_wci_gdppc[[2]]$wci_max

# gov_exp_adj_alpha_wci_gov_exp <- get_wci_range(base_df = adj_gov_exp_full,
#                                            sens_analysis_df = gov_exp_adj_sens_analysis,
#                                            constant_col = 2, wealth_metric = 'gov_exp',
#                                            affordability_metric = 'perdose_gov_exp_percent')
# gov_exp_adj_alpha_gov_exp_min <- gov_exp_adj_alpha_wci_gov_exp[[2]]$wci_min
# gov_exp_adj_alpha_gov_exp_max <- gov_exp_adj_alpha_wci_gov_exp[[2]]$wci_max
