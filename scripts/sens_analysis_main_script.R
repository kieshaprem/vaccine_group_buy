# As sens analysis only modifies price per dose, we only do it once when checking
# for the effect of switching from full to real coverage

bilateral_sens_analysis <- general_sens_analysis(bilateral_full,price_range = base_price_tested)
uniform_sens_analysis <- general_sens_analysis(uniform_full,price_range = base_price_tested)
gdp_adj_sens_analysis <- tiered_pricing_selection(P_min = 5,
                                                  price_range = base_price_tested,
                                                  alphas_range = alphas_tested,
                                                  country_wealth_var = 'GDPPC')
ooppc_adj_sens_analysis <- tiered_pricing_selection(P_min = 5,
                                                        price_range = base_price_tested,
                                                        alphas_range = alphas_tested,
                                                        country_wealth_var = 'OOPPC')

## reordering the partial dfs
bilateral_partial <- bilateral_partial[order(bilateral_partial$gdp_percapita),]
uniform_partial <- uniform_partial[order(uniform_partial$gdp_percapita),]
adj_partial_gdppc <- adj_partial_gdppc[order(adj_partial_gdppc$gdp_percapita),]

bilateral_full_analysis_price <- get_percent_share_df(base_df = bilateral_full,
                                                      sens_analysis_df = bilateral_sens_analysis,
                                                      constant_col = 1, alpha = FALSE)
bilateral_partial_analysis_price <- get_percent_share_df(base_df = bilateral_partial,
                                                      sens_analysis_df = bilateral_sens_analysis,
                                                      constant_col = 1, alpha = FALSE)

uniform_full_analysis_price <- get_percent_share_df(base_df = uniform_full,
                                                      sens_analysis_df = uniform_sens_analysis,
                                                      constant_col = 1, alpha = FALSE)
uniform_partial_analysis_price <- get_percent_share_df(base_df = uniform_partial,
                                                         sens_analysis_df = uniform_sens_analysis,
                                                         constant_col = 1, alpha = FALSE)
gdp_adj_full_analysis_price <-  get_percent_share_df(base_df = adj_full_gdppc,
                                                     sens_analysis_df = gdp_adj_sens_analysis,
                                                     constant_col = 11, alpha = FALSE)
gdp_adj_partial_analysis_price <-  get_percent_share_df(base_df = adj_partial_gdppc,
                                                     sens_analysis_df = gdp_adj_sens_analysis,
                                                     constant_col = 11, alpha = FALSE)
gdp_adj_full_analysis_alpha <-  get_percent_share_df(base_df = adj_full_gdppc,
                                                     sens_analysis_df = gdp_adj_sens_analysis,
                                                     constant_col = 2, alpha = TRUE)
gdp_adj_partial_analysis_alpha <-  get_percent_share_df(base_df = adj_partial_gdppc,
                                                     sens_analysis_df = gdp_adj_sens_analysis,
                                                     constant_col = 2, alpha = TRUE)
gdp_adj_full_analysis_alpha$iso2c <- countrycode(gdp_adj_full_analysis_alpha$Country,origin = 'iso3c',
                                            destination = 'iso2c')

## analysing percent share for sens analysis on OOP-adjusted vax price
adj_full <- adj_full[order(adj_full$gdp_percapita),]
oop_adj_full_analysis_price <- get_percent_share_df(base_df = adj_full,
                                                    sens_analysis_df = ooppc_adj_sens_analysis,
                                                    constant_col = 11, alpha=FALSE)
oop_adj_full_analysis_alpha <- get_percent_share_df(base_df = adj_full,
                                                    sens_analysis_df = ooppc_adj_sens_analysis,
                                                    constant_col = 2, alpha=TRUE)
