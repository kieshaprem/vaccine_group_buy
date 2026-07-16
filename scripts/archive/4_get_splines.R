# affordability_metric = perdose_gdppc_percent

spline_gdppc_percent_bilateral <- spline(x=log(x=bilateral_full$gdp_percapita,base=10),
                                         y=bilateral_full$perdose_gdppc_percent, n = 1000)
plot(spline_gdppc_percent_bilateral$x,spline_gdppc_percent_bilateral$y)

spline_gdppc_percent_uniform <- spline(x=log(x=uniform_full$gdp_percapita,base=10),
                                       y=uniform_full$perdose_gdppc_percent, n = 1000)

spline_gdppc_percent_gdp_adj <- spline(x=log(x=adj_gdppc_full$gdp_percapita,base=10),
                                       y=adj_gdppc_full$perdose_gdppc_percent, n = 1000)
plot(log(x=adj_gdppc_full$gdp_percapita,base=10),adj_gdppc_full$perdose_gdppc_percent)
lines(spline_gdppc_percent_gdp_adj$x,spline_gdppc_percent_gdp_adj$y)


# adj_gov_exp_full <- adj_gov_exp_full[order(adj_gov_exp_full$gdp_percapita),]
# spline_gdppc_percent_gov_exp_adj <- spline(x=log(x=adj_gov_exp_full$gdp_percapita,base=10),
#                                          y=adj_gov_exp_full$perdose_gdppc_percent,n=1000)
# spline_gdppc_percent_gov_exp_adj <- smooth.spline(x=log(x=adj_gov_exp_full$gdp_percapita,base=10),
#                                        y=adj_gov_exp_full$perdose_gdppc_percent,spar=0.4)
# plot(log(x=adj_gov_exp_full$gdp_percapita,base=10),adj_gov_exp_full$perdose_gdppc_percent)
# lines(spline_gdppc_percent_gov_exp_adj$x,spline_gdppc_percent_gov_exp_adj$y)
spline_gdppc_percent_chepc_adj <- spline(x=log(x=adj_chepc_full$gdp_percapita,base=10),
                                           y=adj_chepc_full$perdose_gdppc_percent,n=1000)
# affordability_metric = perdose_gov_exp_percent
# bilateral_full <- bilateral_full[order(bilateral_full$gov_exp),]
# spline_gov_exp_percent_bilateral <- spline(x=log(bilateral_full$gov_exp,base = 10),
#                                          y=bilateral_full$perdose_gov_exp_percent, n=1000)
# plot(spline_gov_exp_percent_bilateral$x,spline_gov_exp_percent_bilateral$y)
# plot(bilateral_full$gov_exp,bilateral_full$perdose_gov_exp_percent)
# plot(log(bilateral_full$gov_exp,base = 10),bilateral_full$perdose_gov_exp_percent)

#uniform_full <- uniform_full[order(uniform_full$gov_exp),]
# spline_gov_exp_percent_uniform <- spline(x=log(uniform_full$gov_exp,base=10),
#                                        y=uniform_full$perdose_gov_exp_percent,n=1000)
# plot(spline_gov_exp_percent_uniform$x,spline_gov_exp_percent_uniform$y)

#adj_gdppc_full <- adj_gdppc_full[order(adj_gdppc_full$gov_exp),]
# spline_gov_exp_percent_gdppc_adj <- spline(x=log(adj_gdppc_full$gov_exp,base=10),
#                                        y=adj_gdppc_full$perdose_gov_exp_percent,n=1000)
# plot(spline_gov_exp_percent_gdppc_adj$x,spline_gov_exp_percent_gdppc_adj$y)
# 
# adj_gov_exp_full <- adj_gov_exp_full[order(adj_gov_exp_full$gov_exp),]
# spline_gov_exp_percent_gov_exp_adj <- spline(x=log(adj_gov_exp_full$gov_exp,base=10),
#                                          y=adj_gov_exp_full$perdose_gov_exp_percent,n=1000)
# plot(spline_gov_exp_percent_gov_exp_adj$x,spline_gov_exp_percent_gov_exp_adj$y)
# 
# splines for sensitivity analysis, affordability_metric = perdose_chepc_percent
bilateral_full <- bilateral_full[order(bilateral_full$chepc),]
spline_chepc_percent_bilateral <- spline(x=log(bilateral_full$chepc,base = 10),
                                           y=bilateral_full$perdose_chepc_percent, n=1000)
plot(spline_chepc_percent_bilateral$x,spline_chepc_percent_bilateral$y)
plot(bilateral_full$chepc,bilateral_full$perdose_chepc_percent)
plot(log(bilateral_full$chepc,base = 10),bilateral_full$perdose_chepc_percent)

uniform_full <- uniform_full[order(uniform_full$chepc),]
spline_chepc_percent_uniform <- spline(x=log(uniform_full$chepc,base=10),
                                         y=uniform_full$perdose_chepc_percent,n=1000)
plot(spline_chepc_percent_uniform$x,spline_chepc_percent_uniform$y)

adj_gdppc_full <- adj_gdppc_full[order(adj_gdppc_full$chepc),]
spline_chepc_percent_gdppc_adj <- spline(x=log(adj_gdppc_full$chepc,base=10),
                                           y=adj_gdppc_full$perdose_chepc_percent,n=1000)
plot(spline_chepc_percent_gdppc_adj$x,spline_chepc_percent_gdppc_adj$y)

adj_chepc_full <- adj_chepc_full[order(adj_chepc_full$chepc),]
spline_chepc_percent_chepc_adj <- spline(x=log(adj_chepc_full$chepc,base=10),
                                             y=adj_chepc_full$perdose_chepc_percent,n=1000)
plot(spline_chepc_percent_chepc_adj$x,spline_chepc_percent_chepc_adj$y)


# splines for sensitivity analysis, affordability_metric = perdose_gdppc_percent
spline_gdppc_percent_gdppc_min <- spline(x=log(gdppc_adj_alpha_gdppc_min$gdp_percapita,base=10),
                                         y=gdppc_adj_alpha_gdppc_min$perdose_gdppc_percent,n=1000)
spline_gdppc_percent_gdppc_max <- spline(x=log(gdppc_adj_alpha_gdppc_max$gdp_percapita,base=10),
                                        y=gdppc_adj_alpha_gdppc_max$perdose_gdppc_percent,n=1000)
# spline_gov_exp_percent_gdppc_min <- spline(x=log(gov_exp_adj_alpha_gdppc_min$gdp_percapita,base=10),
#                                          y=gov_exp_adj_alpha_gdppc_min$perdose_gdppc_percent,n=1000)
# spline_gov_exp_percent_gdppc_max <- spline(x=log(gov_exp_adj_alpha_gdppc_max$gdp_percapita,base=10),
#                                          y=gov_exp_adj_alpha_gdppc_max$perdose_gdppc_percent,n=1000)

# splines for sensitivity analysis, affordability_metric = perdose_gov_exp_percent
# spline_gdppc_percent_gov_exp_min <- spline(x=log(gdppc_adj_alpha_gov_exp_min$gov_exp,base=10),
#                                          y=gdppc_adj_alpha_gov_exp_min$perdose_gov_exp_percent,n=1000)
# spline_gdppc_percent_gov_exp_max <- spline(x=log(gdppc_adj_alpha_gov_exp_max$gov_exp,base=10),
#                                         y=gdppc_adj_alpha_gov_exp_max$perdose_gov_exp_percent,n=1000)
# spline_gov_exp_percent_gov_exp_max <- spline(x=log(gov_exp_adj_alpha_gov_exp_max$gov_exp,base=10),
#                                          y=gov_exp_adj_alpha_gov_exp_max$perdose_gov_exp_percent,n=1000)
# plot(spline_gov_exp_percent_gov_exp_max$x,spline_gov_exp_percent_gov_exp_max$y)
# lines(spline_gov_exp_percent_gov_exp_adj$x,spline_gov_exp_percent_gov_exp_adj$y)

# spline_gov_exp_percent_gov_exp_min <- spline(x=log(gov_exp_adj_alpha_gov_exp_min$gov_exp,base=10),
#                                          y=gov_exp_adj_alpha_gov_exp_min$perdose_gov_exp_percent,n=1000)
#lines(spline_gov_exp_percent_gov_exp_min$x,spline_gov_exp_percent_gov_exp_min$y)

# splines for sensitivity analysis, affordability_metric = perdose_chepc_percent
spline_gdppc_percent_chepc_min <- spline(x=log(gdppc_adj_alpha_chepc_min$chepc,base=10),
                                           y=gdppc_adj_alpha_chepc_min$perdose_chepc_percent,n=1000)
spline_gdppc_percent_chepc_max <- spline(x=log(gdppc_adj_alpha_chepc_max$chepc,base=10),
                                           y=gdppc_adj_alpha_chepc_max$perdose_chepc_percent,n=1000)
spline_chepc_percent_chepc_max <- spline(x=log(chepc_adj_alpha_chepc_max$chepc,base=10),
                                             y=chepc_adj_alpha_chepc_max$perdose_chepc_percent,n=1000)
spline_chepc_percent_chepc_min <- spline(x=log(chepc_adj_alpha_chepc_min$chepc,base=10),
                                             y=chepc_adj_alpha_chepc_min$perdose_chepc_percent,n=1000)