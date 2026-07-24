# affordability_metric = perdose_gdppc_percent

spline_gdppc_percent_bilateral <- spline(x=log(x=bilateral$gdppc,base=10),
                                         y=bilateral$perdose_gdppc_percent, n = 1000)
# smooth_spline_gdppc_bilateral <- smooth.spline(x=log(x=bilateral$gdppc,base=10),
#                                                y=bilateral$perdose_gdppc_percent, df = 11)
#plot(log(x=bilateral$gdppc,base=10),bilateral$perdose_gdppc_percent)
#lines(spline_gdppc_percent_bilateral$x,spline_gdppc_percent_bilateral$y)
#lines(smooth_spline_gdppc_bilateral,col='blue')

spline_gdppc_percent_uniform <- spline(x=log(x=uniform$gdppc,base=10),
                                       y=uniform$perdose_gdppc_percent, n = 1000)
#plot(spline_gdppc_percent_uniform$x,spline_gdppc_percent_uniform$y)

spline_gdppc_percent_gdppc_adj <- spline(x=log(x=adj_gdppc$gdppc,base=10),
                                       y=adj_gdppc$perdose_gdppc_percent, n = 1000)
spline_gdppc_percent_chepc_adj <- spline(x=log(x=adj_chepc$gdppc,base=10),
                                           y=adj_chepc$perdose_gdppc_percent,n=1000)

bilateral <- bilateral[order(bilateral$chepc),]
uniform <- uniform[order(uniform$chepc),]
adj_gdppc <- adj_gdppc[order(adj_gdppc$chepc),]
adj_chepc <- adj_chepc[order(adj_chepc$chepc),]

spline_chepc_percent_bilateral <- spline(x=log(bilateral$chepc,base = 10),
                                           y=bilateral$perdose_chepc_percent, n =1000)
# plot(log(bilateral$chepc,base = 10),bilateral$perdose_chepc_percent)
# lines(spline_chepc_percent_bilateral,col='blue')
# lines(spline_chepc_percent_bilateral,col='green')
# lines(spline_chepc_percent_bilateral,col='red')
# lines(spline_chepc_percent_bilateral,col='purple')
# lines(spline_chepc_percent_bilateral,col='brown')
# 
# 
# interpolated <- predict(spline_chepc_percent_bilateral$fit,x = log(bilateral$chepc,base = 10))
# lines(interpolated$x,interpolated$y)
# lines(spline_chepc_percent_bilateral$fit)
# spline_chepc_percent_bilateral <- spline(x=log(bilateral$chepc,base=10),
#                                          y=bilateral$perdose_chepc_percent,n=1000)
# plot(spline_chepc_percent_bilateral$x,spline_chepc_percent_bilateral$y)

spline_chepc_percent_uniform <- spline(x=log(uniform$chepc,base=10),
                                         y=uniform$perdose_chepc_percent,n=1000)
#plot(spline_chepc_percent_uniform$x,spline_chepc_percent_uniform$y)
spline_chepc_percent_gdppc_adj <- spline(x=log(adj_gdppc$chepc,base=10),
                                           y=adj_gdppc$perdose_chepc_percent,n=1000)
#plot(spline_chepc_percent_gdppc_adj$x,spline_chepc_percent_gdppc_adj$y)

spline_chepc_percent_chepc_adj <- spline(x=log(adj_chepc$chepc,base=10),
                                             y=adj_chepc$perdose_chepc_percent,n=1000)
#plot(spline_chepc_percent_chepc_adj$x,spline_chepc_percent_chepc_adj$y)
#dev.off()
# splines for sensitivity analysis, affordability_metric = perdose_gdppc_percent
# spline_gdppc_percent_gdppc_min <- spline(x=log(gdppc_adj_alpha_gdppc_min$gdppc,base=10),
#                                          y=gdppc_adj_alpha_gdppc_min$perdose_gdppc_percent,n=1000)
# spline_gdppc_percent_gdppc_max <- spline(x=log(gdppc_adj_alpha_gdppc_max$gdppc,base=10),
#                                         y=gdppc_adj_alpha_gdppc_max$perdose_gdppc_percent,n=1000)
# # splines for sensitivity analysis, affordability_metric = perdose_chepc_percent
# spline_gdppc_percent_chepc_min <- spline(x=log(gdppc_adj_alpha_chepc_min$chepc,base=10),
#                                            y=gdppc_adj_alpha_chepc_min$perdose_chepc_percent,n=1000)
# spline_gdppc_percent_chepc_max <- spline(x=log(gdppc_adj_alpha_chepc_max$chepc,base=10),
#                                            y=gdppc_adj_alpha_chepc_max$perdose_chepc_percent,n=1000)
# spline_chepc_percent_chepc_max <- spline(x=log(chepc_adj_alpha_chepc_max$chepc,base=10),
#                                              y=chepc_adj_alpha_chepc_max$perdose_chepc_percent,n=1000)
# spline_chepc_percent_chepc_min <- spline(x=log(chepc_adj_alpha_chepc_min$chepc,base=10),
#                                              y=chepc_adj_alpha_chepc_min$perdose_chepc_percent,n=1000)