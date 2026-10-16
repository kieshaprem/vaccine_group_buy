# ------------------------------------------------------------------------------
# Perspective of affordability (financial burden) of vaccine price per dose
# as % of GDP per capita
# ------------------------------------------------------------------------------

# Order df by GDP per capita
bilateral <- bilateral[order(bilateral$gdppc),]
uniform <- uniform[order(uniform$gdppc),]
adj_gdppc <- adj_gdppc[order(adj_gdppc$gdppc),]
adj_chepc <- adj_chepc[order(adj_chepc$gdppc),]

# Get the affordability (financial burden) variable for each df
bilateral <- add_affordability_percent(bilateral,'gdppc')
uniform <- add_affordability_percent(uniform,'gdppc')
adj_gdppc <- add_affordability_percent(adj_gdppc,'gdppc')
adj_chepc <- add_affordability_percent(adj_chepc,'gdppc')

# Splines for equity plots
spline_gdppc_percent_bilateral <- spline(x=log(x=bilateral$gdppc,base=10),
                                         y=bilateral$perdose_gdppc_percent, 
                                         n = 1000)
spline_gdppc_percent_uniform <- spline(x=log(x=uniform$gdppc,base=10),
                                       y=uniform$perdose_gdppc_percent, 
                                       n = 1000)
spline_gdppc_percent_gdppc_adj <- spline(x=log(x=adj_gdppc$gdppc,base=10),
                                       y=adj_gdppc$perdose_gdppc_percent,
                                       n = 1000)
spline_gdppc_percent_chepc_adj <- spline(x=log(x=adj_chepc$gdppc,base=10),
                                           y=adj_chepc$perdose_gdppc_percent,
                                         n=1000)

# ------------------------------------------------------------------------------
# Perspective of vaccine price per dose as % of CHE per capita
# ------------------------------------------------------------------------------

# Order df by GDP per capita
bilateral <- bilateral[order(bilateral$chepc),]
uniform <- uniform[order(uniform$chepc),]
adj_gdppc <- adj_gdppc[order(adj_gdppc$chepc),]
adj_chepc <- adj_chepc[order(adj_chepc$chepc),]

# Get the affordability (financial burden) variable for each df
bilateral <- add_affordability_percent(bilateral,'chepc')
uniform <- add_affordability_percent(uniform,'chepc')
adj_gdppc <- add_affordability_percent(adj_gdppc,'chepc')
adj_chepc <- add_affordability_percent(adj_chepc,'chepc')

# Splines for equity plots
spline_chepc_percent_bilateral <- spline(x=log(bilateral$chepc,base = 10),
                                           y=bilateral$perdose_chepc_percent, 
                                         n =1000)
spline_chepc_percent_uniform <- spline(x=log(uniform$chepc,base=10),
                                         y=uniform$perdose_chepc_percent,
                                       n=1000)
spline_chepc_percent_gdppc_adj <- spline(x=log(adj_gdppc$chepc,base=10),
                                           y=adj_gdppc$perdose_chepc_percent,
                                         n=1000)

spline_chepc_percent_chepc_adj <- spline(x=log(adj_chepc$chepc,base=10),
                                             y=adj_chepc$perdose_chepc_percent,
                                         n=1000)
