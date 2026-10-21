percent_shares <- data.frame(cbind('country'=bilateral$iso3c,
                                   'bilateral'=bilateral$perdose_gdppc_percent,
                                   'uniform'=uniform$perdose_gdppc_percent,
                                   'adj_gdppc'=adj_gdppc$perdose_gdppc_percent,
                                   'adj_chepc'=adj_chepc$perdose_gdppc_percent))
percent_shares$bilateral <- as.numeric(percent_shares$bilateral)
percent_shares$uniform <- as.numeric(percent_shares$uniform)
percent_shares$adj_gdppc <- as.numeric(percent_shares$adj_gdppc)
percent_shares$adj_chepc <- as.numeric(percent_shares$adj_chepc)
percent_shares$adj_gdppc_max <- as.numeric(adj_gdppc_max$perdose_gdppc_percent)
percent_shares$adj_chepc_max <- as.numeric(adj_chepc_max$perdose_gdppc_percent)

percent_shares$change_uniform <- (percent_shares$uniform-percent_shares$bilateral)*100/percent_shares$bilateral
percent_shares$change_adj_gdppc <- (percent_shares$adj_gdppc-percent_shares$bilateral)*100/percent_shares$bilateral
percent_shares$change_adj_chepc <- (percent_shares$adj_chepc-percent_shares$bilateral)*100/percent_shares$bilateral

check_bilateral <- bilateral_output$calc_output
get_ci(check_bilateral,'gdppc')

percent_shares_chepc <- data.frame(cbind('country'=bilateral$iso3c,
                                   'bilateral'=bilateral$perdose_chepc_percent,
                                   'uniform'=uniform$perdose_chepc_percent,
                                   'adj_gdppc'=adj_chepc$perdose_chepc_percent,
                                   'adj_chepc'=adj_chepc$perdose_chepc_percent))
percent_shares_chepc$bilateral <- as.numeric(percent_shares_chepc$bilateral)
percent_shares_chepc$uniform <- as.numeric(percent_shares_chepc$uniform)
percent_shares_chepc$adj_gdppc <- as.numeric(percent_shares_chepc$adj_gdppc)
percent_shares_chepc$adj_chepc <- as.numeric(percent_shares_chepc$adj_chepc)


adj_gdppc_max <- adj_gdppc_max[order(adj_gdppc_max$gdppc),]
adj_gdppc_max <- add_affordability_percent(adj_gdppc_max,'gdppc')
adj_gdppc_max <- adj_gdppc_max[order(adj_gdppc_max$chepc),]
adj_gdppc_max <- add_affordability_percent(adj_gdppc_max,'chepc')
get_ci(adj_gdppc_max,perspective = 'gdppc')
get_ci(adj_chepc_max,perspective = 'gdppc')
get_ci(adj_gdppc_max,perspective = 'chepc')
get_ci(adj_chepc_max,perspective = 'chepc')

adj_chepc_max <- adj_chepc_max[order(adj_chepc_max$gdppc),]
adj_chepc_max <- add_affordability_percent(adj_chepc_max,'gdppc')
adj_chepc_max <- adj_chepc_max[order(adj_chepc_max$chepc),]
adj_chepc_max <- add_affordability_percent(adj_chepc_max,'chepc')

get_ci(adj_chepc_max,perspective = 'gdppc')
get_ci(adj_chepc_max,perspective = 'chepc')

get_ci(adj_gdppc_max,perspective = 'gdppc')
get_ci(adj_gdppc_max,perspective = 'chepc')

percent_shares_true <- data.frame(cbind('country'=bilateral$iso3c,
                                   'bilateral'=bilateral$percent_share,
                                   'uniform'=uniform$percent_share,
                                   'adj_gdppc'=adj_gdppc$percent_share,
                                   'adj_chepc'=adj_chepc$percent_share))
percent_shares$bilateral <- as.numeric(percent_shares$bilateral)
percent_shares$uniform <- as.numeric(percent_shares$uniform)
percent_shares$adj_gdppc <- as.numeric(percent_shares$adj_gdppc)
percent_shares$adj_chepc <- as.numeric(percent_shares$adj_chepc)
percent_shares$adj_gdppc_max <- as.numeric(adj_gdppc_max$perdose_gdppc_percent)
percent_shares$adj_chepc_max <- as.numeric(adj_chepc_max$perdose_gdppc_percent)

three_lowest_chepc <- percent_shares_true[percent_shares_true$country %in% c('MMR','LAO','KHM'),]
sum(as.numeric(three_lowest_chepc$adj_chepc))

two_highest <- percent_shares_true[percent_shares_true$country %in% c('BRN','SGP'),]
sum(as.numeric(two_highest$adj_chepc))
