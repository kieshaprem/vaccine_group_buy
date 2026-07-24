adj_gdppc_below_lower <- psa_mat[,which(as.vector(unlist(full_outcome_base['savings_adj_gdppc_percent',]))<=-118.6)]
summary(as.vector(unlist(adj_gdppc_below_lower['HIC',])))
summary(as.vector(unlist(adj_gdppc_below_lower['UMIC',])))
summary(as.vector(unlist(adj_gdppc_below_lower['LMIC',])))
summary(as.vector(unlist(adj_gdppc_below_lower['Alternate',])))

uniform_higher <- psa_mat[,which(as.vector(unlist(full_outcome_base['savings_uniform_percent',]))>=94.3)]
summary(as.vector(unlist(uniform_higher['HIC',])))
summary(as.vector(unlist(uniform_higher['UMIC',])))
summary(as.vector(unlist(uniform_higher['LMIC',])))
summary(as.vector(unlist(uniform_higher['Alternate',])))
