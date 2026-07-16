bilateral <- bilateral[,colnames(bilateral)!="total_percourse"]
bilateral$share <- bilateral$total_perdose*100/sum(bilateral$total_perdose)

uniform <- uniform[,colnames(uniform)!="total_percourse"]
uniform <- uniform[,colnames(uniform)!="total_savings_percourse"]
uniform <- uniform[,colnames(uniform)!="percourse_total_savings_percourse"]
uniform$share <- uniform$total_perdose*100/sum(uniform$total_perdose)

gdp_adj <- gdp_adj[,!colnames(gdp_adj) %in% c("total_percourse", "total_savings_percourse",
                                              "percourse_total_savings_percourse")]
gdp_adj$share <- gdp_adj$total_perdose*100/sum(gdp_adj$total_perdose)
