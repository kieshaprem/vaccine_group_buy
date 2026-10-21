library(rineq)

ci(ineqvar = bilateral_full$gdp_percapita,
  outcome = bilateral_full$perdose_gdppc_percent,
  type = "CIw")


ci(ineqvar = uniform_full$gdp_percapita,
   outcome = uniform_full$perdose_gdppc_percent,
   type = "CIw")


ci(ineqvar = adj_gdppc_full$gdp_percapita,
   outcome = adj_gdppc_full$perdose_gdppc_percent,
   type = "CIw")
