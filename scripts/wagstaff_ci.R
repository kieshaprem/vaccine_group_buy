library(rineq)

bilateral_poor_rich <- bilateral[10:1,]
bilateral_poor_rich <- merge(bilateral_poor_rich,
                             dataset[,c("Country","GDP per capita (2022)")],
                             by="Country",all.x=T)
bilateral_poor_rich <- bilateral_poor_rich[order(bilateral_poor_rich$`GDP per capita (2022)`),]

ci(ineqvar = bilateral_poor_rich$`GDP per capita (2022)`,
  outcome = bilateral_poor_rich$perdose_gdppc_percent,
  type = "CIw")

uniform_poor_rich <- uniform
uniform_poor_rich <- merge(uniform_poor_rich,
                       dataset[,c("Country","GDP per capita (2022)")],
                       by="Country",all.x=T)
uniform_poor_rich <- uniform_poor_rich[order(uniform_poor_rich$`GDP per capita (2022)`),]

ci(ineqvar = uniform_poor_rich$`GDP per capita (2022)`,
   outcome = uniform_poor_rich$perdose_gdppc_percent,
   type = "CIw")

gdp_adj_poor_rich <- gdp_adj
gdp_adj_poor_rich <- merge(gdp_adj_poor_rich,
                           dataset[,c("Country","GDP per capita (2022)")],
                           by="Country",all.x=T)
gdp_adj_poor_rich <- gdp_adj_poor_rich[order(gdp_adj_poor_rich$`GDP per capita (2022)`),]

ci(ineqvar = gdp_adj_poor_rich$`GDP per capita (2022)`,
   outcome = gdp_adj_poor_rich$perdose_gdppc_percent,
   type = "CIw")

bilateral <- bilateral_poor_rich
uniform <- uniform_poor_rich
gdp_adj <- gdp_adj_poor_rich
