# load('output/bilateral_full.rdata')
# load('output/bilateral_real.rdata')
# load('output/uniform_full.rdata')
# load('output/uniform_real.rdata')
# load('output/adj_gdppc_full.rdata')
# load('output/adj_gdppc_real.rdata')
# load('output/adj_gov_exp_full.rdata')
# load('output/adj_gov_exp_real.rdata')

source('scripts/0_process_calculations.R')
source('scripts/plot_2a-f.R')
library(countrycode)

# run sens analysis
source('scripts/sens_analysis_main_script.R')

bilateral_full <- bilateral_full_updated
bilateral_real <- bilateral_real_updated
uniform_full <- uniform_full_updated
uniform_real <- uniform_real_updated
adj_gdppc_full <- adj_gdppc_full_updated
adj_gdppc_real <- adj_gdppc_real_updated
adj_chepc_full <- adj_chepc_full_updated
adj_chepc_real <- adj_chepc_real_updated

bilateral_full <- bilateral_full[order(bilateral_full$gdp_percapita),]
bilateral_real <- bilateral_real[order(bilateral_real$gdp_percapita),]
uniform_full <- uniform_full[order(uniform_full$gdp_percapita),]
uniform_real <- uniform_real[order(uniform_real$gdp_percapita),]
adj_chepc_full <- adj_chepc_full[order(adj_chepc_full$gdp_percapita),]
adj_chepc_real <- adj_chepc_real[order(adj_chepc_real$gdp_percapita),]
adj_gdppc_full <- adj_gdppc_full[order(adj_gdppc_full$gdp_percapita),]
adj_gdppc_real <- adj_gdppc_real[order(adj_gdppc_real$gdp_percapita),]

bilateral_full <- add_chepc(bilateral_full)
bilateral_real <- add_chepc(bilateral_real)
uniform_full <- add_chepc(uniform_full)
uniform_real <- add_chepc(uniform_real)

bilateral_full <- add_affordability_percent(bilateral_full)
bilateral_real <- add_affordability_percent(bilateral_real)
uniform_full <- add_affordability_percent(uniform_full)
uniform_real <- add_affordability_percent(uniform_real)
adj_gdppc_full <- add_affordability_percent(adj_gdppc_full)
adj_gdppc_real <- add_affordability_percent(adj_gdppc_real)
adj_chepc_full <- add_affordability_percent(adj_chepc_full)
adj_chepc_real <- add_affordability_percent(adj_chepc_real)


# basic 2a-f
plot_coverage(coverage = 'full',price_adjustment = 'gdp_adj')
#plot_coverage(coverage = 'full',price_adjustment = 'gov_exp_adj')
plot_coverage(coverage = 'full',price_adjustment = 'chepc_adj')


plot_coverage(coverage = 'real',price_adjustment = 'gdp_adj')
plot_coverage(coverage = 'real',price_adjustment = 'gov_exp_adj')
plot_coverage(coverage = 'real',price_adjustment = 'chepc_adj')


#2a-f for comparing GDP per capita and CHE per capita
source('scripts/plot_2a-f_compare_gdp_gov_exp.R')
plot_compare_gdp_gov_exp(coverage = 'full')
plot_compare_gdp_gov_exp(coverage = 'real')

source('scripts/4_get_splines.R')

# 3a-f using GDPPC price adjustment
source('scripts/plot_3a-f.R')
get_plot_3af(weighting_mechanism = 'gdp_adj', affordability_measure = 'perdose_gdppc_percent')
get_plot_3af(weighting_mechanism = 'chepc_adj', affordability_measure = 'perdose_gdppc_percent')
get_plot_3af(weighting_mechanism = 'gdp_adj', affordability_measure = 'perdose_chepc_percent')
get_plot_3af(weighting_mechanism = 'chepc_adj', affordability_measure = 'perdose_chepc_percent')

get_plot_3af_bilateral(affordability_measure = 'perdose_gdppc_percent')
get_plot_3af_bilateral(affordability_measure = 'perdose_chepc_percent')

get_plot_3af_4cols('perdose_gdppc_percent')
get_plot_3af_4cols('perdose_chepc_percent')
