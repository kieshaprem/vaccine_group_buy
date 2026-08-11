source('scripts/1_calculation_function.R')
options(scipen=999)

full_outcome_base <- run_scenario(coverage = 'full', mode = 'psa')
save(full_outcome_base,file = 'output/intermediate/psa_outcomes_empirical_wassumption2.rdata')

country_level_psa <- get_country_level_psa_all(psa_mat = psa_mat, coverage = 'full')
save(country_level_psa,file = 'output/intermediate/psa_country_level_empirical_wassumption2.rdata')

new_outcome <- outcome
load('output/intermediate/psa_outcomes_empirical_other.rdata')

save(full_outcome_base,file = 'output/intermediate/psa_outcomes_empirical_other.rdata')

real_outcome_base <- run_scenario(coverage = 'real', mode = 'psa')
save(real_outcome_base,file = 'output/intermediate/psa_outcomes_empirical_wassumption2_real.rdata')

country_level_psa <- get_country_level_psa_all(psa_mat = psa_mat, coverage = 'real')
save(country_level_psa,file = 'output/intermediate/psa_country_level_empirical_wassumption2_real.rdata')


# psa with alpha
load('output/intermediate/psa_mat_alpha_empirical_full.rdata')
alpha_psa <- run_scenario(coverage = 'full', mode = 'psa')
save(alpha_psa, file = 'output/intermediate/alpha_psa_outcomes_full.rdata')
