# Items below are run for PSA calculations

# ------------------------------------------------------------------------------
# 1. Sample prices from their empirical distribution
# ------------------------------------------------------------------------------
# Output: output/intermediate/psa_price_base.rdata
# ------------------------------------------------------------------------------
source('scripts/1_1_psa_price.R')

# ------------------------------------------------------------------------------
# 2. Apply sampled prices to calculations on expenditure and savings at the
# regional and country level, applying 100% vaccination coverage assumption 
# and real vaccination coverage
# ------------------------------------------------------------------------------
# Input: output/intermediate/psa_price_base.rdata
# ------------------------------------------------------------------------------
# Outputs:
## 100% vax coverage
### output/intermediate/psa_outcomes_base.rdata (outcomes at regional level)
### output/intermediate/psa_countrylevel_base.rdata (outcomes at country level)
## real vax coverage
### output/intermediate/psa_outcomes_real.rdata (outcomes at regional level)
### output/intermediate/psa_countrylevel_real.rdata (outcomes at country level)
# ------------------------------------------------------------------------------
source('scripts/1_2_psa_outcome.R')


