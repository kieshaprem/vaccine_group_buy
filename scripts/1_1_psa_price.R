source('scripts/0_load_mi4a_data.R')
set.seed(123)

# sampling from empirical distribution with assumption that 
# bilateral is always higher than alternate
# Pmin is always lower than uniform
set.seed(123)
psa_mat <- data.frame(array(dim=c(5,10000)))
rownames(psa_mat) <- c('HIC','UMIC','LMIC','Uniform','Pmin')
for (i in (1:10000)){
  base_pmin <- quantile(wpro_lmic_prices,probs = c(0.25))
  base_uniform <- median(wpro_lmic_prices)
  base_umic <- median(umic_prices_bilateral)
  base_hic <- median(hic_prices_bilateral)
  base_lmic <- median(lmic_prices_bilateral)
  
  hic_i <- sample(hic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='HIC',i] <- hic_i
  
  # apply assumption that UMIC price < HIC price
  umic_distribution <- umic_prices[umic_prices<hic_i]
  if (length(umic_distribution)>1){
    umic_i <- sample(umic_distribution, 1, replace = TRUE)
  } else {
        diff <- base_hic - base_umic
        # if sampled HIC price is low and no possible value from 
        # empirical distribution of UMIC price is available,
        # apply the proportion of value difference b/w
        # HIC and UMIC, then scale using sampled HIC price
        umic_i <- round(hic_i-hic_i*diff/base_hic,2)
  }
  psa_mat[rownames(psa_mat)=='UMIC',i] <- umic_i
  
  # apply assumption that LMIC price < UMIC price
  lmic_distribution <- lmic_prices[which(lmic_prices<umic_i)]
  if (length(lmic_distribution)>1){
    lmic_i <- sample(lmic_distribution, 1, replace = TRUE)
  } else {
    diff <- base_umic - base_lmic
    # if sampled UMIC price is low and no possible value from 
    # empirical distribution of LMIC price is available,
    # apply the proportion of value difference b/w
    # UMIC and LMIC, then scale using sampled UMIC price
    lmic_i <- round(umic_i-umic_i*diff/base_umic,2)
  }
  psa_mat[rownames(psa_mat)=='LMIC',i] <- lmic_i
  
  # apply assumption that uniform price < LMIC price
  uniform_distribution <- wpro_lmic_prices[which(wpro_lmic_prices<lmic_i)]
  if (length(uniform_distribution)>1){
    uniform_i <- sample(uniform_distribution, 1, replace = TRUE)
  } else {
    diff <- base_lmic - base_uniform
    # if sampled LMIC price is low and no possible value from 
    # empirical distribution for uniform price is available,
    # apply the proportion of value difference b/w
    # LMIC and uniform price, then scale using sampled LMIC price
    uniform_i <- round(lmic_i-lmic_i*diff/base_lmic,2)
  }
  psa_mat[rownames(psa_mat)=='Uniform',i] <- uniform_i
  
  # apply assumption that Pmin < uniform price
  adj_distribution <- wpro_lmic_prices[which(wpro_lmic_prices<uniform_i)]
  if (length(adj_distribution)>1){
    adj_i <- sample(adj_distribution, 1, replace = TRUE)
  } else {
        diff <- base_uniform - base_pmin
        # if sampled uniform price is low and no possible value from 
        # empirical distribution for Pmin is available,
        # apply the proportion of value difference b/w
        # uniform price and Pmin, then scale using sampled uniform price
        adj_i <- round(uniform_i-uniform_i*diff/base_uniform,2)
  }
  psa_mat[rownames(psa_mat)=='Pmin',i] <- adj_i
}

save(psa_mat, file = 'output/intermediate/psa_price_base.rdata')
