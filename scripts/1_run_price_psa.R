source('scripts/0_load_mi4a_data.R')
set.seed(123)

rtri <- function(n, a, b, c) { # a=min, b=max, c=mode / median
  u <- runif(n)
  fc <- (c - a) / (b - a)
  out <- numeric(n)
  for (i in 1:n) {
    if (u[i] < fc) {
      out[i] <- a + sqrt(u[i] * (b - a) * (c - a))
    } else {
      out[i] <- b - sqrt((1 - u[i]) * (b - a) * (b - c))
    }
  }
  return(out)
}

# create matrix for storing PSA results
psa_mat <- data.frame(array(dim=c(5,10000)))
rownames(psa_mat) <- c('HIC','UMIC','LMIC','Alternate','alpha')
for (i in (1:10000)){
  psa_mat[rownames(psa_mat)=='HIC',i] <- runif(1,min = min(hic_prices), 
                                               max=max(hic_prices))
  psa_mat[rownames(psa_mat)=='UMIC',i] <- runif(1,min = min(umic_prices), 
                                                max=max(umic_prices))
  # minimum for lmic is set as 5.00 as we assumed that will be the price received by LMICs 
  # if they also buy AstraZeneca
  psa_mat[rownames(psa_mat)=='LMIC',i] <- runif(1,min = min(hic_prices), 
                                                max=max(lmic_prices))
  psa_mat[rownames(psa_mat)=='Alternate',i] <- runif(1,min = min(wpro_lmic_prices), 
                                                     max=max(wpro_lmic_prices))
  #psa_mat[rownames(psa_mat)=='alpha',i] <- runif(1,min = 0, max=20)
}

save(psa_mat, file = 'output/intermediate/psa_mat_alpha.rdata')


# sampling from empirical distribution
set.seed(123)
psa_mat <- data.frame(array(dim=c(5,10000)))
rownames(psa_mat) <- c('HIC','UMIC','LMIC','Alternate','alpha')
for (i in (1:10000)){
  psa_mat[rownames(psa_mat)=='HIC',i] <- sample(hic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='UMIC',i] <- sample(umic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='LMIC',i] <- sample(lmic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='Alternate',i] <- sample(wpro_lmic_prices, 1, replace = TRUE)
  #psa_mat[rownames(psa_mat)=='alpha',i] <- runif(1,min = 0, max=20)
}

save(psa_mat, file = 'output/intermediate/psa_mat_alpha_empirical_others.rdata')


# sampling from empirical distribution
set.seed(123)
psa_mat <- data.frame(array(dim=c(6,10000)))
rownames(psa_mat) <- c('HIC','UMIC','LMIC','Alternate','Pmin','alpha')
for (i in (1:10000)){
  psa_mat[rownames(psa_mat)=='HIC',i] <- sample(hic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='UMIC',i] <- sample(umic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='LMIC',i] <- sample(lmic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='Alternate',i] <- sample(wpro_lmic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='Pmin',i] <- sample(wpro_lmic_prices, 1, replace = TRUE)
  #psa_mat[rownames(psa_mat)=='alpha',i] <- runif(1,min = 0, max=20)
}
save(psa_mat, file = 'output/intermediate/psa_mat_pmin.rdata')

load('output/intermediate/psa_mat_pmin.rdata')

# sampling from empirical distribution qith assumption that 
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
  umic_distribution <- umic_prices[umic_prices<hic_i]
  if (length(umic_distribution)>1){
    umic_i <- sample(umic_distribution, 1, replace = TRUE)
  } else {
        diff <- base_hic - base_umic
        umic_i <- round(hic_i-hic_i*diff/base_hic,2)
  }
  psa_mat[rownames(psa_mat)=='UMIC',i] <- umic_i
  
  lmic_distribution <- lmic_prices[which(lmic_prices<umic_i)]
  if (length(lmic_distribution)>1){
    lmic_i <- sample(lmic_distribution, 1, replace = TRUE)
  } else {
    diff <- base_umic - base_lmic
    lmic_i <- round(umic_i-umic_i*diff/base_umic,2)
  }
  psa_mat[rownames(psa_mat)=='LMIC',i] <- lmic_i
  
  uniform_distribution <- wpro_lmic_prices[which(wpro_lmic_prices<lmic_i)]
  if (length(uniform_distribution)>1){
    uniform_i <- sample(uniform_distribution, 1, replace = TRUE)
  } else {
    diff <- base_lmic - base_uniform
    uniform_i <- round(lmic_i-lmic_i*diff/base_lmic,2)
  }
  psa_mat[rownames(psa_mat)=='Uniform',i] <- uniform_i
  
  adj_distribution <- wpro_lmic_prices[which(wpro_lmic_prices<uniform_i)]
  if (length(adj_distribution)>1){
    adj_i <- sample(adj_distribution, 1, replace = TRUE)
  } else {
        diff <- base_uniform - base_pmin
        adj_i <- round(uniform_i-uniform_i*diff/base_uniform,2)
  }
  psa_mat[rownames(psa_mat)=='Pmin',i] <- adj_i
}

save(psa_mat, file = 'output/intermediate/psa_mat_alpha_empirical_wassumption2.rdata')
