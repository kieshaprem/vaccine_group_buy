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

# sampling from empirical distribution qith assumption that bilateral is always higher than alternate
set.seed(123)
psa_mat <- data.frame(array(dim=c(5,10000)))
rownames(psa_mat) <- c('HIC','UMIC','LMIC','Alternate','alpha')
for (i in (1:10000)){
  hic_i <- sample(hic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='HIC',i] <- hic_i
  umic_i <- sample(umic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='UMIC',i] <- umic_i
  lmic_i <- sample(lmic_prices, 1, replace = TRUE)
  psa_mat[rownames(psa_mat)=='LMIC',i] <- lmic_i
  min_price <- median(hic_i,umic_i,lmic_i)
  alternate_i <- sample(wpro_lmic_prices, 1, replace = TRUE)
  while (alternate_i>min_price){
    alternate_i <- sample(wpro_lmic_prices, 1, replace = TRUE)
  }
  psa_mat[rownames(psa_mat)=='Alternate',i] <- alternate_i
  psa_mat[rownames(psa_mat)=='alpha',i] <- runif(1,min = 0, max=20)
}

save(psa_mat, file = 'output/intermediate/psa_mat_alpha_empirical_wassumption.rdata')
