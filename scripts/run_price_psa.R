source('scripts/0_load_mi4a_data.R')
# PSA matrix for bilateral
# create matrix for storing PSA results
psa_mat_bilateral <- data.frame(array(dim=c(3,5000)))
rownames(psa_mat_bilateral) <- c('HIC','UMIC','LMIC')
for (i in (1:5000)){
  set.seed(i)
  psa_mat_bilateral[rownames(psa_mat_bilateral)=='HIC',i] <- runif(1,min = min(hic_prices), max=max(hic_prices))
  psa_mat_bilateral[rownames(psa_mat_bilateral)=='UMIC',i] <- runif(1,min = min(umic_prices), max=max(umic_prices))
  # minimum for lmic is set as 5.00 as we assumed that will be the price received by LMICs if they also buy AstraZeneca
  psa_mat_bilateral[rownames(psa_mat_bilateral)=='LMIC',i] <- runif(1,min = min(hic_prices), max=max(lmic_prices))
}
hic_psa <- unlist(psa_mat_bilateral['HIC',])
umic_psa <- unlist(psa_mat_bilateral['UMIC',])
lmic_psa <- unlist(psa_mat_bilateral['LMIC',])


# PSA matrix for alternate mechanisms
# create matrix for storing PSA results
psa_mat_alternate <- data.frame(array(dim=c(1,5000)))
for (i in (1:5000)){
  set.seed(i)
  psa_mat_alternate[1,i] <- runif(1,min = min(wpro_lmic_prices), max=max(wpro_lmic_prices))
}
alternate_psa <- unlist(psa_mat_alternate[1,])

save(hic_psa,file = 'output/intermediate/hic_psa.rdata')
save(umic_psa,file = 'output/intermediate/umic_psa.rdata')
save(lmic_psa,file = 'output/intermediate/lmic_psa.rdata')
save(alternate_psa,file = 'output/intermediate/alternate_psa.rdata')