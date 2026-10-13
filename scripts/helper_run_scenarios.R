

run_scenario <- function(coverage,mode){
  outcome <- data.frame(array(dim = c(10,10000)))
  rownames(outcome) <- c('bilateral_total','uniform_total','adj_gdppc_total',
                         'adj_chepc_total',
                         'savings_uniform_percent','savings_adj_gdppc_percent',
                         'savings_adj_chepc_percent',
                         'savings_uniform','savings_adj_gdppc',
                         'savings_adj_chepc')
  if (mode == 'base'){
    # Base analysis
    bilateral_output <- prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                    mode = mode, comparator = NA)
    uniform_output <- prices_calc(mechanism = 'uniform', coverage = coverage, 
                                  mode = mode, comparator = bilateral_output$calc_output)
    adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                    mode = mode, comparator = bilateral_output$calc_output)
    adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                    mode = mode, comparator = bilateral_output$calc_output)
    save(bilateral_output,file = paste0('output/final_output/bilateral_',coverage,'_',mode,'.rdata'))
    save(uniform_output,file = paste0('output/final_output/uniform_',coverage,'_',mode,'.rdata'))
    save(adj_gdppc_output,file = paste0('output/final_output/adj_gdppc_',coverage,'_',mode,'.rdata'))
    save(adj_chepc_output,file = paste0('output/final_output/adj_chepc_',coverage,'_',mode,'.rdata'))
    
  } else if (mode == 'alpha_min'){
    # Sensitivity analysis: alpha = 0 for tiered pricing
    adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output,alpha = 0)
    adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output, alpha = 0)
    save(adj_gdppc_output,file = paste0('output/final_output/adj_gdppc_',coverage,'_',mode,'.rdata'))
    save(adj_chepc_output,file = paste0('output/final_output/adj_chepc_',coverage,'_',mode,'.rdata'))
  } else if (mode =='alpha_max'){
    # Sensitivity analysis: alpha = 20 for tiered pricing
    adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output,alpha = 20)
    adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output, alpha = 20)
    save(adj_gdppc_output,file = paste0('output/final_output/adj_gdppc_',coverage,'_',mode,'.rdata'))
    save(adj_chepc_output,file = paste0('output/final_output/adj_chepc_',coverage,'_',mode,'.rdata'))
  } else {
    # PSA mode
    for (i in 1:10000){
      print(i)
      bilateral_output <- prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                      mode = mode, comparator = NA, psa_number = i)
      uniform_output <- prices_calc(mechanism = 'uniform', coverage = coverage, 
                                    mode = mode, comparator = bilateral_output$calc_output, psa_number = i)
      adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                      mode = mode, comparator = bilateral_output$calc_output, psa_number = i)
      adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                      mode = mode, comparator = bilateral_output$calc_output, psa_number = i)
      outcome['bilateral_total',paste0("X",i)] <- sum(bilateral_output$calc_output$total_perdose)
      outcome['uniform_total',paste0("X",i)] <- sum(uniform_output$calc_output$total_perdose)
      outcome['adj_gdppc_total',paste0("X",i)] <- sum(adj_gdppc_output$calc_output$total_perdose)
      outcome['adj_chepc_total',paste0("X",i)] <- sum(adj_chepc_output$calc_output$total_perdose)
      
      outcome['savings_uniform',paste0("X",i)] <- sum(uniform_output$calc_output$total_savings_perdose)
      outcome['savings_adj_gdppc',paste0("X",i)] <- sum(adj_gdppc_output$calc_output$total_savings_perdose)
      outcome['savings_adj_chepc',paste0("X",i)] <- sum(adj_chepc_output$calc_output$total_savings_perdose)
      
      outcome['savings_uniform_percent',paste0("X",i)] <- outcome['savings_uniform',paste0("X",i)]*100/
        sum(bilateral_output$calc_output$total_perdose)
      outcome['savings_adj_gdppc_percent',paste0("X",i)] <- outcome['savings_adj_gdppc',paste0("X",i)]*100/
        sum(bilateral_output$calc_output$total_perdose)
      outcome['savings_adj_chepc_percent',paste0("X",i)] <- outcome['savings_adj_chepc',paste0("X",i)]*100/
        sum(bilateral_output$calc_output$total_perdose)
     
    }
    return(outcome)
  }
}

run_country_level_psa <- function(psa_mat, mechanism, coverage, comparator_df = NA){
  outcome <- data.frame(array(dim = c(33,10000)))
  rownames(outcome) <- c(sapply(main_data$iso3c,function(x){paste0(x,'_total')}),
                         sapply(main_data$iso3c,function(x){paste0(x,'_savings')}),
                         sapply(main_data$iso3c,function(x){paste0(x,'_percent')}))
  
  total_rowid <- which(sapply(rownames(outcome),function(x){substr(x,5,nchar(x))=='total'}))
  percentshare_rowid <- which(sapply(rownames(outcome),function(x){substr(x,5,nchar(x))=='percent_share'}))
  if (mechanism!='bilateral'){
    savings_rowid <- which(sapply(rownames(outcome),function(x){substr(x,5,nchar(x))=='savings'}))
  }
  
  for (i in 1:10000){
    if (mechanism!='bilateral'){
      comparator_df <- prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                   mode = 'psa', comparator = NA, psa_number = i)$calc_output
    }
    df <- prices_calc(mechanism = mechanism, coverage = coverage, 
                      mode = 'psa', comparator = comparator_df, psa_number = i)$calc_output
    outcome[total_rowid,paste0("X",i)] <- df$total_perdose
    outcome[percentshare_rowid,paste0("X",i)] <- df$percent_share
    if (mechanism!='bilateral'){
      outcome[savings_rowid,paste0("X",i)] <- df$total_savings_perdose
    }
  }
  return(outcome)
}