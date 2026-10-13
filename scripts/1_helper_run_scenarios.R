

run_scenario <- function(coverage,mode){
  outcome <- data.frame(array(dim = c(26,10000)))
  rownames(outcome) <- c('bilateral_total','uniform_total','adj_gdppc_total',
                         'adj_chepc_total',
                         'savings_uniform_percent','savings_adj_gdppc_percent',
                         'savings_adj_chepc_percent',
                         'savings_uniform','savings_adj_gdppc',
                         'savings_adj_chepc',
                         'bilateral_gdppc_wci','uniform_gdppc_wci',
                         'adj_gdppc_gdppc_wci','adj_chepc_gdppc_wci',
                         'bilateral_chepc_wci','uniform_chepc_wci',
                         'adj_gdppc_chepc_wci','adj_chepc_chepc_wci',
                         'bilateral_gdppc_ci','uniform_gdppc_ci',
                         'adj_gdppc_gdppc_ci','adj_chepc_gdppc_ci',
                         'bilateral_chepc_ci','uniform_chepc_ci',
                         'adj_gdppc_chepc_ci','adj_chepc_chepc_ci')
  if (mode == 'base'){
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
    adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output,alpha = 0)
    adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output, alpha = 0)
    save(adj_gdppc_output,file = paste0('output/final_output/adj_gdppc_',coverage,'_',mode,'.rdata'))
    save(adj_chepc_output,file = paste0('output/final_output/adj_chepc_',coverage,'_',mode,'.rdata'))
  } else if (mode =='alpha_max'){
    adj_gdppc_output <- prices_calc(mechanism = 'adj_gdppc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output,alpha = 20)
    adj_chepc_output <- prices_calc(mechanism = 'adj_chepc', coverage = coverage, 
                                    mode = 'base', comparator = bilateral_output, alpha = 20)
    save(adj_gdppc_output,file = paste0('output/final_output/adj_gdppc_',coverage,'_',mode,'.rdata'))
    save(adj_chepc_output,file = paste0('output/final_output/adj_chepc_',coverage,'_',mode,'.rdata'))
  } else {
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
      
      # outcome['bilateral_gdppc_wci',paste0("X",i)] <- get_wci(bilateral_output$calc_output,
      #                                                   perspective = 'gdppc')$concentration_index
      # outcome['uniform_gdppc_wci',paste0("X",i)] <- get_wci(uniform_output$calc_output,
      #                                                         perspective = 'gdppc')$concentration_index
      # outcome['adj_gdppc_gdppc_wci',paste0("X",i)] <- get_wci(adj_gdppc_output$calc_output,
      #                                                       perspective = 'gdppc')$concentration_index
      # outcome['adj_chepc_gdppc_wci',paste0("X",i)] <- get_wci(adj_chepc_output$calc_output,
      #                                                       perspective = 'gdppc')$concentration_index
      # outcome['bilateral_chepc_wci',paste0("X",i)] <- get_wci(bilateral_output$calc_output,
      #                                                         perspective = 'chepc')$concentration_index
      # outcome['uniform_chepc_wci',paste0("X",i)] <- get_wci(uniform_output$calc_output,
      #                                                       perspective = 'chepc')$concentration_index
      # outcome['adj_gdppc_chepc_wci',paste0("X",i)] <- get_wci(adj_gdppc_output$calc_output,
      #                                                         perspective = 'chepc')$concentration_index
      # outcome['adj_chepc_chepc_wci',paste0("X",i)] <- get_wci(adj_chepc_output$calc_output,
      #                                                         perspective = 'chepc')$concentration_index
      # 
      # outcome['bilateral_gdppc_ci',paste0("X",i)] <- get_ci(bilateral_output$calc_output,
      #                                                         perspective = 'gdppc')
      # outcome['uniform_gdppc_ci',paste0("X",i)] <- get_ci(uniform_output$calc_output,
      #                                                       perspective = 'gdppc')
      # outcome['adj_gdppc_gdppc_ci',paste0("X",i)] <- get_ci(adj_gdppc_output$calc_output,
      #                                                         perspective = 'gdppc')
      # outcome['adj_chepc_gdppc_ci',paste0("X",i)] <- get_ci(adj_chepc_output$calc_output,
      #                                                         perspective = 'gdppc')
      # 
      # outcome['bilateral_chepc_ci',paste0("X",i)] <- get_ci(bilateral_output$calc_output,
      #                                                         perspective = 'chepc')
      # outcome['uniform_chepc_ci',paste0("X",i)] <- get_ci(uniform_output$calc_output,
      #                                                       perspective = 'chepc')
      # outcome['adj_gdppc_chepc_ci',paste0("X",i)] <- get_ci(adj_gdppc_output$calc_output,
      #                                                         perspective = 'chepc')
      # outcome['adj_chepc_chepc_ci',paste0("X",i)] <- get_ci(adj_chepc_output$calc_output,
      #                                                         perspective = 'chepc')
      
    }
    return(outcome)
  }
}

run_country_level_psa <- function(psa_mat, mechanism, coverage, comparator_df = NA){
  outcome <- data.frame(array(dim = c(33,10000)))
  rownames(outcome) <- c(sapply(main_data$iso3c,function(x){paste0(x,'_total')}),
                         sapply(main_data$iso3c,function(x){paste0(x,'_savings')}),
                         sapply(main_data$iso3c,function(x){paste0(x,'_percent')}))
  for (i in 1:10000){
    if (mechanism!='bilateral'){
      comparator_df <- prices_calc(mechanism = 'bilateral', coverage = coverage, 
                                   mode = 'psa', comparator = NA, psa_number = i)$calc_output
    }
    df <- prices_calc(mechanism = mechanism, coverage = coverage, 
                      mode = 'psa', comparator = comparator_df, psa_number = i)$calc_output
    outcome[1:11,paste0("X",i)] <- df$total_perdose
    outcome[23:33,paste0("X",i)] <- df$percent_share
    if (mechanism!='bilateral'){
      outcome[12:22,paste0("X",i)] <- df$total_savings_perdose
    }
  }
  return(outcome)
}