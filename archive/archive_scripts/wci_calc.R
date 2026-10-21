rank_gwt <- function(x, wt){  
  
  n = length(x)
  rank_order <- order(x)
  
  # order and rescale weights
  wt <- wt[rank_order]
  x  <- x[rank_order]
  
  # three times order/rank is not optimal, but the subsequent ranks should encounter a sorted vector
  rank_rank_min   <- rank(x, ties.method = "min") # position of minimum value for ties
  rank_rank_max   <- rank(x, ties.method = "max") # position of minimum value for ties
  
  cs <- cumsum(wt) 
  q <- cs[rank_rank_max] # calculate proportion with at least x_i (using weighted proportions)
  
  
  # shifted version - proportion for at least  x_l, where x_l is the next lowest value for position i
  q_s <- c(0, cs)[rank_rank_min]
  
  # calculate normalized generalized rank and return
  rg <- (q_s + 0.5*(q-q_s))/sum(wt)
  
  # return in original order: to sort back we need to take order again
  rg[order(rank_order)]
}

ci <- function(ineqvar, outcome, weights = NULL, type = c("CI", "CIg", "CIc", "CIw"), 
               method = c("linreg_delta", "linreg_convenience", "cov_convenience", "direct"), 
               df_correction = TRUE, robust_se = FALSE, rse_type = "HC3", rank_function = rineq::rank_wt) {
  argname_outcome <-deparse(substitute(outcome))
  argname_ineqvar <-deparse(substitute(ineqvar))
  
  # check type argument
  type <- match.arg(type)
  method <- match.arg(method)
  

  
  # if no wt argument is given (default), set all sampling weights to 1.
  # sum(weights) = 1, see Lerman & Yitzhaki (1989), par. 2
  weights <- rep(1, length(ineqvar))
  
  # pack in dataframe 
  cdf <- data.frame(ineqvar = ineqvar, outcome = outcome, weights = weights)
  
  # ineqvar can be incomplete, but weights and outcome should be complete for calcs:
  # cdf <- cdf[ !is.na(cdf$weights) & !is.na(cdf$outcome),]
  cdf <- na.omit(cdf)
  n <- nrow(cdf)
  cdf$weights_norm = cdf$weights / sum(cdf$weights)
  
  # calculate weighted average of the health variable
  mean_outcome <- weighted.mean(cdf$outcome, w = cdf$weights)
  
  # calculate weighed rank of the wealth variable
  cdf$weighted_rank <- rank_gwt(x = cdf$ineqvar, wt = cdf$weights)
  
  # correction factor for sample vs population degree of freedoms 
  if(df_correction) cf <- (n-1)/n else cf = 1
  
  # calculate average rank
  meanw_rank  <- mean(cdf$weighted_rank)
  
  # variance of weighted rank
  var_ranku = var_wt(cdf$weighted_rank, wt = cdf$weights, na.rm = T)
  var_ranku_notls <- var_wt(weighted_rank_notls,wt=rep(1,10))
  ###### actual core calulations ###############
  if(method == "linreg_delta")
  {
    # follow 8.12 in world bank doc to also get  standard error
    cdf$wo = cdf$weights*cdf$outcome
    
    mod_lm_notls = lm(bilateral_full[bilateral_full$iso3c!='TLS',]$perdose_gdppc_percent ~ 
                        weighted_rank_notls)
    
    # calculate the index using the delta method
    
    #for illustration purposes define the g() function. 
    # grad is are the partial derivatives with respect to a and b
    g = function(a,b, const_var) { 2*const_var * b / (a+b/2) } 
    
    a_notls = coef(mod_lm)[["(Intercept)"]]
    b_notls = coef(mod_lm)[["weighted_rank"]]
    
    
    dterm = 8* var_ranku / (2*a+b)^2
    grad = c(-b*dterm, a*dterm)  
    cf=1
    concentration_index = g(a,b, var_ranku)*cf
    con_index_notls <- g(a_notls,b_notls,var_ranku_notls)
    if(robust_se) vcov = sandwich::vcovHC(mod_lm, type=rse_type)
    else vcov = vcov(mod_lm)
    
    varC = (t(grad) %*% vcov  %*% grad)[1,1]
    
  }else if(method == "linreg_convenience"){
    
    lhsu = 2*var_ranku*(cdf$outcome/mean_outcome)
    
    dm = data.frame(y = lhsu, x = cdf$weighted_rank)
    mod_lm = lm(y ~ x, data = dm, weights = cdf$weights) # dont forget weighting
    concentration_index = coef(mod_lm)[["x"]]*cf
    
    if(robust_se) vcov = sandwich::vcovHC(mod_lm, type=rse_type)
    else vcov = vcov(mod_lm)
    
    varC = vcov[2,2]
    
  }else if(method == "cov_convenience"){
    
    cv = cov.wt(cbind(cdf$outcome, cdf$weighted_rank), cdf$weights)
    
    
    concentration_index = (2/mean_outcome)*cv$cov[1,2] *cf
    
    varC = cv$cov[2,2]
  }else if(method == "direct"){
    
    # order df
    o <- order(cdf$weighted_rank)
    ordered_df <- cdf[o,]
    
    
    concentration_index <- 2 * sum(cdf$weights_norm * (cdf$outcome - mean_outcome) * (cdf$weighted_rank - meanw_rank) / mean_outcome)
    
    
    
    # calculate variance of the ci for weighted microcase data
    # see Kakwani et al. (eq 14) with qi = qt, R = ft and y = x (orig)
    # or worldbank document 8.9
    qi <- cumsum(ordered_df$outcome) / sum(ordered_df$outcome)
    qilag <- c(0, qi[-n])
    ai <- ordered_df$outcome / mean_outcome * (2 * ordered_df$weighted_rank - 1 - concentration_index) + 
      2 - qilag - qi
    varC <- (sum(ai^2) / n - (1 + concentration_index)^2) / n
    
  }
  else
  {
    stop("unkown method")  
  }
  
  
  ###### calculate the different types of concentration index #############
  
  if (is.null(type)) 
  {
    
    concentration_index <- concentration_index
    type = "CI"	
    
  } else if (type == "CI"){
    
    # relative
    concentration_index <- concentration_index
    
  } else if (type == "CIg"){
    
    # generalized
    concentration_index <- concentration_index *  mean_outcome
    
    varC = varC * mean_outcome^2 # treat as constant
    
  } else if (type == "CIc"){
    
    # Erreygers correction
    f = 4 * mean_outcome / (max(outcome, na.rm = TRUE) - min(outcome, na.rm = TRUE))
    concentration_index <- concentration_index * f
    varC = varC* f^2 # treat as constant
    
  } else if(type == "CIw"){
    
    # Wagstaff
    concentration_index <- concentration_index/(1-mean_outcome)
    varC = varC * (1/(1-mean_outcome))^2 # treat as constant
  }
  
  # return an object of class hci
  ci <-
    list(concentration_index = concentration_index,
         type = type,
         method = method,
         variance = varC, 
         fractional_rank = cdf$weighted_rank,
         outcome = cdf$outcome,
         ineqvar = cdf$ineqvar,
         call =  deparse(match.call()),
         argname_outcome = argname_ineqvar,
         argname_ineqvar = argname_outcome,
         n = n, 
         robust_se = robust_se, 
         rse_type = rse_type,
         df_correction = df_correction)
  class(ci) <- "hci"
  return(ci)
}


line_fit <- lm(bilateral_full[bilateral_full$iso3c!='TLS',]$perdose_gdppc_percent~bilateral_full[bilateral_full$iso3c!='TLS',]$gdp_percapita)
line_fit_new <- lm(bilateral_full$perdose_gdppc_percent~bilateral_full$gdp_percapita)

plot(bilateral_full$gdp_percapita,bilateral_full$perdose_gdppc_percent)
abline(line_fit)
abline(line_fit_new)
