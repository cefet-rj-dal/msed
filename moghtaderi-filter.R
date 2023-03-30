library(EMD)


get_overrall_adaptative_trend <- function(returns, i_){
  overral_adaptative_trend <- vector()
  print(returns$nimf)
  for (iimf in i_:returns$nimf){
    if(length(overral_adaptative_trend) == 0){
      overral_adaptative_trend <- returns$imf[,iimf]
      next
    }
    overral_adaptative_trend = overral_adaptative_trend + returns$imf[,iimf]
  }
  return (overral_adaptative_trend + returns$residue)
}

get_shock_significants_events <- function(returns, i_){
  shock_significants_events <- vector()
  
  for (iimf in i_:returns$nimf){
    if(length(shock_significants_events) == 0){
      shock_significants_events <- returns$imf[,iimf]
      next
    }
    shock_significants_events = shock_significants_events + returns$imf[,iimf]
  }
  return (shock_significants_events)
}

get_short_term_flutuation <- function(returns, i_){
  short_term_flutuation <- vector()
  
  for (iimf in 1:i_){
    if(length(short_term_flutuation) == 0){
      short_term_flutuation <- returns$imf[,iimf]
      next
    }
    short_term_flutuation = short_term_flutuation + returns$imf[,iimf]
  }
  return (short_term_flutuation)
}

energy_ratio_approach = function(param_sup, param_inf, returns){
  high_short_fluctuation <- NULL
  ##Calculo RI
  limit_sup <- log2(param_sup)
  limit_inf <- log2(param_inf)
  
  gs <- vector()
  rs <- vector()
  
  Zsup <- 0
  Zinf <- 0
  
  for(i in 1:returns$nimf){
    gs <- append(gs,log2(abs((sum(returns$imf[,i])))^2))
    if(i >=1 ){
      Zinf <- strtoi(EMD::extrema(returns$imf[,i])[5])
      Zsup <- strtoi(EMD::extrema(returns$imf[,i-1])[5]) 
    }
    r <- Zsup/Zinf
    rs <- append(rs,log2(r))
  }
  
  print(rs)
  print(gs)

  is_gi <- vector()
  if (length(gs) > 0){
    for(i in 2:length(gs)){
        if (gs[i] > gs[i-1]){
          is_gi <- append(is_gi,i)
        }
      }
  }
  print(is_gi)
  
  is_ri <- vector()
  if (length(rs) > 0){
    for(i in 2:length(rs)){
      if (rs[i] <= limit_sup && rs[i] >= limit_inf ){
        is_ri <- append(is_ri,i)
      }
    }
  }
  print(is_ri)
  i_ <- vector()
  
  for(gi in is_gi){
    for(ri in is_ri){
      if(gi==ri){
        i_ <- append(i_,gi)
      }
    }
  }
  
  if (length(i_) == 0){
    if (length(is_ri) > 0){
      i_ = min(is_ri)
    }
    
    if(length(is_gi) >0 ) {
      i_ = min(is_gi)
    }
  }
  if ( length(is_gi)==0 & length(is_gi)==0){
    i_ = 2
  }
  print("I energetico")
  print(i_)
  overral_adaptative_trend <- get_overrall_adaptative_trend(returns, i_)
  shock_significants_events <- get_shock_significants_events(returns, i_)
  short_term_flutuation <- get_short_term_flutuation(returns,i_)
  high_short_fluctuation <- returns$imf[,1]
  
  return(list(ve =  high_short_fluctuation , oat = overral_adaptative_trend, sst = shock_significants_events,
              stf = short_term_flutuation, lt = returns$residue ))
}