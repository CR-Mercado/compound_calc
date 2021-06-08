calc_returns <- function(initial, rate, compounds){ 
  initial * (1 + rate/compounds)^compounds
} 

rate = seq(from = 0.01, to = 3, by = .01)
compound_type = c("annual","semi","quarter","month","daily","continuous")

x = expand.grid(rate, compound_type)
colnames(x) <- c("rate","compound_type")

initial_val = 1000
vals = rep(0, nrow(x))

for(i in 1:nrow(x)){ 
  
  if(x$compound_type[i] == "annual"){ 
    vals[i] <- calc_returns(initial = initial_val, rate = x$rate[i],
                 compounds = 1) - initial_val
  }
  
  if(x$compound_type[i] == "semi"){ 
    vals[i] <- calc_returns(initial = initial_val, rate = x$rate[i],
                 compounds = 2) - initial_val
  }
  
  if(x$compound_type[i] == "quarter"){ 
    vals[i] <- calc_returns(initial = initial_val, rate = x$rate[i],
                 compounds = 4) - initial_val
  }
  
  if(x$compound_type[i] == "month"){ 
    vals[i] <- calc_returns(initial = initial_val, rate = x$rate[i],
                 compounds = 12) - initial_val
  }
  
  if(x$compound_type[i] == "daily"){ 
    vals[i] <- calc_returns(initial = initial_val, rate = x$rate[i],
                 compounds = 365) - initial_val
  }
  
  if(x$compound_type[i] == "continuous"){ 
    vals[i] <- initial_val * 2.7183^(x$rate[i])  - initial_val
  }
  
}
  

x$growth = vals

saveRDS(x, file = "rates_by_compound_granular.rds")
