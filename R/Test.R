library(lmtest)


# Investing Universe for each year
for (i in 1:Periods){
  tmp <- lookback_size_list[[i]][,-1]
  tmp <- tmp %>% select_if(~!all(is.na(.))) %>% select_if(~!all(.==0)) # select columns not full of 0s or NAs
  print(length(colnames(tmp)))
}

# the average stock number in each double-sorted portfolio
for(i in 1:Periods){
  for(j in c(1,2)){
    for (k in c(1,3)){
      l1 = length(intersect(SMB[[i]][[j]], HML[[i]][[k]]))
      l2 = length(intersect(SMB[[i]][[j]], RMW[[i]][[k]]))
      l3 = length(intersect(SMB[[i]][[j]], CMA[[i]][[k]]))
      min = min(l1, l2, l3)
    }
  }
  print(min)
}













