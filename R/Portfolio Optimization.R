####Outline####
#### Purpose: Constructing a series of low volatility portfolios by using different weighting strategies other than equal-weighted and market capitalization-weighted.

#### Context

#### Methods: In this section, we use seven weighted-methods to cons

#### Results

#### Valuation

rm(list = ls())
####Load Required Packages####
require(readxl)
require(xts)
require(ggplot2)
require(dplyr)
require(tidyverse)
require(pracma)

####Import Data####
df_price <- read_excel('Price.xlsx')
df_size <- read_excel("Market Capitalizations.xlsx")
df_MR <- read_excel("J203T Price.xlsx", col_types = c("date", "numeric", "skip", "skip", "skip", "skip", "skip"))
df_RF <- read_excel("91days T-Bill rate.xlsx")

####Sorting Data####
#### Reform df_price
tmp <- data.frame()
for (i in 1:(nrow(df_price)/132)) {
    tmp[1:132,i] = df_price[(1+132*(i-1)):((132*i)),3]
}

tmp_cnames <- vector()
range <- seq(from = 1, to = nrow(df_price), by = 132)
for (i in 1:(nrow(df_price)/132)) {
    x <- range[i]
    tmp_cnames[i] <- df_price[x,2]
}

colnames(tmp) <- tmp_cnames
tmp <- cbind(df_price[1:132,1], tmp)
df_price <- tmp
df_price <- df_price %>% remove_rownames %>% column_to_rownames(var="Date")

#### Reform df_size
tmp <- df_price
for (i in 1:(nrow(df_size)/132)) {
  tmp[1:132,i] = df_size[(1+132*(i-1)):((132*i)),4]
}

df_size <- tmp

# change the format of rownames (dates)
df_price <- df_price[nrow(df_price):1, ] # reorder the dataframe
df_size <- df_size[nrow(df_size):1, ] # reorder the dataframe
rownames(df_price) <- format(as.Date(rownames(df_price)),format  = "%Y/%m")
rownames(df_size) <- format(as.Date(rownames(df_size)),format  = "%Y/%m")

rm(tmp, tmp_cnames, range)
#### Reform df_RF and df_MR
date1 <- df_RF$Date
df_RF <- as.numeric(df_RF$Value)
df_RF <- (1+df_RF/100)^(1/365)-1
df_RF <- as.data.frame(cbind(Dates=date1,Value=df_RF))
df_RF$Dates <- as.Date(df_RF$Dates)
rm(date1)

date2 <- as.data.frame(df_MR$Dates)
df_MR <- as.data.frame(lapply(df_MR[,-1],as.numeric))
df_MR <- as.data.frame(log(df_MR[2:nrow(df_MR),]/df_MR[1:(nrow(df_MR)-1),])) # log return
# df_MR <- as.data.frame(df_MR[2:nrow(df_MR),]-df_MR[1:(nrow(df_MR)-1),])/df_MR[1:(nrow(df_MR)-1),] # simple return
df_MR <- cbind(Dates=date2[-1,],df_MR)
colnames(df_MR) <- c("Dates","MR")
rm(date2)

df_MR$Dates <- format(as.Date(df_MR$Dates),format  = "%Y/%m")
df_MR <-df_MR %>%
  group_by(Dates) %>%
  summarize(MR = sum(MR))
df_MR <- subset(df_MR, Dates %in% rownames(df_price[-1,]))

df_RF$Dates <- format(as.Date(df_RF$Dates),format  = "%Y/%m")
df_RF <-df_RF %>%
  group_by(Dates) %>%
  summarize(Value = sum(as.numeric(Value)))
df_RF <- subset(df_RF, Dates %in% rownames(df_price[-1,]))

df_MR$MR <- df_MR$MR - df_RF$Value
colnames(df_MR) <- c("Dates", "MR_RF")

df_MR <- as.data.frame(df_MR[,-1])
rownames(df_MR) <- rownames(df_price[-1,])

df_RF <- as.data.frame(df_RF[,-1])
rownames(df_RF) <- rownames(df_price[-1,])

#### Calculate the stock excess returns
df_SR <- log(df_price[2:nrow(df_price),]/df_price[1:(nrow(df_price)-1),]) # log return
# df_SR <- (df_price[2:nrow(df_price),]-df_price[1:(nrow(df_price)-1),])/df_price[1:(nrow(df_price)-1),] # simple return
df_SR[df_SR==Inf] <- NA

f1<-function(x){
  x-df_RF$Value
}
df_Ex_SR <- apply(df_SR, f1, MARGIN = 2)

####CAPM (Time-Varying Betas)####
holding = 1 # Define the rebalance frequency
rollingW = 36 # Define the length of lookback estimation window
lens = nrow(df_Ex_SR)
periods = as.integer((lens - rollingW) / holding) # Calculate the holding periods
print(c(periods,rollingW))

# Run time-series regression under the standard CAPM model to estimate market betas
all_betas <- vector(mode = "list", length = periods)

for (i in 1:periods){
  df_MR_tmp <- as.data.frame(df_MR[(i*holding):(i*holding+rollingW),,drop = F])
  df_Ex_SR_tmp <- df_Ex_SR[(i*holding):(i*holding+rollingW),]
  
  betas <- vector()
  labels <- vector()
  for (j in colnames(df_Ex_SR)){
    df_index = names(na.omit(df_Ex_SR_tmp[,j]))
    if (length(df_index) >= 2) { # We can only draw a line based on two points or above
      b = coef(lm(df_Ex_SR_tmp[df_index,j]~df_MR_tmp[df_index,]))[2] # [2] represents for the second coef in the regression model
      labels <- c(labels,j)
      betas <- c(betas,b)
    }
  }
  df_betas <- data.frame(t(betas))
  colnames(df_betas) <- labels
  all_betas[[i]] <- df_betas
}

rm(df_Ex_SR_tmp, df_MR_tmp, df_betas)
####Order the betas####

####Test: Rank shares according to their scores
beta_order <- order(unlist(all_betas[[1]]), na.last = NA)
names(beta_order) <- names(all_betas[[1]])
beta_order
names(beta_order)[beta_order]

####Generalized lookback period calculations
HML <- vector(mode = "list", length = periods)

HML <- lapply(all_betas, function(x){
  
  mean_Betas <- sapply(x, function(y){ 
  #calculate the mean of betas for each company
  ans <- mean(unlist(y), na.rm = T)
  names(ans) <- names(y)
  return(ans)
  })
  
  order_betas <- order(mean_Betas, na.last = NA) 
  names(order_betas) <- names(x)

  number_of_portfolios = 5
  N <- length(order_betas)
  quantile = trunc(N/number_of_portfolios)
  
  return(list(Low_Beta=names(order_betas)[order_betas][1:quantile],
              P1=names(order_betas)[order_betas][(quantile+1):(2*quantile)],
              P2=names(order_betas)[order_betas][(2*quantile+1):(3*quantile)],
              P3=names(order_betas)[order_betas][(3*quantile+1):(N-quantile)],
              High_Beta=names(order_betas)[order_betas][(N-quantile+1):N]))
})

####Portfolio Constructions####
lookback_return_list <- vector(mode = "list", length = periods)
holding_return_list <- vector(mode = "list", length = periods)

for (x in 1:periods) { 
  lookback_return_list[[x]] = df_SR[(1+(x-1)* holding):(rollingW+(x-1)* holding),]
  holding_return_list[[x]] = df_SR[(1+rollingW+(x-1)* holding):(rollingW+ holding+(x-1)* holding), ]
}

lookback_size_list <- vector(mode = "list", length = periods)
holding_size_list <- vector(mode = "list", length = periods)

for (x in 1:periods) { 
  lookback_size_list[[x]] = df_size[(1+(x-1)* holding):(rollingW+(x-1)* holding),]
  holding_size_list[[x]] = df_size[(1+rollingW+(x-1)* holding):(rollingW+ holding+(x-1)* holding), ]
}

#### 1. Equal-Weighting lowbeta portfolio (Lowbeta Stocks)
#The equal-weighted low beta portfolio (Low beta) is formed by equal weighting 
#those stocks with the lowest 20% of betas. 
hld_period_calcs1 <- function(a, b){
  port = a[ ,b[[1]]]
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  starting_weights = rep(1/length(port), length(port)) # Equal-Weight
  level = rbind(starting_weights,port+1)
  level = rollapply(level, FUN = prod, width = 1:(holding +1), align = "right")
  value = rowSums(level,na.rm = T)
  returns = round(log(value[2:length(value)]/value[1:(length(value)-1)]),4) # log return
  return(returns)
}

df_portfolio <- data.frame()
df_portfolio <- data.frame(row.names = c(rownames(df_SR)[-c(1:rollingW)][1:(periods*holding)]),
                           MR_RF = round(df_MR$MR_RF[-c(1:rollingW)][1:(periods*holding)],4),
                           Lowbeta = unlist(Map(hld_period_calcs1, holding_return_list, HML))) # port_number 1 is low beta portfolio

#### 2. Market Capitalization-Weighting (MCW) (Lowbeta Stocks)
# Calculate low-beta market capitalization
mcap_wght <- vector(mode = "list", length = periods)

for (i in 1:periods){
  mcap_wght[[i]] <- as.data.frame(t(data.frame(mcap_wght=unlist(holding_size_list[[i]][unlist(HML[[i]][1])])
                                 /sum(unlist(holding_size_list[[i]][unlist(HML[[i]][1])]), na.rm = T))))
}

hld_period_calcs2 <- function(a, b, weights){
  port = a[ ,b[[1]]]
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  idx = intersect(names(port), names(weights))
  port <- port[idx]
  starting_weights <- weights[idx] #MVP_wght
  level = rbind(starting_weights,port+1)
  level = rollapply(level, FUN = prod, width = 1:(holding +1), align = "right")
  value = rowSums(level,na.rm = T)
  returns = round(log(value[2:length(value)]/value[1:(length(value)-1)]),4) # log return
  return(returns)
}

df_portfolio <- df_portfolio %>% mutate(MCW = unlist(Map(hld_period_calcs2, a = holding_return_list, b = HML, weights = mcap_wght)))

#### 3. The naive equally-weighted portfolio (EW) (All stocks)
hld_period_calcs3 <- function(x){
  port = x
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  starting_weights = rep(1/length(port), length(port)) # Equal-Weight
  level = rbind(starting_weights,port+1)
  level = rollapply(level, FUN = prod, width = 1:(holding +1), align = "right")
  value = rowSums(level,na.rm = T)
  returns = round(log(value[2:length(value)]/value[1:(length(value)-1)]),4) # log return
  return(returns)
}

df_portfolio <- df_portfolio %>% mutate(EW = unlist(Map(hld_period_calcs3, x = holding_return_list)))

#### 4. Minimum Variance Portfolio (MVP)
# Calculate the GMVP weights
# These portfolios are rebalanced monthly using a 36 months rolling window to estimate the covariance matrix. 

#### Unconstrained
MVP_wght <- vector(mode = "list", length = periods)

for (i in 1:periods){
  tmp <- lookback_return_list[[i]][unlist(HML[[i]][1])]
  tmp <- tmp %>%
    select_if(~ !any(is.na(.))) %>% select_if(~ !any(.==0))
  cov_tmp <- as.matrix(cov(tmp))
  inv_tmp <- as.matrix(inv(cov_tmp)) # use pinv will cause the sum of wght not equal to 1, because it won't return real inv
  wght <- data.frame((ones(1,dim(cov_tmp)[1]) %*% inv_tmp)/sum(inv_tmp))
  MVP_wght[[i]] <- data.frame(wght, row.names = "MVP_wght")
  colnames(MVP_wght[[i]]) <- colnames(cov_tmp)
}

df_portfolio <- df_portfolio %>% mutate(MVP = unlist(Map(hld_period_calcs2, a = holding_return_list, b = HML, weights = MVP_wght)))


#### With long-only constrain
i = 1
tmp <- lookback_return_list[[i]][unlist(HML[[i]][1])]
tmp <- tmp %>%
  select_if(~ !any(is.na(.))) %>% select_if(~ !any(.==0))




#### 5. Low Volatility Single Index Model(SIM)




#### Equal Risk Contribution (ERC)


#### Naive Risk Parity (NRP)


#### Maximum Diversification Portfolio (MDP)










