rm(list = ls())

####Load any required libraries####
require(readxl)
require(xts)
require(ggplot2)
require(dplyr)
require(tidyverse)

####Importing Data####
# Please use setwd() to set working directory to where we store data
df_Price <- read_excel("Price.xlsx")
df_Size <- read_excel("Size.xlsx")
df_PE <- read_excel("PE.xlsx")
df_MR <- read_excel("Indices.xlsx", col_types = c("date", "numeric", "skip", "skip", "skip", "skip", "skip"))
df_RF <- read_excel("91days T-Bill rate.xlsx")
df_EBIT <- read_excel("EBIT.xlsx")
df_Asset <- read_excel("Asset.xlsx")

####Sorting Data####
#df_RF means the dataframe of risk-free return
date1 <- df_RF$Date
df_RF <- as.numeric(df_RF$Value)
df_RF <- (1+df_RF/100)^(1/365)-1
df_RF <- as.data.frame(cbind(Dates=date1,Value=df_RF))
df_RF$Dates <- as.Date(df_RF$Dates)
rm(date1)

#df_return means the dataframe of stock returns
df_Price <- subset(df_Price, Dates >= "2000-01-01" & Dates <= "2020-01-01")
Date <- as.data.frame(df_Price[, 1])
df_Price <- as.data.frame(lapply(df_Price[,-1],as.numeric))
df_return <- log(df_Price[2:nrow(df_Price),]/df_Price[1:(nrow(df_Price)-1),]) # log return
# df_return <- (df_Price[2:nrow(df_Price),]-df_Price[1:(nrow(df_Price)-1),])/df_Price[1:(nrow(df_Price)-1),] # simple return
df_return <- cbind(Dates=Date$Dates[-1],df_return)
df_return <- df_return[,colSums(is.na(df_return))<nrow(df_return)]
df_Price <- cbind(Date, df_Price)

# cl_outliers1 <- function(c){
#   b <- boxplot(c, plot = FALSE)
#   a <- c
#   a[which(c %in% b$out)] <- NA
#   #Use the follwoing code to replace outliers with mean
#   #a[which(c %in% b$out)] <- mean(c[which(! c %in% b$out)],na.rm=TRUE
#   return(a)
# }

cl_outliers2 <- function(x){
  quantiles <- quantile( x, c(.01, .99 ), na.rm = T )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x[ x < -0.4 ] <- NA
  x[ x > 0.4 ] <- NA
  return(x)
}

df_return[,-1] <- as.data.frame(apply(df_return[,-1], FUN = cl_outliers2, MARGIN = 2))
#about 756 abnormal daily return data will be turned into NA

#df_MR means the dataframe of market returns
date2 <- as.data.frame(df_MR$Dates)
df_MR <- as.data.frame(lapply(df_MR[,-1],as.numeric))
df_MR <- as.data.frame(log(df_MR[2:nrow(df_MR),]/df_MR[1:(nrow(df_MR)-1),])) # log return
# df_MR <- as.data.frame(df_MR[2:nrow(df_MR),]-df_MR[1:(nrow(df_MR)-1),])/df_MR[1:(nrow(df_MR)-1),] # simple return
df_MR <- cbind(Dates=date2[-1,],df_MR)
colnames(df_MR) <- c("Dates","MR_RF")
df_MR <- subset(df_MR, Dates %in% df_return$Dates)
df_RF <- subset(df_RF, Dates %in% as.Date(df_return$Dates))
df_MR[,2] <- round(as.numeric(unlist(df_MR$MR_RF))-as.numeric(unlist(df_RF$Value)),4)
df_return <- cbind(df_MR, df_return[,-1])
rm(date2)

#df_PE means the dataframe of shares' P/E ratios
df_PE <- subset(df_PE, Dates %in% df_return$Dates)
Dates <- df_PE[,1]
df_PE <- as.data.frame(lapply(df_PE[,-1], as.numeric))
df_PE <- cbind(Dates, df_PE)
df_PE <- df_PE[ ,(names(df_PE) %in% names(df_return[,-2]))]

#Shape df_Size and make it have the same date as df.return, and then change the column format of 
#df_Size from logic to numeric.
df_Size <- subset(df_Size, Dates %in% df_return$Dates)
df_Size <- as.data.frame(lapply(df_Size[,-1],as.numeric))
df_Size <- cbind(Dates, df_Size)
df_Size <- df_Size[ ,(names(df_Size) %in% names(df_return[,-2]))]
df_Size[df_Size == 0] <- NA # about 131 daily size data will be changed into NA

#df_OP and make it have the same date as df.return, and then change the column format of 
#df_OP from logic to numeric.
Date <- df_EBIT$Dates
df_EBIT <- as.data.frame(lapply(df_EBIT[,-1],as.numeric))
df_Asset[df_Asset == 0] <- NA # about 260 daily Asset data will be changed into NA
df_Asset <- as.data.frame(lapply(df_Asset[,-1],as.numeric))

# Here, we use EBIT/Asset (ROA) rather than ROE as the indication of operating profitability
df_OP <- df_EBIT/df_Asset
df_OP <- cbind(Dates=Date, df_OP)
df_OP <- subset(df_OP, Dates %in% df_return$Dates)
df_OP <- df_OP[ ,(names(df_OP) %in% names(df_return[,-2]))]

#df_INV and make it have the same date as df.return, and then change the column format of 
#df_INV from logic to numeric.
df_INV <- as.data.frame(log(df_Asset[2:nrow(df_Asset),]/df_Asset[1:(nrow(df_Asset)-1),])) # log rate
# df_INV <- (df_Asset[2:nrow(df_Asset),]-df_Asset[1:(nrow(df_Asset)-1),])/df_Asset[1:(nrow(df_Asset)-1),] # simple rate
df_INV <- cbind(Dates=Date[-1],df_INV)
df_INV <- subset(df_INV, Dates %in% df_return$Dates)
df_INV <- df_INV[ ,(names(df_INV) %in% names(df_return[,-2]))]

repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}   

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

df_INV_tmp <- df_INV
df_INV_tmp[is.na(df_INV_tmp)] <- NaN
df_INV_tmp[df_INV_tmp==0] <- NA
df_INV_tmp[,-1] <- lapply(df_INV_tmp[,-1], repeat.before)
df_INV_tmp <- na.locf(df_INV_tmp, fromLast = T, na.rm = F, maxgap = 252)
df_INV_tmp[is.nan(df_INV_tmp)] <- NA
df_INV <- df_INV_tmp

####Preparing and align list####
Len <- nrow(df_return)
trading_days <- 250 #how many trading days in one year
Lookback <- trading_days
Gap <- 0
Holding <- trading_days

Periods <- trunc((Len-Lookback-Gap)/Holding)
Periods #This is the total return holding list

lookback_return_list <- vector(mode = "list", length = Periods)
gap_return_list <- vector(mode = "list", length = Periods)
holding_return_list <- vector(mode = "list", length = Periods)

for (x in 1:Periods) { 
  lookback_return_list[[x]] = df_return[(1+(x-1)*Holding):(Lookback+(x-1)*Holding),]
  gap_return_list[[x]] = df_return[(1+Lookback+(x-1)*Gap):(Lookback+Gap+(x-1)*Gap), ]
  holding_return_list[[x]] = df_return[(1+Lookback+Gap+(x-1)*Holding):(Lookback+Gap+Holding+(x-1)*Holding), ]
}

#Divide those comopanies based on their size
lookback_size_list <- vector(mode = "list", length = Periods)
gap_size_list <- vector(mode = "list", length = Periods)
holding_size_list <- vector(mode = "list", length = Periods)

for (x in 1:Periods) { 
  lookback_size_list[[x]] = df_Size[(1+((x-1)*Holding)):(Lookback+((x-1)*Holding)), ]
  gap_size_list[[x]] = df_Size[(1+Lookback+(x-1)*Gap):(Lookback+Gap+(x-1)*Gap), ]
  holding_size_list[[x]] = df_Size[(1+Lookback+Gap+(x-1)*Holding):(Lookback+Gap+Holding+(x-1)*Holding), ]
}

#Divide the companies based on their Value.
lookback_PE_list <- vector(mode = "list", length = Periods)
gap_PE_list <- vector(mode = "list", length = Periods)
holding_PE_list <- vector(mode = "list", length = Periods)

for (x in 1:Periods) { 
  lookback_PE_list[[x]] = df_PE[(1+((x-1)*Holding)):(Lookback+((x-1)*Holding)), ]
  gap_PE_list[[x]] = df_PE[(1+Lookback+(x-1)*Gap):(Lookback+Gap+(x-1)*Gap), ]
  holding_PE_list[[x]] = df_PE[(1+Lookback+Gap+(x-1)*Holding):(Lookback+Gap+Holding+(x-1)*Holding), ]
}

#Divide the companies based on their OP.
lookback_OP_list <- vector(mode = "list", length = Periods)
gap_OP_list <- vector(mode = "list", length = Periods)
holding_OP_list <- vector(mode = "list", length = Periods)

for (x in 1:Periods) { 
  lookback_OP_list[[x]] = df_OP[(1+((x-1)*Holding)):(Lookback+((x-1)*Holding)), ]
  gap_OP_list[[x]] = df_OP[(1+Lookback+(x-1)*Gap):(Lookback+Gap+(x-1)*Gap), ]
  holding_OP_list[[x]] = df_OP[(1+Lookback+Gap+(x-1)*Holding):(Lookback+Gap+Holding+(x-1)*Holding), ]
}

#Divide the companies based on their investment growth rate (INV).
lookback_INV_list <- vector(mode = "list", length = Periods)
gap_INV_list <- vector(mode = "list", length = Periods)
holding_INV_list <- vector(mode = "list", length = Periods)

for (x in 1:Periods) { 
  lookback_INV_list[[x]] = df_INV[(1+((x-1)*Holding)):(Lookback+((x-1)*Holding)), ]
  gap_INV_list[[x]] = df_INV[(1+Lookback+(x-1)*Gap):(Lookback+Gap+(x-1)*Gap), ]
  holding_INV_list[[x]] = df_INV[(1+Lookback+Gap+(x-1)*Holding):(Lookback+Gap+Holding+(x-1)*Holding), ]
}

####SMB Portfolios####
SMB <- vector(mode = "list", length = Periods)

SMB <- lapply(lookback_size_list, function(x){
  
  size_mean <- sapply(x[-1], function(y){
    ans <- mean(unlist(y), na.rm = T)
    names(ans) <- names(y)
    return(ans)
  })
  
  rank_size <- rank(size_mean, na.last = NA) 
  # na.last for controlling the treatment of NAs. If TRUE, missing values. 
  #in the data are put last; if FALSE, they are put first; if NA, they are removed;
  #if "keep" they are kept with rank NA.
  
  number_of_portfolios = 2
  N <- length(rank_size)
  quantile = trunc(N/number_of_portfolios)
  
  return(list(Big=names(rank_size)[rank_size][1:quantile],
              Small=names(rank_size)[rank_size][(quantile+1):N]))
})

####HML Portfolios####
HML <- vector(mode = "list", length = Periods)

HML <- lapply(lookback_PE_list, function(x){
  
  PE_mean <- sapply(x[-1], function(y){
    ans <- mean(unlist(y), na.rm = T)
    names(ans) <- names(y)
    return(ans)
  })
  
  rank_PE <- rank(PE_mean, na.last = NA) 
  
  number_of_portfolios = 3
  N <- length(rank_PE)
  quantile = trunc(N/number_of_portfolios)
  
  return(list(Growth=names(rank_PE)[rank_PE][1:quantile],
              Neutral=names(rank_PE)[rank_PE][(quantile+1):(N-quantile)],
              Value=names(rank_PE)[rank_PE][(N-quantile+1):N]))
})
#### High PE: growth; Low PE: value

####WML Portfolios####
#winners are the top 20%
#losers are the bottom 20%
WML <- vector(mode = "list", length = Periods)

Sum_S <- function(s){
  if (is.na(all(s))) {return(NA)}
  else if (is.na(all(s))==F) {return(sum(s, na.rm = T))}
}

WML <- lapply(lookback_return_list, function(x){
  
  Return_colsum <- sapply(x[-c(1:2)], function(y){
    ans <- Sum_S(unlist(y))
    names(ans) <- names(y)
    return(ans)
  })
  
  rank_Return <- rank(Return_colsum, na.last = NA) 
  
  number_of_portfolios = 3
  N <- length(rank_Return)
  quantile = trunc(N/number_of_portfolios)
  
  return(list(Winners=names(rank_Return)[rank_Return][1:quantile],
              Neutral=names(rank_Return)[rank_Return][(quantile+1):(N-quantile)],
              Losers=names(rank_Return)[rank_Return][(N-quantile+1):N]))
})

####RMW Portfolio####
#Stocks with robust and weak operating profitability#
#robust firms are the top 33% 
#weak firms are the bottom 33% 
RMW <- vector(mode = "list", length = Periods)

RMW <- lapply(lookback_OP_list, function(x){
  
  OP_mean <- sapply(x[-1], function(y){
    ans <- mean(unlist(y), na.rm = T)
    names(ans) <- names(y)
    return(ans)
  })
  
  rank_OP <- rank(OP_mean, na.last = NA) 
  
  number_of_portfolios = 3
  N <- length(rank_OP)
  quantile = trunc(N/number_of_portfolios)
  
  return(list(Robust=names(rank_OP)[rank_OP][1:quantile],
              Neutral=names(rank_OP)[rank_OP][(quantile+1):(N-quantile)],
              Weak=names(rank_OP)[rank_OP][(N-quantile+1):N]))
})

####CMA Portfolio####
#Stocks of high and low investment firms (conservative minus aggressive)
#Conservative means firms with lower investment growth rate
#Aggressive means firms with higher investment growth rate
CMA <- vector(mode = "list", length = Periods)

CMA <- lapply(lookback_INV_list, function(x){
  
  INV_colsum <- sapply(x[-1], function(y){
    ans <- mean(unlist(y))
    names(ans) <- names(y)
    return(ans)
  })
  
  rank_INV <- rank(INV_colsum, na.last = NA) 
  
  number_of_portfolios = 3
  N <- length(rank_INV)
  quantile = trunc(N/number_of_portfolios)
  
  return(list(Conservative=names(rank_INV)[rank_INV][1:quantile],
              Neutral=names(rank_INV)[rank_INV][(quantile+1):(N-quantile)],
              Aggressive=names(rank_INV)[rank_INV][(N-quantile+1):N]))
})

####One-Way Sorting Procedure####
####Holding Period####
port_1 <- holding_return_list[[15]][,WML[[15]]$Loser]
port_1 <- port_1 %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))

#1.Create starting share levels for each portfolio determined by share weights
starting_weights = rep(1,length(port_1))

#2.Grow share levels by daily returns over holding period
level_1 <- rbind(starting_weights, port_1 + 1)

#we can add na.rm=T to rollapply to see how this will change the data, and it's important!!!
#level_1 <- round(rollapply(level_1, FUN = prod, width = 1:(Holding+1), align = "right", na.rm=T), 2)
level_1 <- round(rollapply(level_1, FUN = prod, width = 1:(Holding+1), align = "right"),2)

#3.Sum portfolio share levels for each day
value_1 <- rowSums(level_1, na.rm = T)
length(value_1)

#4.Calculate returns from portfolio values
#The following codes used to calculate the return rate
# returns_1 = round(log(value_1[2:length(value_1)]/value_1[1:(length(value_1)-1)]),4) # log return
returns_1 = round(((value_1[2:length(value_1)] - value_1[1:(length(value_1)-1)])/value_1[1:(length(value_1)-1)]),4) # simple return
length(returns_1)

####Generalized Holding Period Calculations####
hld_period_calcs1 <- function(a, b, weights =1, port_number){
  port = a[ ,b[[port_number]]]
  #delete the companies which the return rate is full with NAs or 0s
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  #the above means a[[1]][,unlist(b[[1]][1])]
  starting_weights = rep(weights, length(port))
  level = rbind(starting_weights,port+1)
  level = rollapply(level, FUN = prod, width = 1:(Holding +1), align = "right")
  value = rowSums(level,na.rm = T)
  # returns = round(((value[2:length(value)] - value[1:(length(value)-1)])/value[1:(length(value)-1)]),4) # simple return
  returns = round(log(value[2:length(value)]/value[1:(length(value)-1)]),4) # log return
  return(returns)
}

test <- Map(hld_period_calcs1, a=holding_return_list, b = RMW, port_number = 2)

#In "Map(hld_period_calcs1, holding_list, portfolio_names, port_number=1)", which means that
#we need to map holding_list, portfolios_names and port_number =1 into the function hld_period_calcs1(),
#in where a is holding_list, b is portfolio_names and port_number = 1, weight = 1
Lookback <- trading_days
Periods <- trunc((Len-Lookback-Gap)/Holding)
Periods

portfolio_df <- data.frame(Dates = df_return$Dates[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                           MR_RF = df_return$MR_RF[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                           Big = unlist(Map(hld_period_calcs1, holding_return_list, SMB, 
                                            port_number = 1)),
                           Small = unlist(Map(hld_period_calcs1, holding_return_list, SMB,
                                              port_number = 2)),
                           Growth = unlist(Map(hld_period_calcs1, holding_return_list, HML,
                                               port_number = 1)),
                           Value = unlist(Map(hld_period_calcs1, holding_return_list, HML,
                                              port_number = 3)),
                           Winners = unlist(Map(hld_period_calcs1, holding_return_list, WML,
                                                port_number = 1)),
                           Losers = unlist(Map(hld_period_calcs1, holding_return_list, WML,
                                               port_number = 3)),
                           Robust = unlist(Map(hld_period_calcs1, holding_return_list, RMW,
                                               port_number = 1)),
                           Weak = unlist(Map(hld_period_calcs1, holding_return_list, RMW,
                                             port_number = 3)),
                           Conservative = unlist(Map(hld_period_calcs1, holding_return_list, CMA,
                                                     port_number = 1)),
                           Aggressive = unlist(Map(hld_period_calcs1, holding_return_list, CMA,
                                                   port_number = 3)))

count_nas <- sapply(portfolio_df, function(x){sum(is.na(x))})
count_nas

#port = holding_list[[port_number]][,portfolio_names[[port_number]][[port_number]]]
#Remember the diff bet [] and [[]], for which the [] bring us a list, while the [[]] bring us the element of the list

####Plotting our results and running our model####
##2019/09 has extreme values, thus we must clean those extreme values.
portfolio_clean <- portfolio_df %>% 
  filter_at(vars(names(.[,2:12])),all_vars(.>-0.2 & .<0.2 & !is.na(.)))

count_nas <- sapply(portfolio_clean, function(x){sum(is.na(x))})
count_nas

plot1 <- portfolio_clean <- portfolio_clean %>%
  mutate(WML = round(Winners-Losers,4),
         Market_Level = rollapply((1+MR_RF), FUN = prod, width=1:nrow(portfolio_clean),align="right"),
         SMB_Level = rollapply((1+(Small-Big)), FUN = prod, width=1:nrow(portfolio_clean),align="right"),
         HML_Level = rollapply((1+(Value-Growth)), FUN = prod, width=1:nrow(portfolio_clean),align="right"),
         WML_Level = rollapply((1+(Winners-Losers)), FUN = prod, width=1:nrow(portfolio_clean),align="right"),
         RMW_Level = rollapply((1+(Robust-Weak)), FUN = prod, width=1:nrow(portfolio_clean),align="right"),
         CMA_Level = rollapply((1+(Conservative-Aggressive)), FUN = prod, width=1:nrow(portfolio_clean),align="right"))

portfolio_clean %>%
  select(Dates, MKT = Market_Level, SMB = SMB_Level, HML = HML_Level, WML = WML_Level, RMW = RMW_Level, CMA = CMA_Level) %>%
  gather(key = "Strategy", value = "Value", -1) %>%
  ggplot(aes(x=Dates, y=Value,colour=Strategy))+
  geom_line(size=1)+
  theme_bw()+
  ggtitle("The evolution of R1 invested in 6 Different Portfolios")
  


####Two-way Sorting Procedure####
####FF-5 Factor####
####Holding Period####
port_2 <- holding_return_list[[1]][,intersect(SMB[[1]][[2]],HML[[1]][[1]])]
port_2 <- port_2 %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))

#1.Create starting share levels for each portfolio determined by share weights
starting_weights = rep(1,length(port_2))

#2.Grow share levels by daily returns over holding period
level_2 <- rbind(starting_weights, port_2 + 1)

#we can add na.rm=T to rollapply to see how this will change the data, and it's important!!!
#level_2 <- round(rollapply(level_2, FUN = prod, width = 1:(Holding+1), align = "right", na.rm=T), 2)
level_2 <- round(rollapply(level_2, FUN = prod, width = 1:(Holding+1), align = "right"),2)

#3.Sum portfolio share levels for each day
value_2 <- rowSums(level_2, na.rm = T)
length(value_2)

#4.Calculate returns from portfolio values
#The following codes used to calculate the return rate
returns_2 = round(log(value_2[2:length(value_2)]/value_2[1:(length(value_2)-1)]),4)
length(returns_2)

####Generalized Holding Period Calculations####
hld_period_calcs2 <- function(a, b, c, weights =1, port_number1, port_number2){
  temp <- intersect(c[[port_number2]],b[[port_number1]])
  port <- a[,temp]
  # a is holding_period_return, b for HML list, c for SMB list, port_number1 for HML, port_number2 for SMB
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  starting_weights = rep(weights, length(port))
  level = rbind(starting_weights,port+1)
  level = rollapply(level, FUN = prod, width = 1:(Holding +1), align = "right")
  value = rowSums(level,na.rm = T)
  # returns = round(((value[2:length(value)] - value[1:(length(value)-1)])/value[1:(length(value)-1)]),4) # simple return
  returns = round(log(value[2:length(value)]/value[1:(length(value)-1)]),4) # log return
  return(returns)
}


test1 <- data.frame(hld_period_calcs2(a= holding_return_list[[1]],b = HML[[1]], c = SMB[[1]],port_number1=1, port_number2=1))
test2 <- Map(hld_period_calcs2, a= holding_return_list,b = HML, c = SMB, port_number1=1, port_number2=1)

#In "Map(hld_period_calcs, holding_return_list, portfolio_names, port_number=1)", which means that
#we need to map holding_return_list, portfolios_names and port_number =1 into the function hld_period_calcs(),
#in where a is holding_return_list, b is portfolio_names and port_number = 1, weight = 1
Lookback <- trading_days
Periods <- trunc((Len-Lookback-Gap)/Holding)
Periods

portfolio_df2 <- data.frame(Dates = df_return$Dates[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                            MR_RF = df_return$MR_RF[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                            B_Value = unlist(Map(hld_period_calcs2, holding_return_list, HML, SMB, 
                                                 port_number1 = 3, port_number2 = 1)),
                            B_Neutral_PE = unlist(Map(hld_period_calcs2, holding_return_list, HML, SMB,
                                                      port_number1 = 2, port_number2 = 1)),
                            B_Growth = unlist(Map(hld_period_calcs2, holding_return_list, HML, SMB,
                                                  port_number1 = 1, port_number2 = 1)),
                            B_Robust = unlist(Map(hld_period_calcs2, holding_return_list, RMW, SMB,
                                                  port_number1 = 1, port_number2 = 1)),
                            B_Neutral_OP = unlist(Map(hld_period_calcs2, holding_return_list, RMW, SMB,
                                                      port_number1 = 2, port_number2 = 1)),
                            B_Weak = unlist(Map(hld_period_calcs2, holding_return_list, RMW, SMB,
                                                port_number1 = 3, port_number2 = 1)),
                            B_Conservative= unlist(Map(hld_period_calcs2, holding_return_list, CMA, SMB,
                                                       port_number1 = 1, port_number2 = 1)),
                            B_Neutral_INV = unlist(Map(hld_period_calcs2, holding_return_list, CMA, SMB,
                                                       port_number1 = 2, port_number2 = 1)),
                            B_Aggressive = unlist(Map(hld_period_calcs2, holding_return_list, CMA, SMB,
                                                      port_number1 = 3, port_number2 = 1)),
                            S_Value = unlist(Map(hld_period_calcs2, holding_return_list, HML, SMB,
                                                 port_number1 = 3, port_number2 = 2)),
                            S_Neutral_PE = unlist(Map(hld_period_calcs2, holding_return_list, HML, SMB,
                                                      port_number1 = 2, port_number2 = 2)),
                            S_Growth = unlist(Map(hld_period_calcs2, holding_return_list, HML, SMB,
                                                  port_number1 = 1, port_number2 = 2)),
                            S_Robust = unlist(Map(hld_period_calcs2, holding_return_list, RMW, SMB,
                                                  port_number1 = 1, port_number2 = 2)),
                            S_Neutral_OP = unlist(Map(hld_period_calcs2, holding_return_list, RMW, SMB,
                                                      port_number1 = 2, port_number2 = 2)),
                            S_Weak = unlist(Map(hld_period_calcs2, holding_return_list, RMW, SMB,
                                                port_number1 = 3, port_number2 = 2)),
                            S_Conservative= unlist(Map(hld_period_calcs2, holding_return_list, CMA, SMB,
                                                       port_number1 = 1, port_number2 = 2)),
                            S_Neutral_INV = unlist(Map(hld_period_calcs2, holding_return_list, CMA, SMB,
                                                       port_number1 = 2, port_number2 = 2)),
                            S_Aggressive = unlist(Map(hld_period_calcs2, holding_return_list, CMA, SMB,
                                                      port_number1 = 3, port_number2 = 2))
)

count_nas <- sapply(portfolio_df2, function(x){sum(is.na(x))})
count_nas

#port = holding_list[[port_number]][,portfolio_names[[port_number]][[port_number]]]
#Remember the diff bet [] and [[]], for which the [] bring us a list, while the [[]] bring us the element of the list

####Plotting our results and running our model####
#### We need to clean some extreme values.
portfolio_clean2 <- portfolio_df2 %>% 
  filter_at(vars(names(.[,2:20])),all_vars(.>-0.2 & .<0.2 & !is.na(.)))

count_nas <- sapply(portfolio_clean2, function(x){sum(is.na(x))})
count_nas

portfolio_clean2 <- portfolio_clean2 %>%
  mutate(Market_Level = rollapply((1+MR_RF),FUN = prod, width=1:nrow(portfolio_clean2),align="right"),
         SMB = round((1/3)*((1/3)*(S_Value+S_Neutral_PE+S_Growth)-
                       (1/3)*(B_Value+B_Neutral_PE+B_Growth)+
                       (1/3)*(S_Robust+S_Neutral_OP+S_Growth)-
                       (1/3)*(B_Robust+B_Neutral_OP+B_Growth)+
                       (1/3)*(S_Conservative+S_Neutral_INV+S_Aggressive)-
                       (1/3)*(B_Conservative+B_Neutral_INV+B_Aggressive)),4),
         HML = round((1/2)*(S_Value+B_Value)-(1/2)*(S_Growth+B_Growth),4),
         RMW = round((1/2)*(S_Robust+B_Robust)-(1/2)*(S_Weak+B_Weak),4),
         CMA = round((1/2)*(S_Conservative+B_Conservative)-(1/2)*(S_Aggressive+B_Aggressive),4),
         SMB_Level = rollapply((1+SMB),
                               FUN = prod, width=1:nrow(portfolio_clean2),align="right"),
         HML_Level = rollapply((1+HML),
                               FUN = prod, width=1:nrow(portfolio_clean2),align="right"),
         RMW_Level = rollapply((1+RMW),
                               FUN = prod, width=1:nrow(portfolio_clean2),align="right"),
         CMA_Level = rollapply((1+CMA),
                               FUN = prod, width=1:nrow(portfolio_clean2),align="right"))


plot2 <- portfolio_clean2 %>%
  select(Dates, MKT = Market_Level, SMB = SMB_Level, HML = HML_Level, RMW = RMW_Level, CMA = CMA_Level) %>%
  gather(key = "Strategy", value = "Value", -1) %>%
  ggplot(aes(x=Dates, y=Value,colour=Strategy))+
  geom_line(size=1)+
  theme_bw()+
  ggtitle("The evolution of R1 invested in five different portfolios")+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(color = "grey",
                                          size = 0.7,
                                          linetype = 2))+
  geom_text(
    data = . %>% filter(Dates == max(Dates)),
    aes(label = round(Value, 2)),
    vjust = "outward", hjust = "outward",fontface ="plain", color = "black", size = 4, check_overlap = T,
    show.legend = FALSE) 

####FF-3 Factor####
hld_period_calcs3 <- function(a, b, c, weights =1, port_number1, port_number2){
  temp <- intersect(c[[port_number2]],b[[port_number1]])
  port <- a[,temp]
  # a is holding_period_return, b for HML list, c for SMB list, port_number1 for HML, port_number2 for SMB
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  starting_weights = rep(weights, length(port))
  level = rbind(starting_weights,port+1)
  level = rollapply(level, FUN = prod, width = 1:(Holding +1), align = "right")
  value = rowSums(level,na.rm = T)
  returns = round((value[-1]-value[-length(value)])/value[-length(value)],4) # log return
  # returns = round(((value[2:length(value)] - value[1:(length(value)-1)])/value[1:(length(value)-1)]),4) # simple return
  return(returns)
}

Lookback <- trading_days
Periods <- trunc((Len-Lookback-Gap)/Holding)
Periods

portfolio_df3 <- data.frame(Dates = df_return$Dates[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                            MR_RF = df_return$MR_RF[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                            B_Value = unlist(Map(hld_period_calcs3, holding_return_list, HML, SMB, 
                                                 port_number1 = 3, port_number2 = 1)),
                            B_Neutral_PE = unlist(Map(hld_period_calcs3, holding_return_list, HML, SMB,
                                                      port_number1 = 2, port_number2 = 1)),
                            B_Growth = unlist(Map(hld_period_calcs3, holding_return_list, HML, SMB,
                                                  port_number1 = 1, port_number2 = 1)),
                            S_Value = unlist(Map(hld_period_calcs3, holding_return_list, HML, SMB,
                                                 port_number1 = 3, port_number2 = 2)),
                            S_Neutral_PE = unlist(Map(hld_period_calcs3, holding_return_list, HML, SMB,
                                                      port_number1 = 2, port_number2 = 2)),
                            S_Growth = unlist(Map(hld_period_calcs3, holding_return_list, HML, SMB,
                                                  port_number1 = 1, port_number2 = 2))
)

count_nas <- sapply(portfolio_df3, function(x){sum(is.na(x))})
count_nas

####Plotting our results and running our model####
#### We need to clean some extreme values.
portfolio_clean3 <- portfolio_df3 %>% 
  filter_at(vars(names(.[,2:8])),all_vars(.>-0.2 & .<0.2 & !is.na(.)))

count_nas <- sapply(portfolio_clean3, function(x){sum(is.na(x))})
count_nas

portfolio_clean3 <- portfolio_clean3 %>%
  mutate(Market_Level = rollapply((1+MR_RF),FUN = prod, width=1:nrow(portfolio_clean3),align="right"),
         SMB = (1/3)*(S_Value+S_Neutral_PE+S_Growth)-(1/3)*(B_Value+B_Neutral_PE+B_Growth),
         HML = (1/2)*(S_Value+B_Value)-(1/2)*(S_Growth+B_Growth),
         SMB_Level = rollapply((1+SMB),
                               FUN = prod, width=1:nrow(portfolio_clean3),align="right"),
         HML_Level = rollapply((1+HML),
                               FUN = prod, width=1:nrow(portfolio_clean3),align="right"))

plot3 <- portfolio_clean3 %>%
  select(Dates, MKT = Market_Level, SMB = SMB_Level, HML = HML_Level) %>%
  gather(key = "Strategy", value = "Value", -1) %>%
  ggplot(aes(x=Dates, y=Value,colour=Strategy))+
  geom_line(size=1)+
  theme_bw()+
  ggtitle("The evolution of R1 invested in three different strategies")+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(color = "grey",
                                          size = 0.7,
                                          linetype = 2))+
  geom_text(
    data = . %>% filter(Dates == max(Dates)),
    aes(label = round(Value, 2)),
    vjust = "outward", hjust = "outward",fontface ="plain", color = "black", size = 4, check_overlap = T,
    show.legend = FALSE)

####Regression Section####
df_SR <- df_return

#### CAPM (Simulation (compare the bias of FM, IV)) 


#### CAPM (OLS & IV) sorted
head(df_MR)

#### FF3-factor (OLS & IV) sorted
df_FF3 <- portfolio_clean3 %>% select(Dates, MR_RF, SMB, HML)

#### Carhart4-factor (OLS & IV)
tmp1 <- portfolio_clean3 %>% select(Dates, MR_RF, SMB, HML)
tmp2 <- portfolio_clean %>% select(Dates,WML)

df_FF4 <- merge(tmp1,tmp2,by.y = 'Dates')

#### FF5-factor (OLS & IV) sorted
df_FF5 <- portfolio_clean2 %>% select(Dates, MR_RF, SMB, HML, RMW, CMA)

#### FF3-factor(adding CS factors as control variables in IV) sorted
Dates <- df_Size$Dates
df_ln_Size <- log(df_Size[,-1])
df_ln_Size <-  apply(t(df_ln_Size), MARGIN = 2, scale)
df_ln_Size <- cbind(Dates,data.frame(t(df_ln_Size)))
colnames(df_ln_Size) <- colnames(df_Size)

# test wither the cross-section mean is 0 now
# apply(df_ln_Size[,-1], 1, mean, na.rm =T)
#Standardization: re-scale cross-sectional firm characteristics (PE,OP,INV)
#(x - mean(x)) / sd(x)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

df_std_PE <-  apply(t(df_PE[, -1]), 2, scale)
df_std_PE <- cbind(Dates,data.frame(t(df_std_PE)))
colnames(df_std_PE)<- colnames(df_PE)

df_std_OP <-  apply(t(df_OP[, -1]), 2, scale)
df_std_OP <- cbind(Dates,data.frame(t(df_std_OP)))
colnames(df_std_OP)<- colnames(df_OP)

df_std_INV <-  apply(t(df_INV[, -1]), 2, scale)
df_std_INV <- cbind(Dates,data.frame(t(df_std_INV)))
colnames(df_std_INV)<- colnames(df_INV)

#### Compare CS and TS model####
df_18_PR = portfolio_df2[,-2]

# Firm characteristics of these 18 portfolios are calculated by the equal-weighted mean of firm characteristics
# for individual stocks in each portfolio. Then these firm characteristics are standardized by z-score (0,1)

####df_18_Size####
hld_size_calcs <- function(a, b, c, port_number1, port_number2){
  temp <- intersect(c[[port_number2]],b[[port_number1]])
  port <- a[,temp]
  # a is holding_size_list, b for HML list, c for SMB list, port_number1 for HML, port_number2 for SMB
  # Only select the row that do not have na or full of 0s
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  row_mean = rollapply(port, FUN = mean, width = 1:(Holding +1), align = "right",by.column = F)
  return(row_mean)
}

####df_18_Size####
hld_size_calcs <- function(a, b, c, port_number1, port_number2){
  temp <- intersect(c[[port_number2]],b[[port_number1]])
  port <- a[,temp]
  # a is holding_size_list, b for HML list, c for SMB list, port_number1 for HML, port_number2 for SMB
  # Only select the row that do not have na or full of 0s
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  row_mean = rollapply(port, FUN = mean, width = 1:(Holding +1), align = "right",by.column = F)
  return(row_mean)
}

test5 <- data.frame(hld_size_calcs(a= holding_size_list[[1]],b = HML[[1]], c = SMB[[1]],port_number1=1, port_number2=1))
test6 <- Map(hld_size_calcs, a= holding_size_list,b = HML, c = SMB, port_number1=1, port_number2=1)
#In "Map(hld_size_calcs, holding_size_list, portfolio_names, port_number=1)", which means that
#we need to map holding_size_list, portfolios_names, port_number1 and port_number2 into the function hld_size_calcs(),
#in where a is holding_size_list, b/c are portfolio_names and weight = 1
Lookback <- trading_days
Periods <- trunc((Len-Lookback-Gap)/Holding)
Periods

df_18_size <- data.frame(Dates = df_return$Dates[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                         B_Value = unlist(Map(hld_size_calcs, holding_size_list, HML, SMB, 
                                              port_number1 = 3, port_number2 = 1)),
                         B_Neutral_PE = unlist(Map(hld_size_calcs, holding_size_list, HML, SMB,
                                                   port_number1 = 2, port_number2 = 1)),
                         B_Growth = unlist(Map(hld_size_calcs, holding_size_list, HML, SMB,
                                               port_number1 = 1, port_number2 = 1)),
                         B_Robust = unlist(Map(hld_size_calcs, holding_size_list, RMW, SMB,
                                               port_number1 = 1, port_number2 = 1)),
                         B_Neutral_OP = unlist(Map(hld_size_calcs, holding_size_list, RMW, SMB,
                                                   port_number1 = 2, port_number2 = 1)),
                         B_Weak = unlist(Map(hld_size_calcs, holding_size_list, RMW, SMB,
                                             port_number1 = 3, port_number2 = 1)),
                         B_Conservative= unlist(Map(hld_size_calcs, holding_size_list, CMA, SMB,
                                                    port_number1 = 1, port_number2 = 1)),
                         B_Neutral_INV = unlist(Map(hld_size_calcs, holding_size_list, CMA, SMB,
                                                    port_number1 = 2, port_number2 = 1)),
                         B_Aggressive = unlist(Map(hld_size_calcs, holding_size_list, CMA, SMB,
                                                   port_number1 = 3, port_number2 = 1)),
                         S_Value = unlist(Map(hld_size_calcs, holding_size_list, HML, SMB,
                                              port_number1 = 3, port_number2 = 2)),
                         S_Neutral_PE = unlist(Map(hld_size_calcs, holding_size_list, HML, SMB,
                                                   port_number1 = 2, port_number2 = 2)),
                         S_Growth = unlist(Map(hld_size_calcs, holding_size_list, HML, SMB,
                                               port_number1 = 1, port_number2 = 2)),
                         S_Robust = unlist(Map(hld_size_calcs, holding_size_list, RMW, SMB,
                                               port_number1 = 1, port_number2 = 2)),
                         S_Neutral_OP = unlist(Map(hld_size_calcs, holding_size_list, RMW, SMB,
                                                   port_number1 = 2, port_number2 = 2)),
                         S_Weak = unlist(Map(hld_size_calcs, holding_size_list, RMW, SMB,
                                             port_number1 = 3, port_number2 = 2)),
                         S_Conservative= unlist(Map(hld_size_calcs, holding_size_list, CMA, SMB,
                                                    port_number1 = 1, port_number2 = 2)),
                         S_Neutral_INV = unlist(Map(hld_size_calcs, holding_size_list, CMA, SMB,
                                                    port_number1 = 2, port_number2 = 2)),
                         S_Aggressive = unlist(Map(hld_size_calcs, holding_size_list, CMA, SMB,
                                                   port_number1 = 3, port_number2 = 2))
)

count_nas <- sapply(df_18_size, function(x){sum(is.na(x))})
count_nas

Dates <- df_18_PR$Dates
### cross-sectional z-sores for the size of 18 standard portfolios
df_18_std_size <-  apply(t(df_18_size[, -1]), MARGIN = 2, scale)
df_18_std_size <- cbind(Dates,data.frame(t(df_18_std_size)))
colnames(df_18_std_size) <- colnames(df_18_PR)

# apply(df_18_std_size[,-1], 1, mean, na.rm =T)
####df_18_PE####
hld_PE_calcs <- function(a, b, c, port_number1, port_number2){
  temp <- intersect(c[[port_number2]],b[[port_number1]])
  port <- a[,temp]
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  row_mean = rollapply(port, FUN = mean, width = 1:(Holding +1), align = "right",by.column = F)
  return(row_mean)
}

df_18_PE <- data.frame(Dates = df_return$Dates[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                       B_Value = unlist(Map(hld_PE_calcs, holding_PE_list, HML, SMB, 
                                            port_number1 = 3, port_number2 = 1)),
                       B_Neutral_PE = unlist(Map(hld_PE_calcs, holding_PE_list, HML, SMB,
                                                 port_number1 = 2, port_number2 = 1)),
                       B_Growth = unlist(Map(hld_PE_calcs, holding_PE_list, HML, SMB,
                                             port_number1 = 1, port_number2 = 1)),
                       B_Robust = unlist(Map(hld_PE_calcs, holding_PE_list, RMW, SMB,
                                             port_number1 = 1, port_number2 = 1)),
                       B_Neutral_OP = unlist(Map(hld_PE_calcs, holding_PE_list, RMW, SMB,
                                                 port_number1 = 2, port_number2 = 1)),
                       B_Weak = unlist(Map(hld_PE_calcs, holding_PE_list, RMW, SMB,
                                           port_number1 = 3, port_number2 = 1)),
                       B_Conservative= unlist(Map(hld_PE_calcs, holding_PE_list, CMA, SMB,
                                                  port_number1 = 1, port_number2 = 1)),
                       B_Neutral_INV = unlist(Map(hld_PE_calcs, holding_PE_list, CMA, SMB,
                                                  port_number1 = 2, port_number2 = 1)),
                       B_Aggressive = unlist(Map(hld_PE_calcs, holding_PE_list, CMA, SMB,
                                                 port_number1 = 3, port_number2 = 1)),
                       S_Value = unlist(Map(hld_PE_calcs, holding_PE_list, HML, SMB,
                                            port_number1 = 3, port_number2 = 2)),
                       S_Neutral_PE = unlist(Map(hld_PE_calcs, holding_PE_list, HML, SMB,
                                                 port_number1 = 2, port_number2 = 2)),
                       S_Growth = unlist(Map(hld_PE_calcs, holding_PE_list, HML, SMB,
                                             port_number1 = 1, port_number2 = 2)),
                       S_Robust = unlist(Map(hld_PE_calcs, holding_PE_list, RMW, SMB,
                                             port_number1 = 1, port_number2 = 2)),
                       S_Neutral_OP = unlist(Map(hld_PE_calcs, holding_PE_list, RMW, SMB,
                                                 port_number1 = 2, port_number2 = 2)),
                       S_Weak = unlist(Map(hld_PE_calcs, holding_PE_list, RMW, SMB,
                                           port_number1 = 3, port_number2 = 2)),
                       S_Conservative= unlist(Map(hld_PE_calcs, holding_PE_list, CMA, SMB,
                                                  port_number1 = 1, port_number2 = 2)),
                       S_Neutral_INV = unlist(Map(hld_PE_calcs, holding_PE_list, CMA, SMB,
                                                  port_number1 = 2, port_number2 = 2)),
                       S_Aggressive = unlist(Map(hld_PE_calcs, holding_PE_list, CMA, SMB,
                                                 port_number1 = 3, port_number2 = 2))
)

count_nas <- sapply(df_18_PE, function(x){sum(is.na(x))})
count_nas

df_18_std_PE <-  apply(t(df_18_PE[, -1]), MARGIN = 2, scale)
df_18_std_PE <- cbind(Dates,data.frame(t(df_18_std_PE)))
colnames(df_18_std_PE) <- colnames(df_18_PR)

# is.nan.data.frame <- function(x)
#   do.call(cbind, lapply(x, is.nan))
# df_18_std_size[is.nan(df_18_std_size)] <- NA

####df_18_OP####
hld_OP_calcs <- function(a, b, c, port_number1, port_number2){
  temp <- intersect(c[[port_number2]],b[[port_number1]])
  port <- a[,temp]
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  row_mean = rollapply(port, FUN = mean, width = 1:(Holding +1), align = "right",by.column = F)
  return(row_mean)
}

df_18_OP <- data.frame(Dates = df_return$Dates[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                       B_Value = unlist(Map(hld_OP_calcs, holding_OP_list, HML, SMB, 
                                            port_number1 = 3, port_number2 = 1)),
                       B_Neutral_OP = unlist(Map(hld_OP_calcs, holding_OP_list, HML, SMB,
                                                 port_number1 = 2, port_number2 = 1)),
                       B_Growth = unlist(Map(hld_OP_calcs, holding_OP_list, HML, SMB,
                                             port_number1 = 1, port_number2 = 1)),
                       B_Robust = unlist(Map(hld_OP_calcs, holding_OP_list, RMW, SMB,
                                             port_number1 = 1, port_number2 = 1)),
                       B_Neutral_OP = unlist(Map(hld_OP_calcs, holding_OP_list, RMW, SMB,
                                                 port_number1 = 2, port_number2 = 1)),
                       B_Weak = unlist(Map(hld_OP_calcs, holding_OP_list, RMW, SMB,
                                           port_number1 = 3, port_number2 = 1)),
                       B_Conservative= unlist(Map(hld_OP_calcs, holding_OP_list, CMA, SMB,
                                                  port_number1 = 1, port_number2 = 1)),
                       B_Neutral_INV = unlist(Map(hld_OP_calcs, holding_OP_list, CMA, SMB,
                                                  port_number1 = 2, port_number2 = 1)),
                       B_Aggressive = unlist(Map(hld_OP_calcs, holding_OP_list, CMA, SMB,
                                                 port_number1 = 3, port_number2 = 1)),
                       S_Value = unlist(Map(hld_OP_calcs, holding_OP_list, HML, SMB,
                                            port_number1 = 3, port_number2 = 2)),
                       S_Neutral_OP = unlist(Map(hld_OP_calcs, holding_OP_list, HML, SMB,
                                                 port_number1 = 2, port_number2 = 2)),
                       S_Growth = unlist(Map(hld_OP_calcs, holding_OP_list, HML, SMB,
                                             port_number1 = 1, port_number2 = 2)),
                       S_Robust = unlist(Map(hld_OP_calcs, holding_OP_list, RMW, SMB,
                                             port_number1 = 1, port_number2 = 2)),
                       S_Neutral_OP = unlist(Map(hld_OP_calcs, holding_OP_list, RMW, SMB,
                                                 port_number1 = 2, port_number2 = 2)),
                       S_Weak = unlist(Map(hld_OP_calcs, holding_OP_list, RMW, SMB,
                                           port_number1 = 3, port_number2 = 2)),
                       S_Conservative= unlist(Map(hld_OP_calcs, holding_OP_list, CMA, SMB,
                                                  port_number1 = 1, port_number2 = 2)),
                       S_Neutral_INV = unlist(Map(hld_OP_calcs, holding_OP_list, CMA, SMB,
                                                  port_number1 = 2, port_number2 = 2)),
                       S_Aggressive = unlist(Map(hld_OP_calcs, holding_OP_list, CMA, SMB,
                                                 port_number1 = 3, port_number2 = 2))
)

count_nas <- sapply(df_18_OP, function(x){sum(is.na(x))})
count_nas

df_18_std_OP <-  apply(t(df_18_OP[, -1]), MARGIN = 2, scale)
df_18_std_OP <- cbind(Dates,data.frame(t(df_18_std_OP)))
colnames(df_18_std_OP) <- colnames(df_18_PR)

# is.nan.data.frame <- function(x)
#   do.call(cbind, lapply(x, is.nan))
# df_18_std_size[is.nan(df_18_std_size)] <- NA

####df_18_INV####
hld_INV_calcs <- function(a, b, c, port_number1, port_number2){
  temp <- intersect(c[[port_number2]],b[[port_number1]])
  port <- a[,temp]
  port <- port %>% select_if(~sum(is.na(.)) == 0) %>% select_if(~!all(.==0))
  row_mean = rollapply(port, FUN = mean, width = 1:(Holding +1), align = "right",by.column = F)
  return(row_mean)
}

df_18_INV <- data.frame(Dates = df_return$Dates[-c(1:(Lookback+Gap))][1:(Periods*Holding)],
                        B_Value = unlist(Map(hld_INV_calcs, holding_INV_list, HML, SMB, 
                                             port_number1 = 3, port_number2 = 1)),
                        B_Neutral_INV = unlist(Map(hld_INV_calcs, holding_INV_list, HML, SMB,
                                                   port_number1 = 2, port_number2 = 1)),
                        B_Growth = unlist(Map(hld_INV_calcs, holding_INV_list, HML, SMB,
                                              port_number1 = 1, port_number2 = 1)),
                        B_Robust = unlist(Map(hld_INV_calcs, holding_INV_list, RMW, SMB,
                                              port_number1 = 1, port_number2 = 1)),
                        B_Neutral_INV = unlist(Map(hld_INV_calcs, holding_INV_list, RMW, SMB,
                                                   port_number1 = 2, port_number2 = 1)),
                        B_Weak = unlist(Map(hld_INV_calcs, holding_INV_list, RMW, SMB,
                                            port_number1 = 3, port_number2 = 1)),
                        B_Conservative= unlist(Map(hld_INV_calcs, holding_INV_list, CMA, SMB,
                                                   port_number1 = 1, port_number2 = 1)),
                        B_Neutral_INV = unlist(Map(hld_INV_calcs, holding_INV_list, CMA, SMB,
                                                   port_number1 = 2, port_number2 = 1)),
                        B_Aggressive = unlist(Map(hld_INV_calcs, holding_INV_list, CMA, SMB,
                                                  port_number1 = 3, port_number2 = 1)),
                        S_Value = unlist(Map(hld_INV_calcs, holding_INV_list, HML, SMB,
                                             port_number1 = 3, port_number2 = 2)),
                        S_Neutral_INV = unlist(Map(hld_INV_calcs, holding_INV_list, HML, SMB,
                                                   port_number1 = 2, port_number2 = 2)),
                        S_Growth = unlist(Map(hld_INV_calcs, holding_INV_list, HML, SMB,
                                              port_number1 = 1, port_number2 = 2)),
                        S_Robust = unlist(Map(hld_INV_calcs, holding_INV_list, RMW, SMB,
                                              port_number1 = 1, port_number2 = 2)),
                        S_Neutral_INV = unlist(Map(hld_INV_calcs, holding_INV_list, RMW, SMB,
                                                   port_number1 = 2, port_number2 = 2)),
                        S_Weak = unlist(Map(hld_INV_calcs, holding_INV_list, RMW, SMB,
                                            port_number1 = 3, port_number2 = 2)),
                        S_Conservative= unlist(Map(hld_INV_calcs, holding_INV_list, CMA, SMB,
                                                   port_number1 = 1, port_number2 = 2)),
                        S_Neutral_INV = unlist(Map(hld_INV_calcs, holding_INV_list, CMA, SMB,
                                                   port_number1 = 2, port_number2 = 2)),
                        S_Aggressive = unlist(Map(hld_INV_calcs, holding_INV_list, CMA, SMB,
                                                  port_number1 = 3, port_number2 = 2))
)

count_nas <- sapply(df_18_INV, function(x){sum(is.na(x))})
count_nas

df_18_std_INV <-  apply(t(df_18_INV[, -1]), MARGIN = 2, scale)
df_18_std_INV <- cbind(Dates,data.frame(t(df_18_std_INV)))
colnames(df_18_std_INV) <- colnames(df_18_PR)

# #Newey-West Adjustment for a time-series data 
# ret <- c(0.37264297, -0.15778735, -0.13038129,  1.08330381, -0.11640718,
#          -0.05459263, -0.28703501,  1.50972202, -0.5622087 , -0.30937038)
# ret
# 
# NWtest <- function (ret,lag,h0) {
#   T=length(ret)
#   vv=var(ret)
#   for (l in 1:lag) {
#     cc=cov(ret[1:(length(ret)-l)],ret[(l+1):length(ret)])
#     vv=vv+2*(1-l/lag)*cc}
#   y=(mean(ret)-h0)/sqrt(vv)*sqrt(T)
# }
# 
# t.test(ret)[1]
# t_NW  = NWtest(ret,1,0)
# t_NW

####LHS Test Asset (3*25 portfolios)####








