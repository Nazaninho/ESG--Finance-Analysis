
  #' title: Sustainable Finance
  #' subtitle: ESG 2
  #' author: 'Nazanin Hosseini'
  #' date: ''
  #' output: pdf_document
  #' ---
  
  

## Preparatory steps
# Housekeeping
rm(list = ls())

# Packages 
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("statsmodels")
#install.packages("dplyr")
#install.packages("car")
#install.packages("lmtest")
#install.packages("sandwich")
library(rmarkdown)  # to generate pdf file
library(tinytex)    # to generate pdf file
library(readxl)     # to read Excel files
library(tidyverse)
library(lubridate)
library(stats)
library(dplyr)
library(stats)
library(stats4)
library(MASS)
library(car)        # to perform joint hypothesis tests
library(dplyr)
library(lubridate)
library(lmtest)     # to estimate regression using White's standard errors
library(sandwich)
library(zoo)
library(dplyr)
library(tidyr)
library(moments)
library(openxlsx)

# Set working directory
setwd("/Users/naz/Downloads") 



##################################################################################
## Import Data - All data stored in the folder "Data part 2" in the work directory
##################################################################################
#Returns - Different numbers of years from the other data sets!
StockReturn_df <- read.csv('Data part 2/StockReturns.csv')
FF5_df <- read.csv('Data part 2/FamaFrench5Factors.csv', skip = 733)
FactorNames <- c('MKT','SMB','HML','RMW','CMA')
colnames(FF5_df)<-c('Year','MKT', colnames(FF5_df)[3:7])

#All other data sets have the same number of years!!
#ESG information
ESG_df <- read.csv('Data part 2/ESG.csv')
ESG_Combined_df <- read.csv('Data part 2/ESG_Combined.csv')
Environment_df <- read.csv('Data part 2/Environment.csv')
Social_df <- read.csv('Data part 2/Social.csv')
Governance_df <- read.csv('Data part 2/Governance.csv')
CO2_df <- read.csv('Data part 2/CO2.csv')
CO2toRevenue_df <- read.csv('Data part 2/CO2toRevenue.csv')

#Profitability
ROE_df <- read.csv('Data part 2/ROE.csv')
ROIC_df <- read.csv('Data part 2/ROIC.csv')
ROA_df <- read.csv('Data part 2/ROA.csv')
OperatingMargin_df <- read.csv('Data part 2/OperatingMargin.csv')

#Pricing ratios
MarketBook_df <- read.csv('Data part 2/MarketBook.csv')
DividendYield_df <- read.csv('Data part 2/DividendYield.csv')

#######################################################################################
## Data pre-processing - Get all data in matrixes from 2002-2023 and remove date column
#######################################################################################
#Return data
StockReturn <- as.matrix(StockReturn_df[StockReturn_df["Year"]>= 2002 & StockReturn_df["Year"]<=2023,-1 ])
FF5 <- as.matrix(FF5_df[FF5_df["Year"]>= 2002 & FF5_df["Year"]<=2023,2:6])
RF <- as.matrix(FF5_df[FF5_df["Year"]>= 2002 & FF5_df["Year"]<=2023,"RF"]) #Pick out the risk free rate in the years we are interested in
Re <- sweep(StockReturn, 1, RF, FUN = "-")#Excess stock return matrix

#Profitability data
ROE <- as.matrix(ROE_df[ROE_df["Year"]>= 2002 & ROE_df["Year"]<=2023,-1])
ROIC <- as.matrix(ROIC_df[ROIC_df["Year"]>= 2002 & ROIC_df["Year"]<=2023,-1])
ROA <- as.matrix(ROA_df[ROA_df["Year"]>= 2002 & ROA_df["Year"]<=2023,-1])
OperatingMargin <- as.matrix(OperatingMargin_df[OperatingMargin_df["Year"]>= 2002 & OperatingMargin_df["Year"]<=2023,-1])

#ESG data
ESG <- as.matrix(ESG_df[ESG_df["Year"]>= 2002 & ESG_df["Year"]<=2023,-1])
ESG_Combined <- as.matrix(ESG_Combined_df[ESG_Combined_df["Year"]>= 2002 & ESG_Combined_df["Year"]<=2023,-1])
Environment <- as.matrix(Environment_df[Environment_df["Year"]>= 2002 & Environment_df["Year"]<=2023,-1])
Social <- as.matrix(Social_df[Social_df["Year"]>= 2002 & Social_df["Year"]<=2023,-1])
Governance <- as.matrix(Governance_df[Governance_df["Year"]>= 2002 & Governance_df["Year"]<=2023,-1])
CO2 <- log(1+as.matrix(CO2_df[CO2_df["Year"]>= 2002 & CO2_df["Year"]<=2023,-1]))
CO2toRevenue <- as.matrix(CO2toRevenue_df[CO2toRevenue_df["Year"]>= 2002 & CO2toRevenue_df["Year"]<=2023,-1])
DCO2 <- CO2[-1,]-CO2[-22,]

#Pricing ratios
MarketBook <- as.matrix(MarketBook_df[MarketBook_df["Year"]>= 2002 & MarketBook_df["Year"]<=2023,-1])
DividendYield <- as.matrix(DividendYield_df[DividendYield_df["Year"]>= 2002 & DividendYield_df["Year"]<=2023,-1])

#Number of years of data
T_Y <- length(RF)

#Remove outliers
MaxAbsRet <- 500
ROE[abs(ROE)>=MaxAbsRet] <- NA
ROIC[abs(ROIC)>=MaxAbsRet] <- NA
ROA[abs(ROA)>=MaxAbsRet] <- NA
OperatingMargin[abs(OperatingMargin)>=MaxAbsRet] <- NA
MarketBook[MarketBook<= 0] <- NA

################################################################################
## Analysis - Mean Ratings
################################################################################
#Calculate mean ESG ratings for each year
Mean_ESG <- rowMeans(ESG, na.rm = TRUE)
Mean_CO2 <- rowMeans(CO2, na.rm = TRUE)
Mean_CO2toRevenue <- rowMeans(CO2toRevenue, na.rm = TRUE)

plot(Mean_ESG, type = 'l', lwd = 3, ylim = c(0,120), xaxt = "n",
     main = "", xlab="", ylab="Mean ESG Rating and CO2 to Rev")
lines(Mean_CO2toRevenue/10, col = "red", lwd = 3)
axis(1, at=1:22, labels=2002:2023)
legend(17,120, c("Mean ESG", "CO2/Rev"), lty = 1, col = c("black", "red"))

################################################################################
## Univariate sorts
################################################################################
Sorting_Variables <- array(0, dim = c(22,503,7))
Sorting_Variables[,,1] <- ESG_Combined
Sorting_Variables[,,2] <- Environment
Sorting_Variables[,,3] <- Social
Sorting_Variables[,,4] <- Governance
Sorting_Variables[,,5] <- CO2
Sorting_Variables[,,6] <- CO2toRevenue
Sorting_Variables[1:21,,7] <- DCO2

N <- 4
ROE_portfolios <- array(0, dim = c(21,N+1,7))
ROIC_portfolios <- array(0, dim = c(21,N+1,7))
Re_portfolios <- array(0, dim = c(21,N+1,7))
OM_portfolios <- array(0, dim = c(21,N+1,7))
MB_portfolios <- array(0, dim = c(21,N+1,7))
DY_portfolios <- array(0, dim = c(21,N+1,7))
dimnames(ROE_portfolios) <- list(paste('Year',c(2003:2023)),paste(c('Lo', paste('P',2:(N-1), sep=''),'Hi','Hi-Lo')),
     c('ESG','E','S','G','CO2','CO2_Rev','Diff_CO2'))
dimnames(ROIC_portfolios) <- list(paste('Year',c(2003:2023)),paste(c('Lo', paste('P',2:(N-1), sep=''),'Hi','Hi-Lo')),
                                 c('ESG','E','S','G','CO2','CO2_Rev','Diff_CO2'))
dimnames(Re_portfolios) <- list(paste('Year',c(2003:2023)),paste(c('Lo', paste('P',2:(N-1), sep=''),'Hi','Hi-Lo')),
                                 c('ESG','E','S','G','CO2','CO2_Rev','Diff_CO2'))
dimnames(OM_portfolios) <- list(paste('Year',c(2003:2023)),paste(c('Lo', paste('P',2:(N-1), sep=''),'Hi','Hi-Lo')),
                                 c('ESG','E','S','G','CO2','CO2_Rev','Diff_CO2'))
dimnames(MB_portfolios) <- list(paste('Year',c(2002:2022)),paste(c('Lo', paste('P',2:(N-1), sep=''),'Hi','Hi-Lo')),
                                 c('ESG','E','S','G','CO2','CO2_Rev','Diff_CO2'))
dimnames(DY_portfolios) <- list(paste('Year',c(2002:2022)),paste(c('Lo', paste('P',2:(N-1), sep=''),'Hi','Hi-Lo')),
                                 c('ESG','E','S','G','CO2','CO2_Rev','Diff_CO2'))

for(t in 1:21){
  for(m in 1:7){
    order_variable <- order(Sorting_Variables[t,,m])
    variable_sorted <- Sorting_Variables[t,order_variable,m]
    Nt <- length(na.omit(variable_sorted))
    Kt <- floor(Nt/N)
    
    ROE_ordered <- ROE[t+1, order_variable[1:Nt]]
    ROIC_ordered <- ROIC[t+1, order_variable[1:Nt]]
    Re_ordered <- Re[t+1, order_variable[1:Nt]]
    OM_ordered <- OperatingMargin[t+1, order_variable[1:Nt]]
    MB_ordered <- MarketBook[t, order_variable[1:Nt]]
    DY_ordered <- DividendYield[t, order_variable[1:Nt]]
    
    for(n in 1:(N-1)){
      N_L <- 1 + (n-1)*Kt
      N_H <- n*Kt
      
      ROE_portfolios[t,n,m] <- mean(ROE_ordered[N_L:N_H], na.rm = TRUE)
      ROIC_portfolios[t,n,m] <- mean(ROIC_ordered[N_L:N_H], na.rm = TRUE)
      Re_portfolios[t,n,m] <- mean(Re_ordered[N_L:N_H], na.rm = TRUE)
      OM_portfolios[t,n,m] <- mean(OM_ordered[N_L:N_H], na.rm = TRUE)
      MB_portfolios[t,n,m] <- mean(MB_ordered[N_L:N_H], na.rm = TRUE)
      DY_portfolios[t,n,m] <- mean(DY_ordered[N_L:N_H], na.rm = TRUE)
      
    }
    N_L <- 1 + (N-1)*Kt
    N_H <- Nt
    
    ROE_portfolios[t,N,m] <- mean(ROE_ordered[N_L:N_H], na.rm = TRUE)
    ROIC_portfolios[t,N,m] <- mean(ROIC_ordered[N_L:N_H], na.rm = TRUE)
    Re_portfolios[t,N,m] <- mean(Re_ordered[N_L:N_H], na.rm = TRUE)
    OM_portfolios[t,N,m] <- mean(OM_ordered[N_L:N_H], na.rm = TRUE)
    MB_portfolios[t,N,m] <- mean(MB_ordered[N_L:N_H], na.rm = TRUE)
    DY_portfolios[t,N,m] <- mean(DY_ordered[N_L:N_H], na.rm = TRUE)
  }
  
}

ROE_portfolios[,N+1,] <- ROE_portfolios[,N,]-ROE_portfolios[,1,] 
ROIC_portfolios[,N+1,] <- ROIC_portfolios[,N,]-ROIC_portfolios[,1,]
Re_portfolios[,N+1,] <- Re_portfolios[,N,]-Re_portfolios[,1,]
OM_portfolios[,N+1,] <- OM_portfolios[,N,]-OM_portfolios[,1,]
MB_portfolios[,N+1,] <- MB_portfolios[,N,]-MB_portfolios[,1,]
DY_portfolios[,N+1,] <- DY_portfolios[,N,]-DY_portfolios[,1,]

Mean_ROE <- apply(ROE_portfolios,2:3,mean)
Sd_ROE <- apply(ROE_portfolios,2:3,sd)
t_stat_ROE <- sqrt(21-1)*Mean_ROE/Sd_ROE
round(Mean_ROE,2)
round(t_stat_ROE,2)

Mean_ROIC <- apply(ROIC_portfolios,2:3,mean)
Sd_ROIC <- apply(ROIC_portfolios,2:3,sd)
t_stat_ROIC <- sqrt(21-1)*Mean_ROIC/Sd_ROIC
round(Mean_ROIC,2)
round(t_stat_ROIC,2)

Mean_Re <- apply(Re_portfolios,2:3,mean)
Sd_Re <- apply(Re_portfolios,2:3,sd)
t_stat_Re <- sqrt(21-1)*Mean_Re/Sd_Re
round(Mean_Re,2)
round(t_stat_Re,2)

Mean_OM <- apply(OM_portfolios,2:3,mean)
Sd_OM <- apply(OM_portfolios,2:3,sd)
t_stat_OM <- sqrt(21-1)*Mean_OM/Sd_OM
round(Mean_OM,2)
round(t_stat_OM,2)

Mean_MB <- apply(MB_portfolios,2:3,mean, na.rm = TRUE)
Sd_MB <- apply(MB_portfolios,2:3,sd, na.rm = TRUE)
t_stat_MB <- sqrt(21-1)*Mean_MB/Sd_MB
round(Mean_MB,2)
round(t_stat_MB,2)

Mean_DY <- apply(DY_portfolios,2:3,mean)
Sd_DY <- apply(DY_portfolios,2:3,sd)
t_stat_DY <- sqrt(21-1)*Mean_DY/Sd_DY
round(Mean_DY,2)
round(t_stat_DY,2)

################################################################################
## Test whether long-short average return is explained by exposures to known factors
################################################################################
LS <- Re_portfolios[,N+1,]
I <- length(LS[1,])

#Test whether CAPM explains the mean LS_ESG portfolio return
X <-matrix(1,nrow = T_Y-1,ncol = 2)
X[,2] <- FF5[2:T_Y,1]
b_CAPM <- solve(t(X)%*%X)%*%t(X)%*%LS
res <- LS-X%*%b_CAPM
t_stat_CAPM <- matrix(NA, nrow = 2, ncol = I)
for(i in 1:I){
  var_b <- solve(t(X)%*%X)*as.numeric(sum(res[,i]^2))/(T_Y-1-2) 
  t_stat_CAPM[,i] <- b_CAPM[,i]/sqrt(diag(var_b))
}
colnames(b_CAPM) <- colnames(LS)
rownames(b_CAPM) <- c("Intercept", "MKT")
colnames(t_stat_CAPM) <- colnames(LS)
rownames(t_stat_CAPM) <- c("Intercept", "MKT")
round(b_CAPM,2)
round(t_stat_CAPM,2)

#Test whether the Fama-French 3 factor model explains the mean LS_ESG portfolio return
X <-matrix(1,nrow = T_Y-1,ncol = 4)
X[,2:4] <- FF5[2:T_Y,1:3]
b_FF3 <- solve(t(X)%*%X)%*%t(X)%*%LS
res <- LS-X%*%b_FF3
t_stat_FF3 <- matrix(NA, nrow = 4, ncol = I)
for(i in 1:I){
  var_b <- solve(t(X)%*%X)*as.numeric(sum(res[,i]^2))/(T_Y-1-4) 
  t_stat_FF3[,i] <- b_FF3[,i]/sqrt(diag(var_b))
}
colnames(b_FF3) <- colnames(LS)
rownames(b_FF3) <- c("Intercept", colnames(FF5)[1:3])
colnames(t_stat_FF3) <- colnames(LS)
rownames(t_stat_FF3) <- c("Intercept", colnames(FF5)[1:3])
round(b_FF3,2)
round(t_stat_FF3,2)

#Test whether the Fama-French 5 factor model explains the mean LS_ESG portfolio return
X <-matrix(1,nrow = T_Y-1,ncol = 6)
X[,2:6] <- FF5[2:T_Y,1:5]
b_FF5 <- solve(t(X)%*%X)%*%t(X)%*%LS
res <- LS-X%*%b_FF5
t_stat_FF5 <- matrix(NA, nrow = 6, ncol = I)
for(i in 1:I){
  var_b <- solve(t(X)%*%X)*as.numeric(sum(res[,i]^2))/(T_Y-1-6) 
  t_stat_FF5[,i] <- b_FF5[,i]/sqrt(diag(var_b))
}
colnames(b_FF5) <- colnames(LS)
rownames(b_FF5) <- c("Intercept", colnames(FF5))
colnames(t_stat_FF5) <- colnames(LS)
rownames(t_stat_FF5) <- c("Intercept", colnames(FF5))
round(b_FF5,2)
round(t_stat_FF5,2)