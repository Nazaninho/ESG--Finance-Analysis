
  #' title: Sustainable Finance
  #' subtitle: ESG 1
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



################################################################################
## Import Data - All data stored in the folder "Data" in the work directory
################################################################################
MarketReturn <- read.csv(file = 'Data/Market return.csv')
ESGReturn <- read.csv(file = 'Data/ESG return.csv')
ESGUncertaintyReturn <- read.csv(file = 'Data/ESG uncertainty return.csv')
RiskFreeRate <- read.csv(file = 'Data/Risk-free rate.csv')
StockReturn <- read.csv(file = 'Data/Stock return.csv')
StockBeta <- read.csv(file = 'Data/Stock beta.csv')
CovarianceFactors <- read.csv(file = 'Data/Covariance_Factors.csv')

ESG_A <- read.csv(file = 'Data/ESG rating agency A.csv')
ESG_B <- read.csv(file = 'Data/ESG rating agency B.csv')
ESG_C <- read.csv(file = 'Data/ESG rating agency C.csv')
ESG_D <- read.csv(file = 'Data/ESG rating agency D.csv')
ESG_E <- read.csv(file = 'Data/ESG rating agency E.csv')
ESG_F <- read.csv(file = 'Data/ESG rating agency F.csv')

#Number of years of returns
T_R <- length(MarketReturn[,1])
#Number of years of ESG scores
T_esg <- length(ESG_A[,1])
#Number of years of returns before ESG scores - picks out the year 2000
T_estimation <- T_R-T_esg+1
#Number of stocks
N <- length(StockReturn[1,])-1
#Number of rating agencies
K <- 6

################################################################################
## Initial Data Processing
################################################################################
Rme <- matrix(MarketReturn[,2]-RiskFreeRate[,2], nrow = T_R, ncol = 1)*100 #Excess market return vector in percent per year
Re <- as.matrix(sweep(StockReturn[,2:(N+1)], 1, RiskFreeRate[,2], FUN = "-"))*100 #Excess stock return matrix in percent per year
beta <- as.matrix(StockBeta[,2:(N+1)]) #Beta matrix
Covariance_Factors <- as.matrix(CovarianceFactors[,2:4])

ESG_hat <- array(NA,dim = c(T_esg,N,K)) #Generate a three-dimensional array to store all the standardized ratings
ESG_hat[,,1] <- -(as.matrix(ESG_A[,2:(N+1)]) - apply(as.matrix(ESG_A[,2:(N+1)]),1,mean))/apply(as.matrix(ESG_A[,2:(N+1)]),1,sd) #Store the standardized rating of agency A in the first layer of ESG_hat 
ESG_hat[,,2] <- (as.matrix(ESG_B[,2:(N+1)]) - apply(as.matrix(ESG_B[,2:(N+1)]),1,mean))/apply(as.matrix(ESG_B[,2:(N+1)]),1,sd)
ESG_hat[,,3] <- (as.matrix(ESG_C[,2:(N+1)]) - apply(as.matrix(ESG_C[,2:(N+1)]),1,mean))/apply(as.matrix(ESG_C[,2:(N+1)]),1,sd)
ESG_hat[,,4] <- (as.matrix(ESG_D[,2:(N+1)]) - apply(as.matrix(ESG_D[,2:(N+1)]),1,mean))/apply(as.matrix(ESG_D[,2:(N+1)]),1,sd)
ESG_hat[,,5] <- (as.matrix(ESG_E[,2:(N+1)]) - apply(as.matrix(ESG_E[,2:(N+1)]),1,mean))/apply(as.matrix(ESG_E[,2:(N+1)]),1,sd)
ESG_hat[,,6] <- (as.matrix(ESG_F[,2:(N+1)]) - apply(as.matrix(ESG_F[,2:(N+1)]),1,mean))/apply(as.matrix(ESG_F[,2:(N+1)]),1,sd)

ESG_mean <-  apply(ESG_hat,c(1,2),mean) #Calculate the mean rating across rating agencies for each stock and year
ESG <-  ESG_mean/apply(ESG_mean,1,sd)
ESG_uncertainty <- apply(ESG_hat,1:2,sd) #Calculate the variance of ratings across agencies for each stock and year

Dates_esg <- c(ESG_A[,1]) #Store the dates for ESG scores
################################################################################
## Univariate Sorts
################################################################################
M <- 5 #Number of portfolios

SortingVariables <- array(NA, dim = c(T_esg,N,3))
SortingVariables[,,1] <- beta
SortingVariables[,,2] <- ESG
SortingVariables[,,3] <- ESG_uncertainty

Portfolios_univariate_sorted <- array(NA, dim = c(T_esg-1, M+1, 3))
dimnames(Portfolios_univariate_sorted) <- list( paste("Year", Dates_esg[-1]),
                                                paste(c("Lo", paste("P",c(2:(M-1))), "Hi", "Hi-Lo")),
                                                c("beta", "ESG", "ESG uncertainty"))
Portfolios_univariate_sorted_exposures <- array(NA, dim = c(T_esg-1, M+1, 3, 3))
dimnames(Portfolios_univariate_sorted_exposures) <- list( paste("Year", Dates_esg[-23]),
                                                paste(c("Lo", paste("P",c(2:(M-1))), "Hi", "Hi-Lo")),
                                                c("beta portfolios", "ESG portfolios", "ESG uncertainty portfolios"),
                                                c("beta exposure", "ESG exposure", "ESG uncertainty exposure"))

for (t in 1:(T_esg-1)){
  for (k in 1:3){
    order_SV <- order(SortingVariables[t,,k])
    Re_ordered <- Re[T_estimation+t,order_SV]
    SV_ordered <- SortingVariables[t,order_SV,]
    
    for (m in 1:M){
      N_L <- 1+(m-1)*N/M #The first stock to enter portfolio m
      N_H <- m*N/M #The last stock to enter portfolio m
      Portfolios_univariate_sorted[t,m,k]<- mean(Re_ordered[N_L:N_H]) #Calculate the excess return of k sorted portfolio m
      Portfolios_univariate_sorted_exposures[t,m,k,] <- apply(SV_ordered[N_L:N_H,],2,mean)
    }
  }
}

Portfolios_univariate_sorted[,M+1,] <- Portfolios_univariate_sorted[,M,]-Portfolios_univariate_sorted[,1,]
Mean_Re_univariate_sorted <- apply(Portfolios_univariate_sorted,2:3,mean) #Calculate mean excess portfolio return for each beta-sorted portfolio 
t_stat_Mean_Re_univariate_sorted <- sqrt(T_esg-2)*Mean_Re_univariate_sorted/apply(Portfolios_univariate_sorted,2:3,sd)
print("Mean excess returns univariate sorted:")
round(t(Mean_Re_univariate_sorted),2)
print("t-stat mean excess returns univariate sorted:")
round(t(t_stat_Mean_Re_univariate_sorted),2)

Portfolios_univariate_sorted_exposures[,M+1,,] <- Portfolios_univariate_sorted_exposures[,M,,]-Portfolios_univariate_sorted_exposures[,1,,]
Mean_exposure <- apply(Portfolios_univariate_sorted_exposures,2:4,mean)
print("Mean beta exposure")
round(t(Mean_exposure[,,1]),2)
print("Mean ESG exposure")
round(t(Mean_exposure[,,2]),2)
print("Mean ESG uncertainty exposure")
round(t(Mean_exposure[,,3]),2)

################################################################################
## Test of CAPM Using Long-Short Portfolios Based on Univariate Sorts
################################################################################
#Regress long-short portfolios on market and test whether intercept is 0
X <- matrix(1, nrow = T_esg-1, ncol = 2)
X[,2]<-Rme[(T_estimation+1):T_R]

CAPM_test <- matrix(NA,nrow = 4, ncol = 3)
rownames(CAPM_test) <- c('Intercept', 't-stat', 'Coef Market', 't-stat')
colnames(CAPM_test) <- c('Beta','ESG','ESG uncertainty')

for( k in 1:3){
  LS_k <- Portfolios_univariate_sorted[,M+1,k]
  reg_coef_k <- solve(t(X)%*%X)%*%t(X)%*%LS_k
  res <- LS_k - X%*%reg_coef_k #Regression residuals
  cov_reg_coef_k <- solve(t(X)%*%X)*as.numeric(sum(res^2)/(T_esg-1-2))
  t_stat_k <- reg_coef_k/sqrt(diag(cov_reg_coef_k))
  CAPM_test[c(1,3),k] <- reg_coef_k
  CAPM_test[c(2,4),k] <- t_stat_k
}

round(CAPM_test,2)

################################################################################
## Estimate Factor Models and News Reactions
################################################################################
ESG_news <- ESG[2:T_esg,]-ESG[1:(T_esg-1),]
ESG_uncertainty_news <- ESG_uncertainty[2:T_esg,]-ESG_uncertainty[1:(T_esg-1),]

ESG_model <- matrix(NA, nrow = (T_esg-1), ncol = 2)
ThreeFactor_model <- matrix(NA, nrow = (T_esg-1), ncol = 4)
News_model <- matrix(NA, nrow = (T_esg-1), ncol = 3)

Exposures_ESG_model <- array(NA, dim = c(T_esg-1,2,3))
dimnames(Exposures_ESG_model) <- list(paste("Year", 2001:2022),
                                      c("Intercept", "Lambda_esg"),
                                      c("beta", "ESG", "ESG uncertainty"))
Exposures_ThreeFactor_model <- array(NA, dim = c(T_esg-1,4,3))
dimnames(Exposures_ThreeFactor_model) <- list(paste("Year", 2001:2022),
                                      c("Intercept", "Lambda_m", "Lambda_esg", "Lambda_esgunc"),
                                      c("beta", "ESG", "ESG uncertainty"))

Exposures_News_model <- array(NA, dim = c(T_esg-1,3,2))
dimnames(Exposures_News_model) <- list(paste("Year", 2001:2022),
                                              c("Intercept", "b_esg", "b_esgunc"),
                                              c("ESG news", "ESG uncertainty news"))
for (t in 1:(T_esg-1)){
  Ret <- Re[T_R-T_esg + 1 + t,]
  
  X <- matrix(1, nrow = 2, ncol = N)
  X[2,] <- ESG[t,]
  Omega <- t(X)%*%solve(X%*%t(X))
  ESG_model[t,] <- Ret%*%Omega
  Exposures_ESG_model[t,,1] <- beta[t,]%*%Omega
  Exposures_ESG_model[t,,2] <- ESG[t,]%*%Omega
  Exposures_ESG_model[t,,3] <- ESG_uncertainty[t,]%*%Omega
  
  X <- matrix(1, nrow = 4, ncol = N)
  X[2,] <- beta[t,]
  X[3,] <- ESG[t,]
  X[4,] <- ESG_uncertainty[t,]
  Omega3F <- t(X)%*%solve(X%*%t(X))
  ThreeFactor_model[t,] <- Ret%*%Omega3F
  Exposures_ThreeFactor_model[t,,1] <- beta[t,]%*%Omega3F
  Exposures_ThreeFactor_model[t,,2] <- ESG[t,]%*%Omega3F
  Exposures_ThreeFactor_model[t,,3] <- ESG_uncertainty[t,]%*%Omega3F
  
  X <- matrix(1,nrow = 3, ncol = N)
  X[2,] <- ESG_news[t,]
  X[3,] <- ESG_uncertainty_news[t,]
  Omega <- t(X)%*%solve(X%*%t(X))
  News_model[t,]<- Ret%*%Omega
  Exposures_News_model[t,,1] <- ESG_news[t,]%*%Omega
  Exposures_News_model[t,,2] <- ESG_uncertainty_news[t,]%*%Omega
}

Model_ESG <- matrix(NA, nrow = 4, ncol = 2)
colnames(Model_ESG) <- c('Intercept', 'Lambda_ESG')
rownames(Model_ESG) <- c('Mean','Sdev','Sharpe Ratio','t-stat')
Model_ESG[1,] <- apply(ESG_model, 2, mean)
Model_ESG[2,] <- apply(ESG_model, 2, sd)
Model_ESG[3,] <- Model_ESG[1,]/Model_ESG[2,] 
Model_ESG[4,] <- sqrt(T_esg-2)*Model_ESG[3,]
round(Model_ESG,2)
print("Mean exposure ESG model")
round(t(apply(Exposures_ESG_model,2:3,mean)),2)

Model_3F <- matrix(NA, nrow = 4, ncol = 4)
colnames(Model_3F) <- c('Intercept', 'Lambda_m', 'Lambda_ESG', 'Lambda_ESG_uncertainty')
rownames(Model_3F) <- c('Mean','Sdev','Sharpe Ratio','t-stat')
Model_3F[1,] <- apply(ThreeFactor_model, 2, mean)
Model_3F[2,] <- apply(ThreeFactor_model, 2, sd)
Model_3F[3,] <- Model_3F[1,]/Model_3F[2,] 
Model_3F[4,] <- sqrt(T_esg-2)*Model_3F[3,]
round(Model_3F,2)
print("Mean exposure 3-factor model")
round(t(apply(Exposures_ThreeFactor_model,2:3,mean)),2)
Lambda_3F <- ThreeFactor_model[,2:4]
print("1 standard deviation of exposure is:")
round(c(mean(apply(beta,1,sd)), mean(apply(ESG,1,sd)), mean(apply(ESG_uncertainty,1,sd))),2)
print("Economic magnitude - 1 standard deviation increase in exposure")
round(apply(Lambda_3F,2,mean)*c(mean(apply(beta,1,sd)), mean(apply(ESG,1,sd)), mean(apply(ESG_uncertainty,1,sd))),2)
print("max exposure - min exposure is:")
round(c(max(beta)-min(beta), max(ESG)-min(ESG), max(ESG_uncertainty)-min(ESG_uncertainty)),2)
print("Economic magnitude - max exposure - min exposure")
round(apply(Lambda_3F,2,mean)*c(max(beta)-min(beta), max(ESG)-min(ESG), max(ESG_uncertainty)-min(ESG_uncertainty)),2)

plot(Dates_esg[-1],Rme[(T_estimation+1):T_R], type = 'l', lwd = 2,
     ylab="Excess return", xlab = "")
lines(Dates_esg[-1], Lambda_3F[,1], lwd = 2, col = 'red')
legend(Dates_esg[15], -20, legend=c('Market','Lambda_m'),
       col=c('black','red'), lty=1, cex=0.8)


cor(Rme[(T_estimation+1):T_R],Lambda_3F[,1])

print("Shopping list in year 2021:")
round(Omega3F[1:5,],3)
################################################################################
## Test of 3 factor model using long-short portfolios from univariate sorts
################################################################################
#Regress long-short portfolios on 3-factor model and test whether intercept is 0
X <- matrix(1, nrow = T_esg-1, ncol = 4)
X[,2:4]<-Lambda_3F

ThreeFactor_model_test <- matrix(NA, nrow = 8, ncol = 3)
rownames(ThreeFactor_model_test) <- c('Intercept','t_stat', 'Coef Lambda_m','t-stat', 'Coef Lambda_ESG','t-stat', 'Coef Lambda_ESG_uncertainty','t-stat')
colnames(ThreeFactor_model_test) <- c('Beta','ESG','ESG uncertainty')


for( k in 1:3){
  LS_k <- Portfolios_univariate_sorted[,M+1,k]
  reg_coef_k <- solve(t(X)%*%X)%*%t(X)%*%LS_k
  res <- LS_k - X%*%reg_coef_k #Regression residuals
  cov_reg_coef_k <- solve(t(X)%*%X)*as.numeric(sum(res^2)/(T_esg-1-4))
  t_stat_k <- reg_coef_k/sqrt(diag(cov_reg_coef_k))
  ThreeFactor_model_test[c(1,3,5,7),k] <- reg_coef_k
  ThreeFactor_model_test[c(2,4,6,8),k] <- t_stat_k
}

round(ThreeFactor_model_test,2)

################################################################################
## ESG news and realized returns
################################################################################
Model_news <- matrix(NA, nrow = 3, ncol = 3)
colnames(Model_news) <- c('Intercept','ESG news', 'ESG uncertainty news')
rownames(Model_news) <- c('Mean','Sdev','t-stat')
Model_news[1,] <- apply(News_model, 2, mean)
Model_news[2,] <- apply(News_model, 2, sd)
Model_news[3,] <- sqrt(T_esg-2)*Model_news[1,]/Model_news[2,] 
round(Model_news,2)

################################################################################
## Double Sorts
################################################################################
M <- 5 #Number of groups per sorting variable

Portfolios_beta_ESG_sorted <- array(NA, c(T_esg-1,M,M+1)) #Three dimensional array - beta sort along second dimension and ESG sort along third
Portfolios_beta_ESG_uncertainty_sorted <- array(NA, c(T_esg-1,M,M+1)) #Three dimensional array - beta sort along second dimension and ESG uncertainty sort along third
dimnames(Portfolios_beta_ESG_sorted) <- list(paste("Year", Dates_esg[-1]),
                                             paste(c("Lo beta", paste("beta",2:(M-1)), "Hi beta")),
                                             paste(c("Lo ESG", paste("ESG",2:(M-1)), "Hi ESG", "Hi-Lo ESG")))
dimnames(Portfolios_beta_ESG_uncertainty_sorted) <- list(paste("Year", Dates_esg[-1]),
                                             paste(c("Lo beta", paste("beta",2:(M-1)), "Hi beta")),
                                             paste(c("Lo ESG unc", paste("ESG unc",2:(M-1)), "Hi ESG unc", "Hi-Lo ESG unc")))


Portfolios_beta_ESG_sorted_exposures <- array(NA, c(T_esg-1,M,M+1,3))
Portfolios_beta_ESG_uncertainty_sorted_exposures <- array(NA, c(T_esg-1,M,M+1,3))
dimnames(Portfolios_beta_ESG_sorted_exposures) <- list(paste("Year", Dates_esg[-23]),
                                             paste(c("Lo beta", paste("beta",2:(M-1)), "Hi beta")),
                                             paste(c("Lo ESG", paste("ESG",2:(M-1)), "Hi ESG", "Hi-Lo ESG")),
                                             c("beta exposure", "ESG exposure", "ESG uncertainty exposure"))
dimnames(Portfolios_beta_ESG_uncertainty_sorted_exposures) <- list(paste("Year", Dates_esg[-23]),
                                                         paste(c("Lo beta", paste("beta",2:(M-1)), "Hi beta")),
                                                         paste(c("Lo ESG unc", paste("ESG unc",2:(M-1)), "Hi ESG unc", "Hi-Lo ESG unc")),
                                                         c("beta exposure", "ESG exposure", "ESG uncertainty exposure"))

for (t in 1:(T_esg-1)){
  order_beta_t <- order(beta[t,]) #Get the order of betas in year t-1 from smallest to largest
  SV_beta_ordered <- SortingVariables[t,order_beta_t,]
  ESG_beta_ordered_t <- ESG[t,order_beta_t] #Order the ESG scores based on betas in same year
  ESG_uncertainty_beta_ordered_t <- ESG_uncertainty[t,order_beta_t] #Order ESG uncertainty based on betas in same year
  Re_beta_ordered_t <- Re[T_R-T_esg + 1 + t,order_beta_t] #Sort stocks such that the first element is the excess return on the stock with the lowest beta in year t-1  
  
  for (m in 1:M){
    M_L <- 1+(m-1)*N/M #The first stock in beta-group m
    M_H <- m*N/M #The last stock in beta-group m
    
    Re_beta_m <- Re_beta_ordered_t[M_L:M_H] #Excess returns on the 100 stocks in beta-group m
    ESG_beta_m <- ESG_beta_ordered_t[M_L:M_H] #ESG scores on the 100 stocks in beta-group m
    ESG_uncertainty_beta_m <- ESG_uncertainty_beta_ordered_t[M_L:M_H] #delta scores on the 100 stocks in beta-group m
    SV_beta_ordered_m <- SV_beta_ordered[M_L:M_H,]
    
    Re_beta_m_ESG_ordered <- Re_beta_m[order(ESG_beta_m)] #Order the excess returns on stocks in beta-group m based on their ESG scores
    Re_beta_m_ESG_uncertainty_ordered <- Re_beta_m[order(ESG_uncertainty_beta_m)] #Order the excess returns on stocks in beta-group m based on their ESG uncertainty scores
    SV_beta_m_ESG_ordered <- SV_beta_ordered_m[order(ESG_beta_m),]
    SV_beta_m_ESG_uncertainty_ordered <- SV_beta_ordered_m[order(ESG_uncertainty_beta_m),]
      
    for (n in 1:M){
      N_L <- 1+(n-1)*N/M^2 #First stock in beta-group m to enter portfolio (m,n)
      N_H <- n*N/M^2 #Last stock in beta-group m to enter portfolio (m,n)
      
      Portfolios_beta_ESG_sorted[t,m,n]<- mean(Re_beta_m_ESG_ordered[N_L:N_H]) #Calculate the excess return of beta-ESG sorted portfolio (m,n)
      Portfolios_beta_ESG_uncertainty_sorted[t,m,n]<- mean(Re_beta_m_ESG_uncertainty_ordered[N_L:N_H]) #Calculate the excess return of beta-delta sorted portfolio (m,n)
      
      Portfolios_beta_ESG_sorted_exposures[t,m,n,]<-apply(SV_beta_m_ESG_ordered[N_L:N_H,],2,mean)
      Portfolios_beta_ESG_uncertainty_sorted_exposures[t,m,n,]<-apply(SV_beta_m_ESG_uncertainty_ordered[N_L:N_H,],2,mean)
    }
  }
}

Portfolios_beta_ESG_sorted[,,M+1] <- Portfolios_beta_ESG_sorted[,,M]-Portfolios_beta_ESG_sorted[,,1] 
Portfolios_beta_ESG_uncertainty_sorted[,,M+1] <- Portfolios_beta_ESG_uncertainty_sorted[,,M]-Portfolios_beta_ESG_uncertainty_sorted[,,1] 
Portfolios_beta_ESG_sorted_exposures[,,M+1,] <- Portfolios_beta_ESG_sorted_exposures[,,M,]-Portfolios_beta_ESG_sorted_exposures[,,1,] 
Portfolios_beta_ESG_uncertainty_sorted_exposures[,,M+1,] <- Portfolios_beta_ESG_uncertainty_sorted_exposures[,,M,]-Portfolios_beta_ESG_uncertainty_sorted_exposures[,,1,] 

Mean_Re_beta_ESG_sorted <- apply(Portfolios_beta_ESG_sorted,2:3,mean) 
Mean_Re_beta_ESG_uncertainty_sorted <- apply(Portfolios_beta_ESG_uncertainty_sorted,2:3,mean) 
Mean_beta_ESG_sorted_exposures <- apply(Portfolios_beta_ESG_sorted_exposures,2:4,mean) 
Mean_beta_ESG_uncertainty_sorted_exposures <- apply(Portfolios_beta_ESG_uncertainty_sorted_exposures,2:4,mean) 

round(Mean_Re_beta_ESG_sorted,2)
round(Mean_Re_beta_ESG_uncertainty_sorted,2)
round(Mean_beta_ESG_sorted_exposures,2)
round(Mean_beta_ESG_uncertainty_sorted_exposures,2)


################################################################################
## Alternative Factors Based on Double-Sorted Long-Short Portfolios
################################################################################
LS_double_sort <- matrix(NA, nrow = T_esg-1, ncol = 2)
colnames(LS_double_sort) <- c("ESG", "ESG uncertainty")
LS_double_sort[,1] <- apply(Portfolios_beta_ESG_sorted[,,M+1],1,mean)
LS_double_sort[,2] <- apply(Portfolios_beta_ESG_uncertainty_sorted[,,M+1],1,mean)

Summary_LS_double_sort_vs_ThreeFactor <- matrix(NA, nrow = 7, ncol = 2)
colnames(Summary_LS_double_sort_vs_ThreeFactor) <- c('ESG','ESG uncertainty')
rownames(Summary_LS_double_sort_vs_ThreeFactor) <- c('mean(lambda)', 'mean(LS double)','sdev(lambda)','sdev(LS double)',
                                                     'Sharpe Ratio lambda', 'Sharpe Ratio LS double', 'Corr(LS double,lambda)')

Summary_LS_double_sort_vs_ThreeFactor[1,] <- apply(Lambda_3F[,2:3],2,mean) 
Summary_LS_double_sort_vs_ThreeFactor[2,] <- apply(LS_double_sort,2,mean) 
Summary_LS_double_sort_vs_ThreeFactor[3,] <- apply(Lambda_3F[,2:3],2,sd) 
Summary_LS_double_sort_vs_ThreeFactor[4,] <- apply(LS_double_sort,2,sd) 
Summary_LS_double_sort_vs_ThreeFactor[5,] <- Summary_LS_double_sort_vs_ThreeFactor[1,]/Summary_LS_double_sort_vs_ThreeFactor[3,]
Summary_LS_double_sort_vs_ThreeFactor[6,] <- Summary_LS_double_sort_vs_ThreeFactor[2,]/Summary_LS_double_sort_vs_ThreeFactor[4,]
Summary_LS_double_sort_vs_ThreeFactor[7,1] <- cor(LS_double_sort[,1],Lambda_3F[,2])
Summary_LS_double_sort_vs_ThreeFactor[7,2] <- cor(LS_double_sort[,2],Lambda_3F[,3])
round(Summary_LS_double_sort_vs_ThreeFactor,2)

plot(LS_double_sort[,1], xaxt = "n", type = 'l', ylim = c(-10,5), lwd = 3,
     main = "Alternative ESG Factors", xlab="", ylab="Excess return")
lines(Lambda_3F[,2], type = 'l', col = 'red', lwd = 3)
lines(sd(LS_double_sort[,1])*Lambda_3F[,2]/sd(Lambda_3F[,2]), type = 'p', col = 'blue', lwd = 3)
legend(T_esg-12, 5, legend=c('LS Factor','Lambda_ESG','Scaled Lambda_ESG'),
       col=c('black','red','blue'), lty=1, cex=0.8)
axis(1, at=1:(T_esg-1), labels=Dates_esg[2:T_esg])

plot(LS_double_sort[,2], xaxt = "n", type = 'l', ylim = c(-10,15), lwd = 3,
     main = "Alternative ESG Uncertainty Factors", xlab="", ylab="Excess return")
lines(Lambda_3F[,3], type = 'l', col = 'red', lwd = 3)
lines(sd(LS_double_sort[,2])*Lambda_3F[,3]/sd(Lambda_3F[,3]), type = 'p', col = 'blue', lwd = 3)
legend(10, -4, legend=c('LS Factor','Lambda_ESG_uncertainty','Scaled LS Lambda_ESG'),
       col=c('black','red','blue'), lty=1, cex=0.8)
axis(1, at=1:(T_esg-1), labels=Dates_esg[2:T_esg])



################################################################################
## Portfolio Choice
################################################################################
X <- matrix(1,nrow = T_estimation, ncol = 4)
X[,2:4] <- Covariance_Factors[1:T_estimation,]
B <- solve(t(X)%*%X)%*%t(X)%*%Re[1:T_estimation,]
residuals <- Re[1:T_estimation,] - X%*%B
sigma_i <- apply(residuals,2,sd)

Cov_Re <-  t(B[2:4,])%*%cov(Covariance_Factors)%*%B[2:4,] + diag(sigma_i^2)
ERe <- apply(Re[1:T_estimation,],2,mean)

weights_max_sharpe <- solve(Cov_Re)%*%ERe/sum(solve(Cov_Re)%*%ERe)

Re_max_sharpe <- Re[(T_estimation+1):T_R,]%*%weights_max_sharpe
mean(Re_max_sharpe)
sd(Re_max_sharpe)

psi_esg <- c(0:22)/1000*100 #Need to multiply by 100 because returns are in percent rather than decimals!
psi_sigma <- c(8:12)^2/100 #Need to divide by 100 because returns are in percent rather than decimals!
K_esg <- length(psi_esg)
K_sigma <- length(psi_sigma)

ESG_used_for_optimization <- array(0, dim = c(T_esg, N, 7))
ESG_used_for_optimization[,,1] <- ESG
ESG_used_for_optimization[,,-1] <- ESG_hat

Optimal_Portfolios_return <- array(NA,dim = c(T_esg-1,K_esg,K_sigma, 7))
dimnames(Optimal_Portfolios_return) <- list(paste("Year", Dates_esg[-1]),
                                     paste("Psi esg =", psi_esg/100),
                                     paste("Psi sigma =", psi_sigma*100),
                                     paste("Used for optimization:",c("ESG", "A", "B", "C", "D", "E", "F")))
Optimal_Portfolios_ESG_score <- array(NA, dim = c(T_esg-1,K_esg,K_sigma, 7, 7))
dimnames(Optimal_Portfolios_ESG_score) <- list(paste("Year", Dates_esg[-1]),
                                     paste("Psi esg =", psi_esg/100),
                                     paste("Psi sigma =", psi_sigma*100),
                                     paste("Used for optimization:",c("ESG", "A", "B", "C", "D", "E", "F")),
                                     paste("Used for evaluation:",c("ESG", "A", "B", "C", "D", "E", "F")))
SigmaInv <- solve(Cov_Re)
SigmaInv_One <- SigmaInv%*%matrix(1, nrow = N, ncol = 1)
mu_SigmaInv_One <- as.numeric(ERe%*%SigmaInv_One)
One_SigmaInv_One <- as.numeric(matrix(1, nrow = 1, ncol = N)%*%SigmaInv_One)
SigmaInv_mu <- SigmaInv%*%ERe

for(t in 1:(T_esg-1)){
  for(k in 1:7){
    ESG_k <- ESG_used_for_optimization[t,,k]
    ESG_SigmaInv_One <- as.numeric(ESG_k%*%SigmaInv_One)
    SigmaInv_ESG <- SigmaInv%*%ESG_k
    
    for(n in 1:K_esg){
      for(m in 1:K_sigma){
        psi_omega <- (mu_SigmaInv_One + psi_esg[n]*ESG_SigmaInv_One - psi_sigma[m])/One_SigmaInv_One
        
        portfolio_weights <- (SigmaInv_mu + SigmaInv_ESG*psi_esg[n] - psi_omega*SigmaInv_One)/psi_sigma[m]
        Optimal_Portfolios_return[t,n,m,k] <- Re[T_estimation+t,]%*%portfolio_weights
        
        for( j in 1:7){
          Optimal_Portfolios_ESG_score[t,n,m,k,j] <- ESG_used_for_optimization[t,,j]%*%portfolio_weights  
        }
      }
    }  
  }
}

Average_Portfolio_ExcessReturn <- apply(Optimal_Portfolios_return,2:4,mean)
Volatility_Portfolio_ExcessReturn <- apply(Optimal_Portfolios_return,2:4,sd)
Sharpe_Ratios_Portfolio_ExcessReturn <- Average_Portfolio_ExcessReturn/Volatility_Portfolio_ExcessReturn
Mean_Portfolio_ESG <- apply(Optimal_Portfolios_ESG_score,2:5,mean)

round(Average_Portfolio_ExcessReturn[,,1],2)
round(Volatility_Portfolio_ExcessReturn[,,1],2)
round(Sharpe_Ratios_Portfolio_ExcessReturn[,,1],2)
round(Mean_Portfolio_ESG[,,1,1],2)

#Let's look at how the an optimal portfolio look when optimized using one ESG score but is evaluated using other ratings
print(paste("Psi_esg =", psi_esg[K_esg]/100, "Psi_sigma =", psi_sigma[1]*100))
round(Mean_Portfolio_ESG[K_esg,1,,],2)

print(paste("Psi_esg =", psi_esg[K_esg]/100, "Psi_sigma =", psi_sigma[K_sigma]*100))
round(Mean_Portfolio_ESG[K_esg,K_sigma,,],2)

print(paste("Psi_esg =", psi_esg[floor(K_esg/2)]/100, "Psi_sigma =", psi_sigma[1]*100))
round(Mean_Portfolio_ESG[floor(K_esg/2),1,,],2)

print(paste("Psi_esg =", psi_esg[floor(K_esg/3)]/100, "Psi_sigma =", psi_sigma[1]*100))
round(Mean_Portfolio_ESG[floor(K_esg/3),1,,],2)

print(paste("Psi_esg =", psi_esg[floor(K_esg/2)]/100, "Psi_sigma =", psi_sigma[floor(K_sigma/2)]*100))
round(Mean_Portfolio_ESG[floor(K_esg/2)+1,floor(K_sigma/2),,],2)

################################################################################
## Appendix material
################################################################################
################################################################################
## Target ESG score - An ESG adjusted mean-variance efficient frontier
################################################################################
ESG_target <- c(-7:7)
K_esg <- length(ESG_target)
a <- c(c(7:30)^2/100)
psi_sigma <- c(a,0,-sort(a, decreasing = TRUE))
K_sigma = length(psi_sigma)

psi_omega <- array(NA,dim = c(T_esg-1,K_sigma, K_esg))
dimnames(psi_omega) <- list(paste("Year",2000:2021),
                            paste("psi_sigma", psi_sigma),
                            paste("ESG target", ESG_target))
psi_esg <- array(NA,dim = c(T_esg-1,K_sigma, K_esg))
dimnames(psi_esg) <- list(paste("Year",2000:2021),
                            paste("psi_sigma", psi_sigma),
                            paste("ESG target", ESG_target))

Portfolio_returns <- array(NA,dim = c(T_esg-1,K_sigma,K_esg))
dimnames(Portfolio_returns) <- list(paste("Year",2001:2022),
                          paste("psi_sigma", psi_sigma),
                          paste("ESG target", ESG_target))

Portfolio_expected_returns<- array(NA,dim = c(T_esg-1,K_sigma,K_esg))
dimnames(Portfolio_expected_returns) <- list(paste("Year",2000:2021),
                                    paste("psi_sigma", psi_sigma),
                                    paste("ESG target", ESG_target))
Portfolio_expected_variance<- array(NA,dim = c(T_esg-1,K_sigma,K_esg))
dimnames(Portfolio_expected_variance) <- list(paste("Year",2000:2021),
                                             paste("psi_sigma", psi_sigma),
                                             paste("ESG target", ESG_target))
Portfolio_ESG <- array(NA,dim = c(T_esg-1,K_sigma,K_esg))
dimnames(Portfolio_ESG) <- list(paste("Year",2000:2021),
                          paste("psi_sigma", psi_sigma),
                          paste("ESG target", ESG_target))

portfolio_weights <- array(NA, dim = c(N,T_esg-1,K_sigma,K_esg))

for (t in 1:(T_esg-1)){
  ESGt <- ESG[t,]
  ESGt_SigmaInv_One <- as.numeric(ESGt%*%SigmaInv_One)
  ESGt_SigmaInv_mu <- as.numeric(ESGt%*%SigmaInv_mu)
  SigmaInv_ESGt <- SigmaInv%*%ESGt
  ESGt_SigmaInv_ESGt <- as.numeric(ESGt%*%SigmaInv_ESGt)
  
  for (n in 1:K_sigma){
    if (psi_sigma[n] == 0){ #Calculate minimum variance portfolio subject to ESG constraint ESGp=ESG_target
      for (m in 1:K_esg){
        psi_omega[t,n,m] <- ( ESGt_SigmaInv_ESGt - ESGt_SigmaInv_One*ESG_target[m] )/(ESGt_SigmaInv_ESGt*One_SigmaInv_One - ESGt_SigmaInv_One^2)
        psi_esg[t,n,m] <- (ESG_target[m] - ESGt_SigmaInv_One*psi_omega[t,n,m])/ESGt_SigmaInv_ESGt
        
        portfolio_weights[,t,n,m] <- SigmaInv_ESGt*psi_esg[t,n,m] + psi_omega[t,n,m]*SigmaInv_One
        
        Portfolio_returns[t,n,m] <- Re[T_estimation+t,]%*%portfolio_weights[,t,n,m]
        Portfolio_expected_returns[t,n,m] <- ERe%*%portfolio_weights[,t,n,m]
        Portfolio_expected_variance[t,n,m] <- portfolio_weights[,t,n,m]%*%Cov_Re%*%portfolio_weights[,t,n,m]
        Portfolio_ESG[t,n,m] <- ESGt%*%portfolio_weights[,t,n,m]
      }
    }
    else{
      
      for (m in 1:K_esg){
        psi_omega[t,n,m] <- ( (mu_SigmaInv_One - psi_sigma[n])*ESGt_SigmaInv_ESGt 
                             + (psi_sigma[n]*ESG_target[m] - ESGt_SigmaInv_mu)*ESGt_SigmaInv_One
                            )/(ESGt_SigmaInv_ESGt*One_SigmaInv_One - ESGt_SigmaInv_One^2)
        psi_esg[t,n,m] <- (psi_sigma[n]*ESG_target[m] - ESGt_SigmaInv_mu + psi_omega[t,n,m]*ESGt_SigmaInv_One)/ESGt_SigmaInv_ESGt
        
        portfolio_weights[,t,n,m] <- SigmaInv_mu/psi_sigma[n] + SigmaInv_ESGt*psi_esg[t,n,m]/psi_sigma[n] - psi_omega[t,n,m]*SigmaInv_One/psi_sigma[n]
        
        Portfolio_returns[t,n,m] <- Re[T_estimation+t,]%*%portfolio_weights[,t,n,m]
        Portfolio_expected_returns[t,n,m] <- ERe%*%portfolio_weights[,t,n,m]
        Portfolio_expected_variance[t,n,m] <- portfolio_weights[,t,n,m]%*%Cov_Re%*%portfolio_weights[,t,n,m]
        Portfolio_ESG[t,n,m] <- ESGt%*%portfolio_weights[,t,n,m]
      }
    }
  }
}


MVE_weights <- matrix(0, nrow = N, ncol = K_sigma)
MVE_expected_return <- matrix(0, nrow = K_sigma)
MVE_expected_variance <- matrix(0, nrow = K_sigma)

for (n in 1:K_sigma){
  if (psi_sigma[n] == 0){
    MVE_weights[,n] = SigmaInv_One/One_SigmaInv_One
    MVE_expected_return[n] <- MVE_weights[,n]%*%ERe
    MVE_expected_variance[n] <- MVE_weights[,n]%*%Cov_Re%*%MVE_weights[,n]
  }
  else{
    psi_omega_mve <- as.numeric((mu_SigmaInv_One-psi_sigma[n])/One_SigmaInv_One) 
    MVE_weights[,n] <- (SigmaInv_mu-psi_omega_mve*SigmaInv_One)/psi_sigma[n]
    MVE_expected_return[n] <- MVE_weights[,n]%*%ERe
    MVE_expected_variance[n] <- MVE_weights[,n]%*%Cov_Re%*%MVE_weights[,n]
  }
}

################################################################################
## The ex-ante mean-variance frontier
################################################################################
#Mean expected returns and mean expected volatilities
Mean_expected_return <- apply(Portfolio_expected_returns,2:3,mean)
Mean_expected_volatility <- apply(sqrt(Portfolio_expected_variance),2:3,mean)

MVE_expected_volatility <- sqrt(MVE_expected_variance)

plot(Mean_expected_volatility[,1],Mean_expected_return[,1], type = "l", lwd = 2, col = "brown", xlim = c(0,10), ylim = c(-40,45), xlab = "Expected Volatility", 
     ylab = "Expected Excess Return")
lines(Mean_expected_volatility[,K_esg],Mean_expected_return[,K_esg], lwd = 2, col = "green")
lines(MVE_expected_volatility,MVE_expected_return, lwd = 2, col = "blue")
abline(0,0)
legend(0, 45, c("Low ESG", "High ESG", "MVE portfolio"), lty = 1, col = c("brown", "green", "blue"), cex = 0.7)

################################################################################
## Average realized returns vs volatility of realized returns
################################################################################
#With ESG target
Portfolio_return_average <- apply(Portfolio_returns,2:3,mean)
Portfolio_return_volatility <- apply(Portfolio_returns,2:3,sd)

#Without ESG target
MVE_return <- Re[(T_estimation+1):T_R,]%*%MVE_weights
MVE_return_average <- apply(MVE_return,2,mean)
MVE_return_volatility <- apply(MVE_return,2,sd)

#Plot ex-ante vs realized MVE frontier
plot(Mean_expected_volatility[,1],Mean_expected_return[,1], type = "p", lwd = 2, col = "brown", xlim = c(0,20), ylim = c(-40,45), xlab = "Expected Volatility", 
     ylab = "Expected Excess Return")
lines(Mean_expected_volatility[,K_esg],Mean_expected_return[,K_esg], type = "p", lwd = 2, col = "green")
lines(MVE_expected_volatility,MVE_expected_return, type = "p", lwd = 2, col = "blue")
lines(Portfolio_return_volatility[,1],Portfolio_return_average[,1], type = "l", lwd = 2, col = "brown")
lines(Portfolio_return_volatility[,K_esg],Portfolio_return_average[,K_esg], type = "l", lwd = 2, col = "green")
lines(MVE_return_volatility,MVE_return_average, type = "l", lwd = 2, col = "blue")
abline(0,0)
legend(-0.1, -15, c("Low ESG Ex-ante", "High ESG Ex-ante", "MVE portfolio Ex-ante","Low ESG Ex-post", "High ESG Ex-post", "MVE portfolio Ex-post"),
       lty = c(2,2,2,1,1,1), col = c("brown", "green", "blue","brown", "green", "blue"), cex = 1)

