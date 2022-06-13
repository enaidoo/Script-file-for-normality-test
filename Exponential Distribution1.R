rm(list = ls())

##required R packages
library(normtest)
library(nortest)
library(tseries)
library(fBasics)
library(timeDate)
library(timeSeries)
library(goftest)
library(kuiper.2samp)
library(robCompositions)
library(DescTools)


################################Exponential distribution
set.seed(1234)      #Random seed
N <- 30             #Sample size
sims <- 50000       #Number of replications
alpha<-0.05         #Significance level

## Container to store the p-values of the test

  TEST.SW <- rep(NA, sims) 
  TEST.LF <- rep(NA, sims) 
  TEST.AD <- rep(NA, sims) 
  TEST.JB <- rep(NA, sims) 
  TEST.DA <- rep(NA, sims) 
  TEST.SF <- rep(NA, sims) 
  
  for (i in 1:sims){
 
    Exp=rexp(N, 1) ### simulate from exponential distribution
    
    
    PVAL.SW<-shapiroTest(Exp)@test$p.value             ##Shapiro-Wilk's test for normality
    PVAL.LF<-lillieTest(Exp)@test$p.value              ##Lilliefors normality test
    PVAL.AD<-adTest(Exp)@test$p.value                   ## Aderson-Darling normality test
    PVAL.JB<-JarqueBeraTest(Exp, robust=TRUE,method = "mc",N=100)$p.value ## RobustJarque--Bera test for normality,
    PVAL.DA<-normalTest(Exp, method = "da")@test$p.value[1]  ##D'Agostino normality test.
    PVAL.SF<-sfTest(Exp)@test$p.value                  ##Shapiro-Francia normality test.
    
    TEST.SW[i] <- (PVAL.SW<=alpha)    ####Power
    TEST.LF[i] <- (PVAL.LF<=alpha)    ####Power
    TEST.AD[i] <- (PVAL.AD<=alpha)    ####Power
    TEST.JB[i] <- (PVAL.JB<=alpha)    ####Power
    TEST.DA[i] <- (PVAL.DA<=alpha)    ####Power
    TEST.SF[i] <- (PVAL.SF<=alpha)    ####Power
    
  Exp_Power.SW <- mean(TEST.SW)        ###Power Proportion
  Exp_Power.LF <- mean(TEST.LF)        ###Power Proportion
  Exp_Power.AD <- mean(TEST.AD)        ###Power Proportion
  Exp_Power.JB <- mean(TEST.JB)        ###Power Proportion
  Exp_Power.DA <- mean(TEST.DA)       ###Power Proportion
  Exp_Power.SF <- mean(TEST.SF)       ###Power Proportion

      }   ####End first loop
  

Exp_Power.AD   # results
Exp_Power.SW   # results
Exp_Power.LF   # results
Exp_Power.DA  # results
Exp_Power.SF  # results
Exp_Power.JB   # results

cbind(Exp_Power.AD, Exp_Power.SW, Exp_Power.LF, Exp_Power.DA, Exp_Power.SF, Exp_Power.JB)













mean(replicate(10000,shapiroTest(rnorm(100))@test$p.value)<=0.05)
