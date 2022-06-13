rm(list = ls())

###required R packages
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
library(distr)

################################ Exponential and Exponential distributions
set.seed(1234)       #random seed
N <- 30              #Sample size
sims <- 50000        #Number of replications
alpha<-0.05          #Significance level

## Container to store the p-values of the test
  TEST.SW <- rep(NA, sims) 
  TEST.LF <- rep(NA, sims) 
  TEST.AD <- rep(NA, sims) 
  TEST.JB <- rep(NA, sims) 
  TEST.DA <- rep(NA, sims) 
  TEST.SF <- rep(NA, sims) 
  
  for (i in 1:sims){
 
    Exp1 <-  Exp(1)                  # Simulate from exponential distribution
    Norm2 <- Norm(mean=0,sd=1)       ## Simulate from the normal distribution
    CONV <-  convpow(Exp1+Norm2,1)   #### Convolve Exponential and the Normal distributions
    Rgen=r(CONV)                     #random number generator
    Exp1.Norm2=Rgen(N)               ###simulate from the density      
    
    PVAL.SW<-shapiroTest(Exp1.Norm2)@test$p.value             ##Shapiro-Wilk's test for normality
    PVAL.LF<-lillieTest(Exp1.Norm2)@test$p.value              ##Lilliefors normality test
    PVAL.AD<-adTest(Exp1.Norm2)@test$p.value                   ## Aderson-Darling normality test
    PVAL.JB<-JarqueBeraTest(Exp1.Norm2, robust = TRUE,method = "mc", N=100)$p.value          ##Robust Jarque--Bera test for normality,
    PVAL.DA<-normalTest(Exp1.Norm2, method = "da")@test$p.value[1]  ##D'Agostino normality test.
    PVAL.SF<-sfTest(Exp1.Norm2)@test$p.value                        ##Shapiro-Francia normality test.
    
    TEST.SW[i] <- (PVAL.SW<=alpha)    ####Power
    TEST.LF[i] <- (PVAL.LF<=alpha)    ####Power
    TEST.AD[i] <- (PVAL.AD<=alpha)    ####Power
    TEST.JB[i] <- (PVAL.JB<=alpha)    ####Power
    TEST.DA[i] <- (PVAL.DA<=alpha)    ####Power
    TEST.SF[i] <- (PVAL.SF<=alpha)    ####Power
    
    Exp1.Norm2_Power.SW <- mean(TEST.SW)        ###Power Proportion
    Exp1.Norm2_Power.LF <- mean(TEST.LF)        ###Power Proportion
    Exp1.Norm2_Power.AD <- mean(TEST.AD)        ###Power Proportion
    Exp1.Norm2_Power.JB <- mean(TEST.JB)        ###Power Proportion
    Exp1.Norm2_Power.DA <- mean(TEST.DA)        ###Power Proportion
    Exp1.Norm2_Power.SF <- mean(TEST.SF)        ###Power Proportion

    
 }   ####End first loop
  
  Exp1.Norm2_Power.AD   # results
  Exp1.Norm2_Power.SW   # results
  Exp1.Norm2_Power.LF   # results
  Exp1.Norm2_Power.DA  # results
  Exp1.Norm2_Power.SF  # results
  Exp1.Norm2_Power.JB   # results

cbind(Exp1.Norm2_Power.AD, Exp1.Norm2_Power.SW, Exp1.Norm2_Power.LF, 
      Exp1.Norm2_Power.DA, Exp1.Norm2_Power.SF, Exp1.Norm2_Power.JB)






