# Baseline immunity

# Required packages 
library(tidyr)
library(dplyr)
library(epimixr)
library(here)
library(readxl)

# Functions 
CI_upper <- function(p,n){
  CI_u<- p+1.96 * sqrt((p*(1-p)/n))
  return(CI_u) 
}# 95% confidence interval upper limit

CI_lower <- function(p,n){
  CI_u<- p-1.96 * sqrt((p*(1-p)/n))
  return(CI_u)
} # 95% confidence interval lower limit



# Baseline immunity 
# From Isa paper (AD = Adamawa)
Age <- c("0-4", "5-9", "10-14", "Total")
vacc_prot_n <-c(47, 57, 41, 145)
vacc_prot_prop <-c(0.796, 0.633, 0.594, 0.665)
vacc_unprot_n <- c(12, 33, 28, 73)
vacc_unprot_prop <- c(0.204, 0.367, 0.406, 0.335)
unvacc_prot_n <- c(50, 9, 23, 82)
unvacc_prot_prop <- c(0.595, 0.333, 0.59, 0.547)
unvacc_unprot_n <- c(34, 18, 16, 68)
unvacc_unprot_prop <- c(0.405, 0.667, 0.41, 0.453)
AD <- data.frame(Age, vacc_prot_n, vacc_prot_prop, vacc_unprot_n, vacc_unprot_prop, unvacc_prot_n, unvacc_prot_prop, unvacc_unprot_n, unvacc_unprot_prop)
AD$n <- vacc_prot_n + vacc_unprot_n + unvacc_prot_n + unvacc_unprot_n
AD$total_prot <- vacc_prot_n+ unvacc_prot_n
AD$immunity <- AD$total_prot/AD$n
AD$immunity <- round(AD$immunity, digits = 2)
AD$lower_CI <- mapply(CI_lower, AD$immunity, AD$n)
AD$lower_CI <- round(AD$lower_CI, digits = 2)
AD$upper_CI <- mapply(CI_upper, AD$immunity, AD$n)
AD$upper_CI <- round(AD$upper_CI, digits = 2)
saveRDS(AD, file = "Data/AD_immunity")

# From Abdulfatai paper (SO = Sokoto, JI = Jigawa, KA = Kaduna)
Age2 <- c("<1", "1-5", "6-10", "11-15")
n2 <- c(80, 201, 102, 67)
So_immunity <- c(0.5, 0.97, 0.93, 0.82)
JI_immunity <- c(0.5, 0.97, 0.93, 0.82)
KA_immunity <- c(0.5, 0.97, 0.93, 0.82)
SO_JI_KA <- data.frame(Age2, n2, So_immunity, JI_immunity, KA_immunity)
SO_JI_KA$lower_CI <- mapply(CI_lower, SO_JI_KA$So_immunity, SO_JI_KA$n2)
SO_JI_KA$lower_CI <- round(SO_JI_KA$lower_CI, digits = 2)
SO_JI_KA$upper_CI <- mapply(CI_upper, SO_JI_KA$So_immunity, SO_JI_KA$n2)
SO_JI_KA$upper_CI <- round(SO_JI_KA$upper_CI, digits = 2)
saveRDS(SO_JI_KA, file = "Data/SO_JI_KA_immunity")

# Adamawa

# Baseline immunity 
AD_im<- AD[, "immunity"]
names(AD_im) <- c("0", "5", "10")
AD_im <- AD_im[c(2,3)]
# Removed value goes in as maternal immunity (?)
