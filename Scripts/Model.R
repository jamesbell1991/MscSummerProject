# Running models 

# Packages 
library(epimixr)
library(tidyverse)
library(purrr)


# Import data 
MCV1_cov <- readRDS("Data/MCV1_coverage")
MCV1_cov <- MCV1_cov %>% 
  pivot_longer(!c("State", "LGA")) 
MCV1_cov$value <- MCV1_cov$value / 100
MCV1_cov <- MCV1_cov %>% pivot_wider(names_from = name, values_from = value)

MCV2_cov <- readRDS("Data/MCV2_coverage")
MCV2_cov <- MCV2_cov %>% 
  pivot_longer(!c("State", "LGA")) 
MCV2_cov$value <- MCV2_cov$value / 100
MCV2_cov <- MCV2_cov %>% pivot_wider(names_from = name, values_from = value)

AD_immunity<- readRDS("Data/AD_immunity")
AD_immunity <- AD_immunity %>%
  filter(Age != "Total") %>% 
  select(c("immunity")) %>% 
  mutate(months = c(0,60,120)) %>% 
  relocate(months)
AD_imm_1 <- deframe(AD_immunity)
# months corresponding to lowest band

SO_JI_KA_immunity <- readRDS("Data/SO_JI_KA_immunity")
SO_JI_KA_immunity <- SO_JI_KA_immunity %>% # months corresponding to lowest band
  mutate(months = c(0,12,72,132)) %>% 
  select(c("months", "So_immunity"))
SO_JI_KA_imm_1 <- deframe(SO_JI_KA_immunity)


# Coverage matrices
cov_matrix <- function(x){
  x1 <- MCV1_cov %>% 
    filter(LGA == x) %>% 
    select(!c("State", "LGA"))
  
  x2 <- MCV2_cov %>% 
    filter(LGA == x) %>% 
    select(!c("State", "LGA"))
  
  matrix <- as.matrix(rbind(x1,x2))
  colnames(matrix) <- as.character((seq(1,23)))
  
  assign(quo_name(enquo(x)), matrix, envir=.GlobalEnv)
}

LGA <- unique(MCV1_cov$LGA)

purrr::map(LGA, cov_matrix)





# Testing -----------------------------------------------------------------

maternal_immunity <- 0.68

test <- project_immunity(baseline.immunity = AD_imm_1,
                 baseline.year =9,
                 year =10,
                 coverage = Demsa,
                 schedule = c(9,15),
                 maternal.immunity = maternal_immunity,
                 efficacy= 0.91)



