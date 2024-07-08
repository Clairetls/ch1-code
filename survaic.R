setwd("/home1/p309444/models/AIC")

# install.packages("mgcv")
library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

survival1<-read.csv("survival_model.csv",stringsAsFactors = F)

survival1$age_year<-survival1$age
survival1$BirdID<-as.factor(survival1$BirdID)
survival1$SexEstimate<-as.factor(survival1$SexEstimate)
survival1$statbefore<-as.factor(survival1$statbefore)


DO <- function(data, variable, constant = "", ...) {
  
  # data: the dataset
  # variable: name of the response variable
  # constant: the part of the formula common to all models (start with "+ ...")
  # ...: extra arguments to be passed to gam()
  
  # Rename the response variable
  data <- data %>% rename(trait = variable)
  
  # Prepare formulas
  #do i have to specify the K???
  f1 <- "s(age_year, k=10) + SexEstimate"
  f3 <- "s(age_year, k=10)+SexEstimate+s(age_year, by = SexEstimate, k=10)"

  # Finish formulas
  forms <- list(f1, f3)
  forms <- map(forms, ~ as.formula(str_c("trait ~", .x, constant)))
  
  # Fit multiple models
  aics <- map_dfr(seq(forms), function(i) {
    
    message(paste0("Fitting model ", i, "..."))
    
    # Fit model
    mod <- gam(forms[[i]], data = data, method = "REML", ...)
    
    # Return model and goodness-of-fit
    tibble(i, AIC = AIC(mod), mod = list(mod))
    
  })
  
  # Extract best model
  id <- aics$AIC %>% { which(. == min(.)) }
  best <- aics$mod[id]
  
  # Remove models from the table
  aics <- aics %>% select(-mod)
  
  # Return AICs and best model
  return(list(best = best, AIC = aics))
  
}

#########

survaic<-DO(survival1, 'survival',constant='+lifespan+statbefore+avg_bug+
           s(occasionyear, bs="re")+s(birthyear, bs="re")',family=binomial(link='cloglog'))

saveRDS(survaic, "survaic.rds")


