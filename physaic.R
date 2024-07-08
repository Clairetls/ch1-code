setwd("/home1/p309444/models/AIC")

library(mgcv)
library(chron)
library(lubridate)
library(lme4)
library(tidyverse)

physio<-read.csv("physio_28_5.csv",stringsAsFactors = F)

physio$Observer<-as.factor(physio$Observer)
physio$BirdID<-as.factor(physio$BirdID)
physio$SexEstimate<-as.factor(physio$SexEstimate)
physio$newstat<-as.factor(physio$newstat)


DO <- function(data, variable, constant = "", ...) {
  
  # data: the dataset
  # variable: name of the response variable
  # constant: the part of the formula common to all models (start with "+ ...")
  # ...: extra arguments to be passed to gam()
  
  # Rename the response variable
  data <- data %>% rename(trait = variable)
  
  # Prepare formulas
  #do i have to specify the K???
  f1 <- "s(age_year, k=10) + SexEstimate + s(BirdID, bs='re')"
  f2 <- "s(age_year,k=10) + SexEstimate + s(age_year,BirdID, bs='re')"
  f3 <- "s(age_year, k=10)+SexEstimate+s(age_year, by = SexEstimate, k=10) + s(BirdID, bs='re')"
  f4 <- "s(age_year, k=10)+SexEstimate+s(age_year, by = SexEstimate, k=10) + s(age_year,BirdID, bs='re')"
  
  # Finish formulas
  forms <- list(f1, f2, f3, f4)
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

#################################

bmaic <- physio %>% DO("BodyMass_z", constant = "+newlifespan+ RightTarsus_z+
                       newstat+CatchTime_mins+avg_invert+s(birthyear, bs='re')+
                       s(Observer, bs='re')", family = gaussian)

saveRDS(bmaic, "bmaic.rds")

