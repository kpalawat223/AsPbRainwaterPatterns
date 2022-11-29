#Kunal Palawat*, Dr. Mónica Ramírez-Andreotta*+^
#*University of Arizona, College of Agriculture and Life Sciences, Department of Environmental Science, 1177 E Fourth Street, Rm. 429, Tucson, AZ 857211
#+University of Arizona, Mel and Enid Zuckerman College of Public Health, 1295 N Martin Ave, Tucson, AZ 85724
#^Corresponding Author, mdramire@arizona.edu

#Date Created: November 25, 2022
#Date Last Edited: November 28, 2022

#Description: Code to analyze arsenic and lead concentrations in harvested rainwater compared to background rainwater concentrations.

#requires dataset "SupplementalFile1.xlsx"

#load necessary libraries
library(readxl)
library(tidyverse)
library(lme4)
library(performance)
library(effects)

#set working directory
setwd("")

#load in data
ph <- read_xlsx("SupplementalFile1.xlsx", sheet = "PH", col_names = TRUE)
nadp <- read_xlsx("SupplementalFile1.xlsx", sheet = "NADP", col_names = TRUE)

#attach NADP data to PH data
dat <- rbind(ph, nadp)

#As Final Model
As_model <- lmer(data = dat,
            log(arsenic) ~ community+period+season #natural log normalized data, fixed effects
            + (1|community:site), #site nested within community random effect
            REML = T) #restricted max likelihood method is better than max likelihood for estimates
summary(As_model) #assess model summary, including estimates
performance(As_model) #assess model assumptions

plot(AllEffects(As_model)) #visualize model effects

#Pb Final Model
Pb_model <- lmer(data = dat,
            log(lead) ~ community+season #natural log normalized data, fixed effects
            + (1|community:site), #site nested within community random effect
            REML = T) #restricted max likelihood method is better than max likelihood for estimates
summary(Pb_model) #assess model summary, including estimates
performance(Pb_model) #assess model assumptions

plot(AllEffects(Pb_model)) #visualize model effects


