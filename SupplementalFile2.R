#Kunal Palawat*, Dr. Mónica Ramírez-Andreotta*+^
#*University of Arizona, College of Agriculture and Life Sciences, Department of Environmental Science, 1177 E Fourth Street, Rm. 429, Tucson, AZ 857211
#+University of Arizona, Mel and Enid Zuckerman College of Public Health, 1295 N Martin Ave, Tucson, AZ 85724
#^Corresponding Author, mdramire@arizona.edu

#Date Created: September 14th, 2020
#Date Updated: March 31st, 2023
#Description: Code to perform MLOD protocol for Project Harvest Samples

#requires dataset "SupplementalFile1.xlsx"

#load libraries
library(readxl)
library(tidyverse)

#set working directory
setwd("")

#load data and unique MLODs
ph <- read_xlsx("SupplementalFile1.xlsx", sheet = "PH", col_names = TRUE)
nadp <- read_xlsx("SupplementalFile1.xlsx", sheet = "NADP", col_names = TRUE)
mlod <- read_xlsx("SupplementalFile1.xlsx", sheet = "MLOD", col_names = TRUE)

#attach NADP data to PH data
iw <- rbind(ph, nadp)

#remove already corrected data for ease of the following tutorial
iw <- subset(iw, select = -c(arsenic_corrected, lead_corrected))

#put mlod data into long-form for ease of calculations
mlod.long <- pivot_longer(data=mlod,
                          cols = c("arsenic","lead"),
                          names_to = "analyte",
                          values_to = "mlod_value")

#make data long-form for ease of calculations. here, each sample and analyte is given its own row and all concentrations are stored in the column titled, "value"
iw.long <- pivot_longer(data=iw,
                        cols = c("arsenic","lead"),
                        values_to = "value",
                        names_to = "analyte")

#join concentration data with mlod data for mlod analysis
iw.raw.long <- full_join(iw.long, mlod.long[,c("mlod_name", "analyte", "mlod_value")], by = c("mlod_name", "analyte"))

#copy joined data frame into new object
iw.corrected.long <- iw.raw.long

#if statement to assess whether a value is below the specified mlod. if it is below, substitute concentrations for the mlod value/sqrt(2). The function puts substitution values to significant figures and four for non-substituted values. Unfortunately trailing zeroes are dropped with this code, with no known solution.
iw.corrected.long$sample_value_corrected <- ifelse(iw.corrected.long$value<=as.numeric(iw.corrected.long$mlod_value),
                                                        formatC(signif(((as.numeric(iw.corrected.long$mlod_value)/sqrt(2))),digits=2), digits=2,format="fg", flag="#"),
                                                        formatC(signif((iw.corrected.long$value),digits=4), digits=4,format="fg", flag="#"))

#remove unnecessary columns from data frame 
iw.corrected.long <- subset(iw.corrected.long, select = -c(ID, value, mlod_value))

iw.corrected.long[iw.corrected.long$analyte == "arsenic",]$analyte <- "arsenic_corrected"
iw.corrected.long[iw.corrected.long$analyte == "lead",]$analyte <- "lead_corrected"

#make data wide-form for printing to csv
iw.corrected.wide <- pivot_wider(data=iw.corrected.long,
                                 names_from = "analyte",
                                 values_from = "sample_value_corrected"
                                 #,values_fn = length #to see where duplicates are
)

#print
write.csv(iw.corrected.wide, "data_corrected.csv")