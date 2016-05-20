# clear
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")
# load raw files
source("load.R")
# construct an unbalanced panel of families (hhHead AND constituents)
source("ASUPconstructFamPan.R") 
# take the data from wide to long format
source('ASUPwideToLong.R')


# adjust data for different year gaps, OECD equivalent adults,
familyPanel$yearlyActiveSaving <- ifelse(familyPanel$year<=1999, familyPanel$activeSaving/5,familyPanel$activeSaving/2)


# spit out some tables to latex with moments from the panel
source("ASUPMomentTables.R") 
# spit out plots of wealth, active saving
source("ASUPwealthASPlots.R")
