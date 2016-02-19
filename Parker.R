# Related to Parker - The Consumption Function Re-Estimated

rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data

source("ActiveSavingLoadData.R")

# get rid of some junk
rm(x8489, x8994, x9499, x9901, x0103, x0305, x0507, x0709, x0911, x1113, years)


