# Time path of active savings
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data
library(its)

source("ActiveSavingLoadData.R")

for(i in years){
  assign(paste("a", substr(i,3,4),sep=""), subset(eval(as.name(paste("z",substr(i,3,4),sep=""))), 
                                                      select = c(uniqueID, eval(as.name(paste("hhHead", substr(i,3,4),sep=''))), eval(as.name(paste("impWealthWE",substr(i,3,4),sep=""))))))
}

# join every year by unique ID - create one data set of same individuals over time
dfs <- list(
  a84,
  a89,
  a94,
  a99,
  a01,
  a03,
  a05,
  a07,
  a09,
  a11,
  a13
)

a_all <- join_all(dfs, by = 'uniqueID', type = "inner", match = "all")

# restrict to same household head across time series, and restrict to household head only (to get family saving)
a_all <-subset(a_all, hhHead84==1 & hhHead89==1 & hhHead94==1 & hhHead99==1 & hhHead01==1 & hhHead03==1 & hhHead05==1 & hhHead07==1 & hhHead09==1 & hhHead11==1 & hhHead13==1)
a_all <-subset(a_all, select=c(uniqueID, impWealthWE84, impWealthWE89, impWealthWE94, impWealthWE99, impWealthWE01, impWealthWE03, impWealthWE05, impWealthWE07, impWealthWE09, impWealthWE11, impWealthWE13))

# create time series
