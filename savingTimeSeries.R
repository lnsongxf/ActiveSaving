# Calculates and plots a time path of active savings for panel of same households with same head 1984-13
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data
library(its)       # irregular time series

source("ActiveSavingLoadData.R")

# get rid of some junk
rm(x8489, x8994, x9499, x9901, x0103, x0305, x0507, x0709, x0911, x1113)


# create a variable for each year containing the data we need
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


write.table(a_all, "a_all.txt", sep="\t")


# create time series
t <-c(01/01/1984, 01/01/1989) , 1994, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013)
dates(years)

require(graphics)

z <- ts(a_all,
        start = c(1984, 1), frequency = 1)
z <- window(z[,1], end = c(1984,13))
plot(z), type = "b")    # multiple
plot(z, plot.type = "single", lty = 1:3, col = 4:2)

