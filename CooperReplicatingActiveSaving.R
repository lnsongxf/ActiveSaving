#File to replicate Cooper (2006) - calculating 'active savings'
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data

#source("Parker.R")
source("ActiveSavingLoadData.R")

# merge two timepoints together,
# by matching unique ID 

mergeCriteria = c('uniqueID', 'one', 'sex',
                  '1968IntNum', '1968PersonNum', 'primarySamplingUnit', 'stratification',
                  'longWeight84', 'longWeight89', 'longWeight94', 'longWeight99', 'longWeight01',
                  'longWeight03', 'longWeight05', 'longWeight07', 'longWeight09', 'longWeight11',
                  'longWeight13'
)

for(i in head(years, n=length(years)-1)){
  shortYeari = substr(i,3,4)
  shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
  assign(paste("x",shortYeari, shortYearj, sep=''), merge(eval(as.name(paste("z",shortYeari, sep=''))), 
                                                          eval(as.name(paste("z",shortYearj, sep=''))), by= mergeCriteria, all=FALSE))
}

rm(mergeCriteria, shortYeari, shortYearj, i)










# calculate active saving

# #if wtrMoved89 - I should do something if they moved - should I?
# x8489$activeSaving <- x8489$'farmBusiness89' - x8489$'farmBusiness84' + 
#   x8489$'checkingAccount89'- x8489$'checkingAccount84' + 
#   x8489$'othRealEstate89' - x8489$'othRealEstate84' + 
#   x8489$'stocks89' - x8489$'stocks84' + 
#   x8489$'vehicles89' - x8489$'vehicles84' + 
#   x8489$'othAssets89' - x8489$'othAssets84' - 
#   (x8489$'othDebt89' - x8489$'othDebt84') + 
#   x8489$'homeEquity89' - x8489$'homeEquity84'
# 
# x8994$activeSaving <- x8994$'farmBusiness94' - x8994$'farmBusiness89' + 
#   x8994$'checkingAccount94'- x8994$'checkingAccount89' + 
#   x8994$'othRealEstate94' - x8994$'othRealEstate89' + 
#   x8994$'stocks94' - x8994$'stocks89' + 
#   x8994$'vehicles94' - x8994$'vehicles89' + 
#   x8994$'othAssets94' - x8994$'othAssets89' - 
#   (x8994$'othDebt94' - x8994$'othDebt89') + 
#   x8994$'homeEquity94' - x8994$'homeEquity89'
# 
# x9499$activeSaving <- x9499$'farmBusiness99' - x9499$'farmBusiness94' + 
#   x9499$'checkingAccount99'- x9499$'checkingAccount94' + 
#   x9499$'othRealEstate99' - x9499$'othRealEstate94' + 
#   x9499$'stocks99' - x9499$'stocks94' + 
#   x9499$'vehicles99' - x9499$'vehicles94' + 
#   x9499$'othAssets99' - x9499$'othAssets94' - 
#   (x9499$'othDebt99' - x9499$'othDebt94') + 
#   x9499$'homeEquity99' - x9499$'homeEquity94'
# 
# x0507$activeSaving <- x0507$'farmBusiness07' - x0507$'farmBusiness05' + 
#   x0507$'checkingAccount07'- x0507$'checkingAccount05' + 
#   x0507$'othRealEstate07' - x0507$'othRealEstate05' + 
#   x0507$'stocks07' - x0507$'stocks05' + 
#   x0507$'vehicles07' - x0507$'vehicles05' + 
#   x0507$'othAssets07' - x0507$'othAssets05' - 
#   (x0507$'othDebt07' - x0507$'othDebt05') + 
#   x0507$'homeEquity07' - x0507$'homeEquity05'


# create an 'impActiveSaving' variable which is impWealth change year to year
# get rid of NA values (some observations don't have savings data)
x8489$impActiveSaving <-x8489$'impWealthWE89' - x8489$'impWealthWE84'
x8994$impActiveSaving <-x8994$'impWealthWE94' - x8994$'impWealthWE89'
x9499$impActiveSaving <-x9499$'impWealthWE99' - x9499$'impWealthWE94'
x9901$impActiveSaving <-x9901$'impWealthWE01' - x9901$'impWealthWE99'
x0103$impActiveSaving <-x0103$'impWealthWE03' - x0103$'impWealthWE01'
x0305$impActiveSaving <-x0305$'impWealthWE05' - x0305$'impWealthWE03'
x0507$impActiveSaving <-x0507$'impWealthWE07' - x0507$'impWealthWE05'
x0709$impActiveSaving <-x0709$'impWealthWE09' - x0709$'impWealthWE07'
x0911$impActiveSaving <-x0911$'impWealthWE11' - x0911$'impWealthWE09'
x1113$impActiveSaving <-x1113$'impWealthWE13' - x1113$'impWealthWE11'




rm(i,shortYeari,shortYearj)
# sort dissavers to savers, limit to matching uniqueIDs, get rid of top & bottom 1%, weight & plot
for(i in head(years, n=length(years)-2)){
  # three years
  shortYeari = substr(i,3,4)
  shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
  shortYeark = substr(years[which(years == i)[[1]]+2],3,4)
  yearString = paste(shortYeari, "_", shortYearj,"_", shortYeark,sep="")
  # merge criteria 
  mergeCriteria <- c('uniqueID', 'one', 'sex',
                    '1968IntNum', '1968PersonNum', 'primarySamplingUnit', 'stratification',
                    'longWeight84', 'longWeight89', 'longWeight94', 'longWeight99', 'longWeight01',
                    'longWeight03', 'longWeight05', 'longWeight07', 'longWeight09', 'longWeight11',
                    'longWeight13',
                    paste('intNum',shortYearj,sep=''), paste('farmBusiness',shortYearj,sep=''), paste('stocks',shortYearj,sep=''),
                    paste('othRealEstate',shortYearj,sep=''), paste('vehicles',shortYearj,sep=''), paste('mortgageDebt',shortYearj,sep=''),
                    paste('othAssets',shortYearj,sep=''), paste('wtrMoved',shortYearj,sep=''), paste('houseValue',shortYearj,sep=''),
                    paste('numInFam',shortYearj,sep=''), paste('famIncome',shortYearj,sep=''), paste('state',shortYearj,sep=''),
                    paste('checkingAccount',shortYearj,sep=''), paste('othDebt',shortYearj,sep=''), paste('homeEquity',shortYearj,sep=''),
                    paste('impWealthWE',shortYearj,sep=''), paste('sequenceNum',shortYearj,sep=''), paste('sequenceNum',shortYearj,sep=''),
                    paste('empStatus',shortYearj,sep=''), paste('age',shortYearj,sep=''), paste('hhRelStatus',shortYearj,sep=''),
                    paste('highestSchoolLev',shortYearj,sep=''), paste('hhHead',shortYearj,sep='')
  )
  # merge by unique id
  assign(paste("x", yearString, sep=''), 
         merge(eval(as.name(paste("x",shortYeari,shortYearj,sep=""))),eval(as.name(paste("x",shortYearj,shortYeark,sep=""))),by=mergeCriteria, all=FALSE))
  # survey design
  assign(paste('y',yearString, sep=''), svydesign(
    id=~primarySamplingUnit , 
    strata=~stratification , 
    data=eval(as.name(paste("x",yearString,sep=''))), 
    weights=~eval(as.name(paste('longWeight',shortYeark,sep=''))), 
    nest = TRUE ))
}
rm(shortYeari, shortYearj, shortYeark,i, yearString, mergeCriteria)

#plot active savings - absolute value, excluding top and bottom 1%
par(mfrow = c(3, 3))
for(i in head(years, n=length(years)-2)){
  # three years
  shortYeari = substr(i,3,4)
  shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
  shortYeark = substr(years[which(years == i)[[1]]+2],3,4)
  yearString = paste(shortYeari, "_", shortYearj,"_", shortYeark,sep="")
  #plot
  svyplot(impActiveSaving.x~impActiveSaving.y,
          subset(eval(as.name(paste("y",yearString,sep=''))),
                 eval(as.name(paste("hhHead",shortYeari, sep='')))==1 &
                   eval(as.name(paste("hhHead",shortYearj, sep='')))==1 & #hhHead same year to year
                   eval(as.name("impActiveSaving.x"))<quantile(eval(as.name("impActiveSaving.x")), 0.99) &
                   eval(as.name("impActiveSaving.x"))>quantile(eval(as.name("impActiveSaving.x")), 0.01) &
                   eval(as.name("impActiveSaving.y"))<quantile(eval(as.name("impActiveSaving.y")), 0.99) &
                   eval(as.name("impActiveSaving.y"))>quantile(eval(as.name("impActiveSaving.y")), 0.01)),
          style="transparent",
          pch=19,alpha=c(0,0.5), xlab=paste('Active saving ', shortYeari, '-', shortYearj,sep=''),
          ylab=paste('Active saving ', shortYearj, '-', shortYeark,sep=''),cex=0.3)
}

#plot active savings - logs, excluding top and bottom 1%
par(mfrow = c(3, 3))
for(i in head(years, n=length(years)-2)){
  # three years
  shortYeari = substr(i,3,4)
  shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
  shortYeark = substr(years[which(years == i)[[1]]+2],3,4)
  yearString = paste(shortYeari, "_", shortYearj,"_", shortYeark,sep="")
  #plot
  svyplot(log(impActiveSaving.x)~log(impActiveSaving.y),
          subset(eval(as.name(paste("y",yearString,sep=''))),
                 eval(as.name("impActiveSaving.x"))>0 & eval(as.name("impActiveSaving.y"))>0 & # so the logs don't get sad :(
                   eval(as.name(paste("hhHead",shortYeari, sep='')))==1 &
                   eval(as.name(paste("hhHead",shortYearj, sep='')))==1 & #hhHead same year to year
                   eval(as.name("impActiveSaving.x"))<quantile(eval(as.name("impActiveSaving.x")), 0.99) & # remove top and 
                   eval(as.name("impActiveSaving.x"))>quantile(eval(as.name("impActiveSaving.x")), 0.01) & # bottom 1%
                   eval(as.name("impActiveSaving.y"))<quantile(eval(as.name("impActiveSaving.y")), 0.99) &
                   eval(as.name("impActiveSaving.y"))>quantile(eval(as.name("impActiveSaving.y")), 0.01)),
          style="transparent",
          pch=19,alpha=c(0,0.5), xlab=paste('Log active saving ', shortYeari, '-', shortYearj,sep=''),
          ylab=paste('Active saving ', shortYearj, '-', shortYeark,sep=''),cex=0.3)
}



#plotting the wealth distribution - logs

#create a survey design on the unlinked data
for(i in head(years, n=length(years))){
  shortYeari = substr(i,3,4)
  #survey design
  assign(paste('wealth',shortYeari,sep=''), svydesign(id=~primarySamplingUnit, 
                                                                  strat=~stratification, weights=~eval(as.name(paste("longWeight",shortYeari,sep=""))), 
                                                                  data=eval(as.name(paste("z",shortYeari,sep=""))),
                                                                  nest=TRUE))
}

#plot the histograms
par(mfrow = c(3, 3))
for(i in head(years, n=length(years))){
  # two years
  shortYeari = substr(i,3,4)
  # histogram
  svyhist(~log(eval(as.name(paste("impWealthWE",shortYeari,sep='')))),
          subset(eval(as.name(paste("wealth",shortYeari,sep=''))),
                 eval(as.name(paste("impWealthWE",shortYeari,sep='')))>0,
                 eval(as.name(paste("impWealthWE",shortYeari,sep='')))<quantile(eval(as.name(paste("impWealthWE",shortYeari,sep=''))), 0.99) &
            eval(as.name(paste("impWealthWE",shortYeari,sep='')))>quantile(eval(as.name(paste("impWealthWE",shortYeari,sep=''))), 0.01)),
          main="", col="grey80", xlab=paste("Log wealth",shortYeari), breaks=200, cex=0.75)
}


#plot the histogram - absolute value excluding the top and bottom 1%


par(mfrow = c(3, 3))
for(i in head(years, n=length(years))){
  # two years
  shortYeari = substr(i,3,4)
  # histogram
  svyhist(~eval(as.name(paste("impWealthWE",shortYeari,sep=''))),
          subset(eval(as.name(paste("wealth",shortYeari,sep=''))),
                   eval(as.name(paste("impWealthWE",shortYeari,sep='')))<quantile(eval(as.name(paste("impWealthWE", shortYeari, sep=''))), 0.99) &
                   eval(as.name(paste("impWealthWE",shortYeari,sep='')))>quantile(eval(as.name(paste("impWealthWE", shortYeari, sep=''))), 0.01)),
          main="", col="grey80", xlab=paste("impWealthWE",shortYeari, 'ex top and bottom 1%'), breaks=100, cex=0.75)
 
}




# kernel regression of data without covariates

