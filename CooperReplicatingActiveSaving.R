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
# restrict sample to being responsive in each year
x8489 <-subset(x8489, intNum84>0 & intNum89>0)
x8994 <-subset(x8994, intNum89>0 & intNum94>0)
x9499 <-subset(x9499, intNum94>0 & intNum99>0)
x9901 <-subset(x9901, intNum99>0 & intNum01>0)
x0103 <-subset(x0103, intNum01>0 & intNum03>0)
x0305 <-subset(x0305, intNum03>0 & intNum05>0)
x0507 <-subset(x0507, intNum05>0 & intNum07>0)
x0709 <-subset(x0709, intNum07>0 & intNum09>0)
x0911 <-subset(x0911, intNum09>0 & intNum11>0)
x1113 <-subset(x1113, intNum11>0 & intNum13>0)


# # get rid of DK/refused/NA values - for the self-created wealth variable
# x8489 <-subset(x8489, farmBusiness84<9999996 & farmBusiness89<9999996 
#                & stocks84<9999996 & stocks89<9999996 
#                & vehicles84<999997 & vehicles89<999997 
#                & othAssets84<9999997 & othAssets89<9999997 
#                & othRealEstate84<9999997) & othRealEstate89<9999997)
# 
# x8994 <-subset(x8994, farmBusiness89<9999996 & farmBusiness94<9999996 
#                & stocks89<9999996 & stocks94<9999996 
#                & vehicles89<999997 & vehicles94<999997 
#                & othAssets89<9999997 & othAssets94<9999997
#                & othRealEstate89<9999997) & othRealEstate94<9999997)
# 
# x9499 <-subset(x9499, farmBusiness94<9999996 & farmBusiness99<9999996 
#                & stocks94<9999996 & stocks99<9999996 
#                & vehicles94<999997 & vehicles99<999997 
#                & othAssets94<9999997 & othAssets99<9999997
#                & othRealEstate94<9999997) & othRealEstate99<9999997)
# 
# x0507 <-subset(x0507, farmBusiness05<9999996 & farmBusiness07<9999996 
#                & stocks05<9999996 & stocks07<9999996 
#                & vehicles05<999997 & vehicles07<999997 
#                & othAssets05<9999997 & othAssets07<9999997
#                & othRealEstate05<9999997) & othRealEstate07<9999997)
# 
# x0709 <-subset(x0709, farmBusiness07<9999996 & farmBusiness09<9999996 
#                & stocks07<9999996 & stocks09<9999996 
#                & vehicles07<999997 & vehicles09<999997 
#                & othAssets07<9999997 & othAssets09<9999997
#                & othRealEstate07<9999997) & othRealEstate09<9999997)
# only keep those records whose head of household has not changed between waves
# keep each record whose hhHead89's uniqueID = hhHead84's uniqueID

# make a list of interview numbers in 1989 such that the household head
# is the same as the household head in 1984. By interview number means
# we retain all family records, not just those of the hh head.

#84-89
# familySameHead8489 <- c()
# for(intNum in unique(x8489$'intNum89')){
#   temp1 <- subset(x8489, x8489$'intNum89' ==  intNum & x8489$'hhHead89' == 1)
#   if (dim(temp1)[1] != 1){
#     next
#   } else{
#     if(temp1$'hhHead84'==1){
#     familySameHead8489 <- c(familySameHead8489, intNum)
#   }
#   }
# }
# rm(temp1, intNum)
# 
# #89-94
# familySameHead8994 <- c()
# for(intNum in unique(x8994$'intNum94')){
#   temp1 <- subset(x8994, x8994$'intNum94' ==  intNum & x8994$'hhHead94' == 1)
#   if (dim(temp1)[1] != 1){
#     next
#   } else{
#     if(temp1$'hhHead89'==1){
#       familySameHead8994 <- c(familySameHead8994, intNum)
#     }
#   }
# }
# rm(temp1, intNum)
# 
# #94-99
# familySameHead9499 <- c()
# for(intNum in unique(x9499$'intNum99')){
#    temp1 <- subset(x9499, x9499$'intNum99' ==  intNum & x9499$'hhHead99' == 1)
#    if (dim(temp1)[1] != 1){
#      next
#    } else{
#      if(temp1$'hhHead94'==1){
#        familySameHead9499 <- c(familySameHead9499, intNum)
#      }
#    }
#  }
#  rm(temp1, intNum)

#  #05-07
#  familySameHead0507 <- c()
#  for(intNum in unique(x0709$'intNum07')){
#     temp1 <- subset(x0507, x0507$'intNum07' ==  intNum & x0507$'hhHead07' == 1)
#     if (dim(temp1)[1] != 1){
#       next
#     } else{
#       if(temp1$'hhHead05'==1){
#         familySameHead0507 <- c(familySameHead0507, intNum)
#       }
#     }
#   }
#   rm(temp1, intNum)

# restrict sample to having the same household head in adjacent years
x8489 <- subset(x8489, x8489$hhHead84==1 & x8489$hhHead89==1 )
x8994 <- subset(x8994, x8994$hhHead89==1 & x8994$hhHead94==1 )
x9499 <- subset(x9499, x9499$hhHead94==1 & x9499$hhHead99==1 )
x9901 <- subset(x9901, x9901$hhHead99==1 & x9901$hhHead01==1 )
x0103 <- subset(x0103, x0103$hhHead01==1 & x0103$hhHead03==1 )
x0305 <- subset(x0305, x0305$hhHead03==1 & x0305$hhHead05==1 )
x0507 <- subset(x0507, x0507$hhHead05==1 & x0507$hhHead07==1 )
x0709 <- subset(x0709, x0709$hhHead07==1 & x0709$hhHead09==1 )
x0911 <- subset(x0911, x0911$hhHead09==1 & x0911$hhHead11==1 )
x1113 <- subset(x1113, x1113$hhHead11==1 & x1113$hhHead13==1 )




# save the familySameHead variables (because those for loops are slow as fuck - is there a better way??)
#save(familySameHead8489, familySameHead8994, familySameHead9499, familySameHead0507, file = "familySameHeads.RData")

# load( "familySameHeads.RData" )
# x8489 <-subset(x8489, x8489$'intNum89' %in% familySameHead8489)
# x8994 <-subset(x8994, x8994$'intNum94' %in% familySameHead8994)
# x9499 <-subset(x9499, x9499$'intNum99' %in% familySameHead9499)
# x0507 <-subset(x0507, x0507$'intNum07' %in% familySameHead0507)

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
x8489<-na.omit(x8489)
x8994$impActiveSaving <-x8994$'impWealthWE94' - x8994$'impWealthWE89'
x8994<-na.omit(x8994)
x9499$impActiveSaving <-x9499$'impWealthWE99' - x9499$'impWealthWE94'
x9499<-na.omit(x9499)
x9901$impActiveSaving <-x9901$'impWealthWE01' - x9901$'impWealthWE99'
x9901<-na.omit(x9901)
x0103$impActiveSaving <-x0103$'impWealthWE03' - x0103$'impWealthWE01'
x0103<-na.omit(x0103)
x0305$impActiveSaving <-x0305$'impWealthWE05' - x0305$'impWealthWE03'
x0305<-na.omit(x0305)
x0507$impActiveSaving <-x0507$'impWealthWE07' - x0507$'impWealthWE05'
x0507<-na.omit(x0507)
x0709$impActiveSaving <-x0709$'impWealthWE09' - x0709$'impWealthWE07'
x0709<-na.omit(x0709)
x0911$impActiveSaving <-x0911$'impWealthWE11' - x0911$'impWealthWE09'
x0911<-na.omit(x0911)
x1113$impActiveSaving <-x1113$'impWealthWE13' - x1113$'impWealthWE11'
x1113<-na.omit(x1113)



# turn wide to long format for the survey weights
for(i in head(years, n=length(years)-1)){
    shortYeari = substr(i,3,4)
    shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
    assign(paste("x",shortYeari,shortYearj,sep=""), 
           gather(eval(as.name(paste("x",shortYeari,shortYearj,sep=""))),
                  weightYear,weight,eval(as.name(paste("longWeight",shortYeari,sep=""))):longWeight13))
    #assign(paste("x",shortYeari,shortYearj,sep=""),subset(eval(as.name(paste("x",shortYeari, shortYearj, sep=''))),
    #                                                      eval(as.name(paste("x",shortYeari, shortYearj, sep='')))$weightYear==paste('longWeight',shortYearj,sep='')))
}
rm(i,shortYeari,shortYearj)
# sort dissavers to savers, limit to matching uniqueIDs, get rid of top & bottom 1%, weight & plot
for(i in head(years, n=length(years)-2)){
  # three years
  shortYeari = substr(i,3,4)
  shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
  shortYeark = substr(years[which(years == i)[[1]]+2],3,4)
  yearString = paste(shortYeari, "_", shortYearj,"_", shortYeark,sep="")
  # merge by unique id
  assign(paste("x", yearString, sep=''), 
         merge(eval(as.name(paste("x",shortYeari,shortYearj,sep=""))),eval(as.name(paste("x",shortYearj,shortYeark,sep=""))),by='uniqueID', all=FALSE))
  # keep the variables needed
  assign(paste("saving",yearString,sep=""), 
         eval(as.name(paste("x",yearString,sep="")))[c('impActiveSaving.x', 'impActiveSaving.y', 'weight.y', 'weightYear.y', 'primarySamplingUnit.y', 'stratification.y')])
  # keep only those associated with final year(in group) weight
  assign(paste("saving",yearString,sep=""),
         subset(eval(as.name(paste("saving",yearString,sep=""))),eval(as.name(paste("saving",yearString,sep="")))$weightYear.y==paste('longWeight',shortYeark,sep='')))
  # get rid of top and bottom 1%
  assign(paste("saving",yearString,sep=""),subset(eval(as.name(paste("saving",yearString,sep=""))),
         eval(as.name("impActiveSaving.x"))<quantile(eval(as.name("impActiveSaving.x")), 0.99) &
           eval(as.name("impActiveSaving.x"))>quantile(eval(as.name("impActiveSaving.x")), 0.01) &
           eval(as.name("impActiveSaving.y"))<quantile(eval(as.name("impActiveSaving.y")), 0.99) &
           eval(as.name("impActiveSaving.y"))>quantile(eval(as.name("impActiveSaving.y")), 0.01)))
  # sort dissavers to savers
  #assign(paste("saving",yearString,"_sorted",sep=''), 
  #       eval(as.name(paste("saving",yearString,sep='')))[order(eval(as.name(paste("saving",yearString,sep="")))$impActiveSaving.x),])  
  #weight
  assign(paste('y',yearString, sep=''), svydesign(
    id=~primarySamplingUnit.y , 
    strata=~stratification.y , 
    data=eval(as.name(paste("saving",yearString,sep=''))), 
    weights=~weight.y, 
    nest = TRUE ))
}
rm(shortYeari, shortYearj, shortYeark,i, yearString)

#plot
par(mfrow = c(3, 3))
for(i in head(years, n=length(years)-2)){
  # three years
  shortYeari = substr(i,3,4)
  shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
  shortYeark = substr(years[which(years == i)[[1]]+2],3,4)
  yearString = paste(shortYeari, "_", shortYearj,"_", shortYeark,sep="")
  #plot
#  svyplot(log(impActiveSaving.x)~log(impActiveSaving.y),design=eval(as.name(paste("y",yearString,sep=''))) , style="transparent",
#          pch=19,alpha=c(0,0.5), xlab=paste('Log active saving ', shortYeari, '-', shortYearj,sep=''),
#          ylab=paste('Log active saving ', shortYearj, '-', shortYeark,sep=''),cex=0.75)
  svyplot(impActiveSaving.x~impActiveSaving.y,design=eval(as.name(paste("y",yearString,sep=''))) , style="transparent",
          pch=19,alpha=c(0,0.5), xlab=paste('Active saving ', shortYeari, '-', shortYearj,sep=''),
          ylab=paste('Active saving ', shortYearj, '-', shortYeark,sep=''),cex=0.3)
}


#plotting the wealth distribution - logs

#create a survey design on the unlinked data
for(i in head(years, n=length(years))){
  shortYeari = substr(i,3,4)
  #subset to postive wealth holdings
  assign(paste("z",shortYeari,'pos',sep=''),subset(eval(as.name(paste("z",shortYeari,sep=''))),eval(as.name(paste("impWealthWE",shortYeari,sep='')))>0))
  #survey design
  assign(paste('wealth',shortYeari,sep=''), svydesign(id=~primarySamplingUnit, 
                                                                  strat=~stratification, weights=~eval(as.name(paste("longWeight",shortYeari,sep=""))), 
                                                                  data=eval(as.name(paste("z",shortYeari,"pos",sep=""))),
                                                                  nest=TRUE))
}

#plot the histogram
par(mfrow = c(3, 3))
for(i in head(years, n=length(years))){
  # two years
  shortYeari = substr(i,3,4)
  # histogram
  svyhist(~log(eval(as.name(paste("impWealthWE",shortYeari,sep='')))),
          subset(eval(as.name(paste("wealth",shortYeari,sep=''))),eval(as.name(paste("impWealthWE",shortYeari,sep='')))>0),
          main="", col="grey80", xlab=paste("Log wealth",shortYeari), breaks=200, cex=0.75)
  #lines(svysmooth(~log(eval(as.name(paste("impWealthWE",shortYeari,sep='')))), bandwidth=log(10), eval(as.name(paste("wealth",shortYeari,sep='')))),lwd=2)
}


#plot the histogram - absolute value excluding the top and bottom 1%

#create a survey design on the unlinked data
for(i in head(years, n=length(years))){
  shortYeari = substr(i,3,4)
  #subset to non NA
  assign(paste("z",shortYeari,'NA',sep=''),na.omit(eval(as.name(paste("z",shortYeari, sep='')))))
  #survey design
  assign(paste('wealth',shortYeari,sep=''), svydesign(id=~primarySamplingUnit, 
                                                      strat=~stratification, weights=~eval(as.name(paste("longWeight",shortYeari,sep=""))), 
                                                      data=eval(as.name(paste("z",shortYeari,"NA",sep=""))),
                                                      nest=TRUE))
}
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

