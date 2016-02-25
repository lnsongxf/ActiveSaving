# Related to Parker - The Consumption Function Re-Estimated

rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data
library(data.table) # easy subsetting

source("ActiveSavingLoadData.R")

# get rid of some junk
#rm(x8489, x8994, x9499, x9901, x0103, x0305, x0507, x0709, x0911, x1113)

# gah, clean the individual years up - I should have done this a long time ago! Go back and sort this when the individual and family level files are merged!!
for(i in years){
  shortYear = substr(i,3,4)
  for(j in years){
    if(i==j){
      next
    }else{
      shortYearj = substr(j, 3,4)
      assign(paste("z", shortYear, sep=''), subset(eval(as.name(paste("z", shortYear, sep=''))), select = -c(eval(as.name(paste('intNum', shortYearj, sep=''))),
                                                                                                             eval(as.name(paste('sequenceNum', shortYearj, sep=''))),
                                                                                                             eval(as.name(paste('empStatus', shortYearj, sep=''))),
                                                                                                             eval(as.name(paste('age', shortYearj, sep=''))),
                                                                                                             eval(as.name(paste('hhRelStatus', shortYearj, sep=''))),
                                                                                                             eval(as.name(paste('longWeight', shortYearj, sep=''))))))
                                                                                                             
    }
  }
  
}


# drop any individuals with dumb age categories - NA, DK etc
z84 <- subset(z84, z84$age84>0 & z84$age84<126)
z89 <- subset(z89, z89$age89>0 & z89$age89<126)
z94 <- subset(z94, z94$age94>0 & z94$age94<126)
z99 <- subset(z99, z99$age99>0 & z99$age99<126)
z01 <- subset(z01, z01$age01>0 & z01$age01<126)
z03 <- subset(z03, z03$age03>0 & z03$age03<126)
z05 <- subset(z05, z05$age05>0 & z05$age05<126)
z07 <- subset(z07, z07$age07>0 & z07$age07<126)
z09 <- subset(z09, z09$age09>0 & z09$age09<126)
z11 <- subset(z11, z11$age11>0 & z11$age11<126)
z13 <- subset(z13, z13$age13>0 & z13$age13<126)

# create age categories - sort out your terrible naming conventions so you can turn this in to a loop!

z84$ageCategory <- ifelse(z84$age84==0, 0, ifelse(z84$age84<25, 1,ifelse(z84$age84<46,2,ifelse(z84$age84<56,3,ifelse(z84$age84<66,4,5)))))
z89$ageCategory <- ifelse(z89$age89==0, 0, ifelse(z89$age89<25, 1,ifelse(z89$age89<46,2,ifelse(z89$age89<56,3,ifelse(z89$age89<66,4,5)))))
z94$ageCategory <- ifelse(z94$age94==0, 0, ifelse(z94$age94<25, 1,ifelse(z94$age94<46,2,ifelse(z94$age94<56,3,ifelse(z94$age94<66,4,5)))))
z99$ageCategory <- ifelse(z99$age99==0, 0, ifelse(z99$age99<25, 1,ifelse(z99$age99<46,2,ifelse(z99$age99<56,3,ifelse(z99$age99<66,4,5)))))
z01$ageCategory <- ifelse(z01$age01==0, 0, ifelse(z01$age01<25, 1,ifelse(z01$age01<46,2,ifelse(z01$age01<56,3,ifelse(z01$age01<66,4,5)))))
z03$ageCategory <- ifelse(z03$age03==0, 0, ifelse(z03$age03<25, 1,ifelse(z03$age03<46,2,ifelse(z03$age03<56,3,ifelse(z03$age03<66,4,5)))))
z05$ageCategory <- ifelse(z05$age05==0, 0, ifelse(z05$age05<25, 1,ifelse(z05$age05<46,2,ifelse(z05$age05<56,3,ifelse(z05$age05<66,4,5)))))
z07$ageCategory <- ifelse(z07$age07==0, 0, ifelse(z07$age07<25, 1,ifelse(z07$age07<46,2,ifelse(z07$age07<56,3,ifelse(z07$age07<66,4,5)))))
z09$ageCategory <- ifelse(z09$age09==0, 0, ifelse(z09$age09<25, 1,ifelse(z09$age09<46,2,ifelse(z09$age09<56,3,ifelse(z09$age09<66,4,5)))))
z11$ageCategory <- ifelse(z11$age11==0, 0, ifelse(z11$age11<25, 1,ifelse(z11$age11<46,2,ifelse(z11$age11<56,3,ifelse(z11$age11<66,4,5)))))
z13$ageCategory <- ifelse(z13$age13==0, 0, ifelse(z13$age13<25, 1,ifelse(z13$age13<46,2,ifelse(z13$age13<56,3,ifelse(z13$age13<66,4,5)))))

# family weights - divide assets by number of OECD equivalent adults in houshold
# weight: 1st member = 1, 2nd member=0.67 (if adult), 3rd member=0.43 (if adult), 4+ = 0
#create an indicator for adult
z84$adult <- ifelse(z84$age84>16 & z84$age84<126, 1, 0)
z89$adult <- ifelse(z89$age89>16 & z89$age89<126, 1, 0)
z94$adult <- ifelse(z94$age94>16 & z94$age94<126, 1, 0)
z99$adult <- ifelse(z99$age99>16 & z99$age99<126, 1, 0)
z01$adult <- ifelse(z01$age01>16 & z01$age01<126, 1, 0)
z03$adult <- ifelse(z03$age03>16 & z03$age03<126, 1, 0)
z05$adult <- ifelse(z05$age05>16 & z05$age05<126, 1, 0)
z07$adult <- ifelse(z07$age07>16 & z07$age07<126, 1, 0)
z09$adult <- ifelse(z09$age09>16 & z09$age09<126, 1, 0)
z11$adult <- ifelse(z11$age11>16 & z11$age11<126, 1, 0)
z13$adult <- ifelse(z13$age13>16 & z13$age13<126, 1, 0)
# make a data table - much easier to summarise
for(i in years){
  shortYear <- substr(i,3,4)
  assign(paste("DT", shortYear, sep='' ), as.data.table(eval(as.name(paste("z",shortYear,sep='')))))
  assign(paste("temp", shortYear, sep=''), eval(as.name(paste("DT", shortYear, sep='' )))[,sum(adult), by=eval(paste("intNum", shortYear, sep=''))])
  #assign(transform(eval(as.name(paste("temp", shortYear, sep=''))), OECDweight=ifelse(temp13$V1==0, NA, ifelse(temp13$V1==1, 1,ifelse(temp13$V1==2, 1.67, ifelse(temp13$V1==3, 2.1,0))))))
}

#gahhhhh - create a column of OECD equivalent adult weights
temp84$OECDweight <- ifelse(temp84$V1==1, 1,ifelse(temp84$V1==2, 1.67, ifelse(temp84$V1==3, 2.1,2.1)))
temp89$OECDweight <- ifelse(temp89$V1==1, 1,ifelse(temp89$V1==2, 1.67, ifelse(temp89$V1==3, 2.1,2.1)))
temp94$OECDweight <- ifelse(temp94$V1==1, 1,ifelse(temp94$V1==2, 1.67, ifelse(temp94$V1==3, 2.1,2.1)))
temp99$OECDweight <- ifelse(temp99$V1==1, 1,ifelse(temp99$V1==2, 1.67, ifelse(temp99$V1==3, 2.1,2.1)))
temp01$OECDweight <- ifelse(temp01$V1==1, 1,ifelse(temp01$V1==2, 1.67, ifelse(temp01$V1==3, 2.1,2.1)))
temp03$OECDweight <- ifelse(temp03$V1==1, 1,ifelse(temp03$V1==2, 1.67, ifelse(temp03$V1==3, 2.1,2.1)))
temp05$OECDweight <- ifelse(temp05$V1==1, 1,ifelse(temp05$V1==2, 1.67, ifelse(temp05$V1==3, 2.1,2.1)))
temp07$OECDweight <- ifelse(temp07$V1==1, 1,ifelse(temp07$V1==2, 1.67, ifelse(temp07$V1==3, 2.1,2.1)))
temp09$OECDweight <- ifelse(temp09$V1==1, 1,ifelse(temp09$V1==2, 1.67, ifelse(temp09$V1==3, 2.1,2.1)))
temp11$OECDweight <- ifelse(temp11$V1==1, 1,ifelse(temp11$V1==2, 1.67, ifelse(temp11$V1==3, 2.1,2.1)))
temp13$OECDweight <- ifelse(temp13$V1==1, 1,ifelse(temp13$V1==2, 1.67, ifelse(temp13$V1==3, 2.1,2.1)))

# merge the original file and the weights file
for(i in years){
  shortYear <-substr(i,3,4)
  assign(paste("z",shortYear, sep=''), merge(eval(as.name(paste("z", shortYear, sep=''))), 
                                             eval(as.name(paste("temp",shortYear,sep=''))), 
                                             by = eval(paste('intNum', shortYear, sep=''))))
  assign(paste("z",shortYear, sep=''), rename(eval(as.name(paste("z", shortYear, sep=''))), 
                                             c('V1'='numAdults')))
  eval(call("rm",paste("DT",shortYear,sep="")))
  eval(call("rm",paste("temp",shortYear,sep="")))
}

# create weighted family size variable 
z84$weightedFamSize <- z84$numInFam*z84$OECDweight
z89$weightedFamSize <- z89$numInFam*z89$OECDweight
z94$weightedFamSize <- z94$numInFam*z94$OECDweight
z99$weightedFamSize <- z99$numInFam*z99$OECDweight
z01$weightedFamSize <- z01$numInFam*z01$OECDweight
z03$weightedFamSize <- z03$numInFam*z03$OECDweight
z05$weightedFamSize <- z05$numInFam*z05$OECDweight
z07$weightedFamSize <- z07$numInFam*z07$OECDweight
z09$weightedFamSize <- z09$numInFam*z09$OECDweight
z11$weightedFamSize <- z11$numInFam*z11$OECDweight
z13$weightedFamSize <- z13$numInFam*z13$OECDweight


# create a weighted asset variable
z84$weightedImpWealth <- z84$impWealthWE84/z84$weightedFamSize
z89$weightedImpWealth <- z89$impWealthWE89/z89$weightedFamSize
z94$weightedImpWealth <- z94$impWealthWE94/z94$weightedFamSize
z99$weightedImpWealth <- z99$impWealthWE99/z99$weightedFamSize
z01$weightedImpWealth <- z01$impWealthWE01/z01$weightedFamSize
z03$weightedImpWealth <- z03$impWealthWE03/z03$weightedFamSize
z05$weightedImpWealth <- z05$impWealthWE05/z05$weightedFamSize
z07$weightedImpWealth <- z07$impWealthWE07/z07$weightedFamSize
z09$weightedImpWealth <- z09$impWealthWE09/z09$weightedFamSize
z11$weightedImpWealth <- z11$impWealthWE11/z11$weightedFamSize
z13$weightedImpWealth <- z13$impWealthWE13/z13$weightedFamSize

# drop top and bottom 1%

z84 <- subset(z84, z84$weightedImpWealth<quantile(z84$weightedImpWealth, 0.99) & z84$weightedImpWealth>quantile(z84$weightedImpWealth, 0.01))
z89 <- subset(z89, z89$weightedImpWealth<quantile(z89$weightedImpWealth, 0.99) & z89$weightedImpWealth>quantile(z89$weightedImpWealth, 0.01))
z94 <- subset(z94, z94$weightedImpWealth<quantile(z94$weightedImpWealth, 0.99) & z94$weightedImpWealth>quantile(z94$weightedImpWealth, 0.01))
z99 <- subset(z99, z99$weightedImpWealth<quantile(z99$weightedImpWealth, 0.99) & z99$weightedImpWealth>quantile(z99$weightedImpWealth, 0.01))
z01 <- subset(z01, z01$weightedImpWealth<quantile(z01$weightedImpWealth, 0.99) & z01$weightedImpWealth>quantile(z01$weightedImpWealth, 0.01))
z03 <- subset(z03, z03$weightedImpWealth<quantile(z03$weightedImpWealth, 0.99) & z03$weightedImpWealth>quantile(z03$weightedImpWealth, 0.01))
z05 <- subset(z05, z05$weightedImpWealth<quantile(z05$weightedImpWealth, 0.99) & z05$weightedImpWealth>quantile(z05$weightedImpWealth, 0.01))
z07 <- subset(z07, z07$weightedImpWealth<quantile(z07$weightedImpWealth, 0.99) & z07$weightedImpWealth>quantile(z07$weightedImpWealth, 0.01))
z09 <- subset(z09, z09$weightedImpWealth<quantile(z09$weightedImpWealth, 0.99) & z09$weightedImpWealth>quantile(z09$weightedImpWealth, 0.01))
z11 <- subset(z11, z11$weightedImpWealth<quantile(z11$weightedImpWealth, 0.99) & z11$weightedImpWealth>quantile(z11$weightedImpWealth, 0.01))
z13 <- subset(z13, z13$weightedImpWealth<quantile(z13$weightedImpWealth, 0.99) & z13$weightedImpWealth>quantile(z13$weightedImpWealth, 0.01))
