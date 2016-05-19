# clear
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")
# load raw files
source("load.R") # load the necessary PSID data
source("ASUPconstructFamPan.R") # construct an unbalanced panel

#source("ASUPMomentTables.R") # spit out some tables to latex with moments from the panel


# take the data from wide to long format
source('ASUPwideToLong.R')

###### wealth histograms ######
# subset the data - in 1984 there is no active saving data, there is no PSIDas data
# except for in 1989. When calculating moments, need to omit observations that have 
# missing activeSaving data other than in 1984

dataFP <- subset(familyPanel, select=-c(PSIDas))
dataFP <- dataFP[ (dataFP$year == 1984) | complete.cases(dataFP), ]
dataFP <- dataFP[ (dataFP$year != 1984) | complete.cases(dataFP[,1:11]), ]
dataFP <-subset(dataFP, select=-c(activeSaving))

#create a survey design 
familyPanelSurvey <- svydesign(id=~primarySamplingUnit,
                               strat=~stratification, 
                               weights=~longWeight,
                               data=dataFP,
                               nest=TRUE)
familyPanelSurveyAS <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification, 
                                 weights=~longWeight,
                                 data=na.omit(subset(familyPanel, select=-c(PSIDas))),
                                 nest=TRUE)

#plot the histograms - drop top and bottom 1%
years <- c('1984', '1989', '1994', '1999', '2001', '2003', '2005', '2007', '2009', '2011', '2013')
par(mfrow = c(3, 3))
for(i in head(years, n=length(years))){
  shortYeari = substr(i,3,4)
  # histogram
  svyhist(~eval(as.name("impWealthWOE")),
          subset(familyPanelSurvey,
                 familyPanelSurvey$variables$year==i &
                   eval(as.name("impWealthWOE"))<quantile(eval(as.name("impWealthWOE")), 0.99) &
                   eval(as.name("impWealthWOE"))>quantile(eval(as.name("impWealthWOE")), 0.01)),
          main="", col="grey80", xlab=paste("impWealthWOE",shortYeari, 'ex top and bottom 1%'), breaks=100, cex=0.75)
  
}

rm(dataFP, familyPanelSurvey, familyPanelSurveyAS)

###### active saving plots ######


#plot active savings - absolute value, excluding top and bottom 1%
par(mfrow = c(3, 3))
for(i in head(years, n=length(years)-2)){
  # three years
  yearj <- years[which(years == i)[[1]]+1]
  yeark <- years[which(years == i)[[1]]+2]
  shortYeari <- substr(i,3,4)
  shortYearj <- substr(yearj,3,4)
  shortYeark <- substr(yeark,3,4)
  yearString <- paste(shortYeari, "_", shortYearj,"_", shortYeark,sep="")
  # yeari-yearj vs yearj-yeark
  # yearj
  tempj <- subset(familyPanel, familyPanel$year==yearj)
  tempj <- subset(tempj, select=c(uniqueID, activeSaving, hhHead))
  tempj <- rename(tempj, c('activeSaving'=paste('activeSaving',shortYearj,sep=''),
                           'hhHead'=paste('hhHead',shortYearj,sep=''))) ##### problem here!!!!
  # yeark
  tempk <- subset(familyPanel, familyPanel$year==yeark)
  tempk <- subset(tempk, select=c(uniqueID, activeSaving, hhHead, stratification,  primarySamplingUnit,  longWeight))
  tempk <- rename(tempk, c('activeSaving'=paste('activeSaving',shortYeark,sep=''),
                           'hhHead'=paste('hhHead',shortYeark,sep='')))
  #merge by uniqueID
  tempjk <- merge(tempj, tempk, by='uniqueID')
  tempjk <- na.omit(tempjk)
  #hhHead same year to year 
  tempjk <- subset(tempjk,eval(as.name(paste("hhHead",shortYearj, sep='')))==1 &
    eval(as.name(paste("hhHead",shortYeark, sep='')))==1)
  #create a survey design 
  tempSurvey <- svydesign(id=~primarySamplingUnit,
                               strat=~stratification, 
                               weights=~longWeight,
                               data=tempjk,
                               nest=TRUE)
  #plot
  svyplot(eval(as.name(paste('activeSaving',shortYearj,sep='')))~eval(as.name(paste('activeSaving',shortYeark,sep=''))),
        subset(tempSurvey,
               eval(as.name(paste('activeSaving',shortYearj,sep='')))<quantile(eval(as.name(paste('activeSaving',shortYearj,sep=''))), 0.99) &  # get rid of top and bottom 1%
                 eval(as.name(paste('activeSaving',shortYearj,sep='')))>quantile(eval(as.name(paste('activeSaving',shortYearj,sep=''))), 0.01) &
                 eval(as.name(paste('activeSaving',shortYeark,sep='')))<quantile(eval(as.name(paste('activeSaving',shortYeark,sep=''))), 0.99) &
                 eval(as.name(paste('activeSaving',shortYeark,sep='')))>quantile(eval(as.name(paste('activeSaving',shortYeark,sep=''))), 0.01)),
        style="transparent",
        pch=19,alpha=c(0,0.5), xlab=paste('Active saving ', shortYeari, '-', shortYearj,sep=''),
        ylab=paste('Active saving ', shortYearj, '-', shortYeark,sep=''),cex=0.3)
}
#plot active savings - logs, excluding top and bottom 1%
par(mfrow = c(3, 3))
for(i in head(years, n=length(years)-2)){
  # three years
  yearj <- years[which(years == i)[[1]]+1]
  yeark <- years[which(years == i)[[1]]+2]
  shortYeari <- substr(i,3,4)
  shortYearj <- substr(yearj,3,4)
  shortYeark <- substr(yeark,3,4)
  yearString <- paste(shortYeari, "_", shortYearj,"_", shortYeark,sep="")
  # yeari-yearj vs yearj-yeark
  # yearj
  tempj <- subset(familyPanel, familyPanel$year==yearj)
  tempj <- subset(tempj, select=c(uniqueID, activeSaving, hhHead))
  tempj <- rename(tempj, c('activeSaving'=paste('activeSaving',shortYearj,sep=''),
                           'hhHead'=paste('hhHead',shortYearj,sep=''))) ##### problem here!!!!
  # yeark
  tempk <- subset(familyPanel, familyPanel$year==yeark)
  tempk <- subset(tempk, select=c(uniqueID, activeSaving, hhHead, stratification,  primarySamplingUnit,  longWeight))
  tempk <- rename(tempk, c('activeSaving'=paste('activeSaving',shortYeark,sep=''),
                           'hhHead'=paste('hhHead',shortYeark,sep='')))
  #merge by uniqueID
  tempjk <- merge(tempj, tempk, by='uniqueID')
  tempjk <- na.omit(tempjk)
  #hhHead same year to year 
  tempjk <- subset(tempjk,eval(as.name(paste("hhHead",shortYearj, sep='')))==1 &
                     eval(as.name(paste("hhHead",shortYeark, sep='')))==1)
  #create a survey design 
  tempSurvey <- svydesign(id=~primarySamplingUnit,
                          strat=~stratification, 
                          weights=~longWeight,
                          data=tempjk,
                          nest=TRUE)
  #plot
  svyplot(log(eval(as.name(paste('activeSaving',shortYearj,sep=''))))~log(eval(as.name(paste('activeSaving',shortYeark,sep='')))),
          subset(tempSurvey,
                 eval(as.name(paste('activeSaving',shortYearj,sep='')))<quantile(eval(as.name(paste('activeSaving',shortYearj,sep=''))), 0.99) &  # get rid of top and bottom 1%
                   eval(as.name(paste('activeSaving',shortYearj,sep='')))>quantile(eval(as.name(paste('activeSaving',shortYearj,sep=''))), 0.01) &
                   eval(as.name(paste('activeSaving',shortYeark,sep='')))<quantile(eval(as.name(paste('activeSaving',shortYeark,sep=''))), 0.99) &
                   eval(as.name(paste('activeSaving',shortYeark,sep='')))>quantile(eval(as.name(paste('activeSaving',shortYeark,sep=''))), 0.01)),
          style="transparent",
          pch=19,alpha=c(0,0.5), xlab=paste('Active saving ', shortYeari, '-', shortYearj,sep=''),
          ylab=paste('Active saving ', shortYearj, '-', shortYeark,sep=''),cex=0.3)
}
#get rid of detritus
rm(tempk, tempj, tempjk, i, shortYeari, shortYearj, shortYeark, tempSurvey, yearj, yeark, yearString)

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


