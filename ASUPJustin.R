  # clear
  rm(list = ls()); gc()
  # set working directory.
  setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")
  
  library(survey)    # load survey package (analyzes complex design surveys)   
  library(plyr)      # load package needed to use 'join_all'
  library(tidyr)     # load package needed to go from wide to long data
  library(reshape2)  # load package needed to go from wide to long format
  library(np) # non parametric library
  library(xtable) # pretty latex tables
  library(data.table) # easy subsetting
  
  # set R to produce conservative standard errors instead of crashing
  # http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
  options( survey.lonely.psu = "adjust" )
  # this setting matches the MISSUNIT option in SUDAAN
  # load raw files
  #source("load.R")
  # construct an unbalanced panel of families (hhHead AND constituents)
  #source("ASUPconstructFamPan.R") 
  # take the data from wide to long format
  #source('ASUPwideToLong.R')
  
  #save(familyPanel, file='famPanel.R')
  load(file='famPanel.R')
  
  
  # adjust data for different year gaps, OECD equivalent adults, nominal to real
  #different year gaps
  familyPanel$activeSaving <- ifelse(familyPanel$year<=1999, familyPanel$activeSaving/5,familyPanel$activeSaving/2)
  familyPanel$PSIDas <- familyPanel$PSIDas/5
  #GDP deflator -- import from CSV dowloaded from: https://research.stlouisfed.org/fred2 (in 2013$)  
  GDPdeflator <- read.csv("GDPDeflator.csv",stringsAsFactors=FALSE)
  GDPdeflator <- GDPdeflator[11:21,1:2]
  GDPdeflator <- rename(GDPdeflator, c('FRED.Graph.Observations'='year', 'X'='GDPdeflator'))
  GDPdeflator$GDPdeflator <- as.numeric(GDPdeflator$GDPdeflator)
  GDPdeflator$GDPdeflator <- GDPdeflator$GDPdeflator/100
  # merge with famPan
  familyPanel<- merge(familyPanel, GDPdeflator, by='year')
  rm(GDPdeflator)
  # deflate
  familyPanel$impWealthWE <- round(familyPanel$impWealthWE/familyPanel$GDPdeflator,2)
  familyPanel$impWealthWOE <- round(familyPanel$impWealthWOE/familyPanel$GDPdeflator,2)
  familyPanel$activeSaving <- round(familyPanel$activeSaving/familyPanel$GDPdeflator,2)
  familyPanel$PSIDas <- round(familyPanel$PSIDas/familyPanel$GDPdeflator,2)
  familyPanel$famIncome <- round(familyPanel$famIncome/familyPanel$GDPdeflator,2)
  # OECD equivalent adults
  # family weights - number of OECD equivalent adults in houshold
  #“OECD equivalence scale”. This assigns a value of 1 to the first household member, of 0.7 to
  #each additional adult and of 0.5 to each child - http://www.oecd.org/eco/growth/OECD-Note-EquivalenceScales.pdf
  #create an indicator for adult
  familyPanel$adult <- ifelse(familyPanel$age>16 & familyPanel$age<126, 1, 0)
  familyPanel$adult[is.na(familyPanel$adult)] <- 0
  # individual weights
  familyPanel$indWeight <- ifelse(familyPanel$adult==1 & familyPanel$hhHead==1, 1, ifelse(familyPanel$adult==1, 0.7,0.5))
  
  #count the number of adults in each family & sum the individual weights
  # make a data table - much easier to summarise
  # number of adults
  DT <- as.data.table(familyPanel)
  # sum the individual weights
  indWeightSum <-DT[,sum(indWeight), by=c('intNum','year')]
  # merge the original file and the OECD weights files
  familyPanel <- merge(familyPanel, indWeightSum, by=c('intNum', 'year'), all=TRUE)
  familyPanel <- rename(familyPanel, c('V1'='OECDEquivAdults'))
  rm(DT, indWeightSum)
  
#   test <- subset(familyPanel, select=c('uniqueID','year','intNum','indWeight'))
#   years<-c('1984','1989','1994','1999','2001','2003','2005','2007','2009','2011','2013')
#   for(i in years){
#   assign(paste('test',i,sep=''),subset(test, test$year==i))
#   DT <- as.data.table(eval(as.name(paste('test',i,sep=''))))
#   indWeightSum <-DT[,sum(indWeight), by=c('intNum')]
#   assign(paste('test',i,sep=''), merge(eval(as.name(paste('test',i,sep=''))), indWeightSum, by='intNum'))
#   assign(paste('test',i,sep=''),rename(eval(as.name(paste('test',i,sep=''))), c('V1'=paste('OECDEquivAdults',i,sep=''))))
#   assign(paste('test',i,sep=''),subset(eval(as.name(paste('test',i,sep=''))),select=c('uniqueID',paste('OECDEquivAdults',i,sep=''))))
#   assign(paste('test',i,sep=''),na.omit(eval(as.name(paste('test',i,sep='')))))
#   }  
# 
#   test1 <- merge(test1984, test1989, by=c('uniqueID'),all=TRUE)
#   for(i in years[3:11]){
#   test1<-merge(test1,eval(as.name(paste('test',i,sep=''))),by=c('uniqueID'),all=TRUE)
#   }
#   
#   
#   fP_test <- melt(test1, id.vars=c("uniqueID"))
#   fP_test$year <- ifelse(fP_test$variable=='OECDEquivAdults1984',1984,
#                          ifelse(fP_test$variable=='OECDEquivAdults1989',1989,
#                                  ifelse(fP_test$variable=='OECDEquivAdults1994',1994,
#                                         ifelse(fP_test$variable=='OECDEquivAdults1999',1999,
#                                                ifelse(fP_test$variable=='OECDEquivAdults2001',2001,
#                                                       ifelse(fP_test$variable=='OECDEquivAdults2003',2003,
#                                                              ifelse(fP_test$variable=='OECDEquivAdults2005',2005,
#                                                                     ifelse(fP_test$variable=='OECDEquivAdults2007',2007,
#                                                                            ifelse(fP_test$variable=='OECDEquivAdults2009',2009,
#                                                                                   ifelse(fP_test$variable=='OECDEquivAdults2011',2011,2013))))))))))
#   
#   fP_test <- subset(fP_test,select=-c(variable))
#   fP_test<-rename(fP_test, c('value'='OECDEquivAdults'))
#   
#   testy <- merge(familyPanel,fP_test, by=c('uniqueID', 'year'), all=TRUE))
  
  # create weighted savings
  familyPanel$activeSaving <- round(familyPanel$activeSaving/familyPanel$OECDEquivAdults,2)
  familyPanel$PSIDas <- round(familyPanel$PSIDas/familyPanel$OECDEquivAdults,2) 
  
  # only keep necessary data 
  familyPanel <-subset(familyPanel,select=-c(indWeight, OECDEquivAdults, intNum, GDPdeflator, adult))

  
  # spit out some tables to latex with moments from the panel
  source("ASUPMomentTables.R")  #----need to change to adjusted data!!!!
# spit out plots of wealth, active saving
source("ASUPwealthASPlots.R")  #----need to change to adjusted data!!!!
