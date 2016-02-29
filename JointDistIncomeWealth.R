# Related to Parker - The Consumption Function Re-Estimated

rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data
library(data.table) # easy subsetting

source("ActiveSavingLoadData.R")





# family weights - number of OECD equivalent adults in houshold
#“OECD equivalence scale”. This assigns a value of 1 to the first household member, of 0.7 to
#each additional adult and of 0.5 to each child - http://www.oecd.org/eco/growth/OECD-Note-EquivalenceScales.pdf
#create an indicator for adult
z84$adult <- ifelse(z84$age84>18 & z84$age84<126, 1, 0)
z89$adult <- ifelse(z89$age89>18 & z89$age89<126, 1, 0)
z94$adult <- ifelse(z94$age94>18 & z94$age94<126, 1, 0)
z99$adult <- ifelse(z99$age99>18 & z99$age99<126, 1, 0)
z01$adult <- ifelse(z01$age01>18 & z01$age01<126, 1, 0)
z03$adult <- ifelse(z03$age03>18 & z03$age03<126, 1, 0)
z05$adult <- ifelse(z05$age05>18 & z05$age05<126, 1, 0)
z07$adult <- ifelse(z07$age07>18 & z07$age07<126, 1, 0)
z09$adult <- ifelse(z09$age09>18 & z09$age09<126, 1, 0)
z11$adult <- ifelse(z11$age11>18 & z11$age11<126, 1, 0)
z13$adult <- ifelse(z13$age13>18 & z13$age13<126, 1, 0)

# individual weights

z84$indWeight <- ifelse(z84$adult==1 & z84$hhHead84==1, 1, ifelse(z84$adult==1, 0.7,0.5))
z89$indWeight <- ifelse(z89$adult==1 & z89$hhHead89==1, 1, ifelse(z89$adult==1, 0.7,0.5))
z94$indWeight <- ifelse(z94$adult==1 & z94$hhHead94==1, 1, ifelse(z94$adult==1, 0.7,0.5))
z99$indWeight <- ifelse(z99$adult==1 & z99$hhHead99==1, 1, ifelse(z99$adult==1, 0.7,0.5))
z01$indWeight <- ifelse(z01$adult==1 & z01$hhHead01==1, 1, ifelse(z01$adult==1, 0.7,0.5))
z03$indWeight <- ifelse(z03$adult==1 & z03$hhHead03==1, 1, ifelse(z03$adult==1, 0.7,0.5))
z05$indWeight <- ifelse(z05$adult==1 & z05$hhHead05==1, 1, ifelse(z05$adult==1, 0.7,0.5))
z07$indWeight <- ifelse(z07$adult==1 & z07$hhHead07==1, 1, ifelse(z07$adult==1, 0.7,0.5))
z09$indWeight <- ifelse(z09$adult==1 & z09$hhHead09==1, 1, ifelse(z09$adult==1, 0.7,0.5))
z11$indWeight <- ifelse(z11$adult==1 & z11$hhHead11==1, 1, ifelse(z11$adult==1, 0.7,0.5))
z13$indWeight <- ifelse(z13$adult==1 & z13$hhHead13==1, 1, ifelse(z13$adult==1, 0.7,0.5))


#count the number of adults in each family & sum the individual weights
# make a data table - much easier to summarise
for(i in years){
  shortYear <- substr(i,3,4)
  assign(paste("DT", shortYear, sep='' ), as.data.table(eval(as.name(paste("z",shortYear,sep='')))))
  # sum the individual weights
  assign(paste("temp", shortYear, sep=''),
         eval(as.name(paste("DT", shortYear, sep='' )))[,sum(indWeight), by=eval(paste("intNum", shortYear, sep=''))])
}



# merge the original file and the num adults & weights files
for(i in years){
  shortYear <-substr(i,3,4)
  assign(paste("z",shortYear, sep=''), merge(eval(as.name(paste("z", shortYear, sep=''))), 
                                             eval(as.name(paste("temp",shortYear,sep=''))), 
                                             by = eval(paste('intNum', shortYear, sep=''))))
  assign(paste("z",shortYear, sep=''), rename(eval(as.name(paste("z", shortYear, sep=''))), 
                                              c('V1'='OECDEquivAdults')))
  eval(call("rm",paste("DT",shortYear,sep="")))
  eval(call("rm",paste("temp",shortYear,sep="")))
}

# calculate adjusted family size
z84$adjFamNum <- z84$numInFam84*z84$OECDEquivAdults
z89$adjFamNum <- z89$numInFam89*z89$OECDEquivAdults
z94$adjFamNum <- z94$numInFam94*z94$OECDEquivAdults
z99$adjFamNum <- z99$numInFam99*z99$OECDEquivAdults
z01$adjFamNum <- z01$numInFam01*z01$OECDEquivAdults
z03$adjFamNum <- z03$numInFam03*z03$OECDEquivAdults
z05$adjFamNum <- z05$numInFam05*z05$OECDEquivAdults
z07$adjFamNum <- z07$numInFam07*z07$OECDEquivAdults
z09$adjFamNum <- z09$numInFam09*z09$OECDEquivAdults
z11$adjFamNum <- z11$numInFam11*z11$OECDEquivAdults
z13$adjFamNum <- z13$numInFam13*z13$OECDEquivAdults



# create weighted income+ wealth
z84$adjIncomeWealth <- (z84$famIncome84 + z84$impWealthWE84)/z84$adjFamNum
z84 <- na.omit(z84)
z89$adjIncomeWealth <- (z89$famIncome89 + z89$impWealthWE89)/z89$adjFamNum
z89 <- na.omit(z89)
z94$adjIncomeWealth <- (z94$famIncome94 + z94$impWealthWE94)/z94$adjFamNum
z94 <- na.omit(z94)
z99$adjIncomeWealth <- (z99$famIncome99 + z99$impWealthWE99)/z99$adjFamNum
z99 <- na.omit(z99)
z01$adjIncomeWealth <- (z01$famIncome01 + z01$impWealthWE01)/z01$adjFamNum
z01 <- na.omit(z01)
z03$adjIncomeWealth <- (z03$famIncome03 + z03$impWealthWE03)/z03$adjFamNum
z03 <- na.omit(z03)
z05$adjIncomeWealth <- (z05$famIncome05 + z05$impWealthWE05)/z05$adjFamNum
z05 <- na.omit(z05)
z07$adjIncomeWealth <- (z07$famIncome07 + z07$impWealthWE07)/z07$adjFamNum
z07 <- na.omit(z07)
z09$adjIncomeWealth <- (z09$famIncome09 + z09$impWealthWE09)/z09$adjFamNum
z09 <- na.omit(z09)
z11$adjIncomeWealth <- (z11$famIncome11 + z11$impWealthWE11)/z11$adjFamNum
z11 <- na.omit(z11)
z13$adjIncomeWealth <- (z13$famIncome13 + z13$impWealthWE13)/z13$adjFamNum
z13 <- na.omit(z13)


#create a survey design on the unlinked data
for(i in head(years, n=length(years))){
  shortYeari = substr(i,3,4)
  #survey design
  assign(paste('incomeWealth',shortYeari,sep=''), svydesign(id=~primarySamplingUnit, 
                                                      strat=~stratification, weights=~eval(as.name(paste("longWeight",shortYeari,sep=""))), 
                                                      data=eval(as.name(paste("z",shortYeari,sep=""))),
                                                      nest=TRUE))
}

#plot the histograms - drop top and bottom 1%

par(mfrow = c(3, 3))
for(i in head(years, n=length(years))){
  # two years
  shortYeari = substr(i,3,4)
  # histogram
  svyhist(~eval(as.name("adjIncomeWealth")),
          subset(eval(as.name(paste("incomeWealth",shortYeari,sep=''))),
                 eval(as.name("adjIncomeWealth"))<quantile(eval(as.name("adjIncomeWealth")), 0.99) &
                   eval(as.name("adjIncomeWealth"))>quantile(eval(as.name("adjIncomeWealth")), 0.01)),
          main="", col="grey80", xlab=paste("adjIncomeWealth",shortYeari, 'ex top and bottom 1%'), breaks=100, cex=0.75)
  
}





