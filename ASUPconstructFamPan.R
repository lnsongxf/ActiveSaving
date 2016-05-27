# This file construct an unbalanced panel from the PSID data
# need to run load.R first

# cleaning up the wealth data - this is a real mess

# get rid of NA/DK wealth data
years <- c('89','94','99','01','03','05','07','09','11','13')
# only a subset in '84
f84 <- subset(f84, realEstateEquity84<9999997 & farmBusiness84<9999997 &
                stocks84<9999997 & impWealthWOE84<9999997)
# create 13 real estate equity variable
f13$realEstateEquity13 <- f13$realEstateAssets13-f13$realEstateDebt13
f13 <-subset(f13, select=-c(realEstateAssets13, realEstateDebt13))
for(i in years){
  assign(paste("f",i,sep=''), subset(eval(as.name(paste("f",i,sep=''))),
                                     eval(as.symbol(paste("annuityAdd",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("realEstateBought",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("realEstateImprovement",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("investFarmBusiness",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("stocksPurchased",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("assetsRemoved",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("debtsAdded",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("impWealthWOE",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("realEstateEquity",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("farmBusiness",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("stocks",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("pensionsCashed",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("realEstateSold",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("farmBusinessSold",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("stocksSold",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("debtsRemoved",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("assetsAdded",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("inheritanceReceived",i,sep='')))<9999997
                                       ))
}
rm(i)
# PSID's active saving variable - drop NA
f89 <- subset(f89, f89$activeSaving <9999997)
# value of all other inheritances http://simba.isr.umich.edu/cb.aspx?vList=V17387
# in 1989 there is only one variable for 'all other inheritances
f89 <- subset(f89, f89$allOthInheritance89 <9999999)
# all other years - 2 variables: make my own allOtherInheritance variable
for(i in years[2:10]){
  assign(paste("f",i,sep=''), subset(eval(as.name(paste("f",i,sep=''))),
                                     eval(as.symbol(paste("secondInheritance",i,sep='')))<9999997 &
                                       eval(as.symbol(paste("thirdInheritance",i,sep='')))<9999997 ))
}
f94$allOthInheritance94<- f94$secondInheritance94 + f94$thirdInheritance94 
f99$allOthInheritance99<- f99$secondInheritance99 + f99$thirdInheritance99 
f01$allOthInheritance01<- f01$secondInheritance01 + f01$thirdInheritance01 
f03$allOthInheritance03<- f03$secondInheritance03 + f03$thirdInheritance03 
f05$allOthInheritance05<- f05$secondInheritance05 + f05$thirdInheritance05 
f07$allOthInheritance07<- f07$secondInheritance07 + f07$thirdInheritance07 
f09$allOthInheritance09<- f09$secondInheritance09 + f09$thirdInheritance09 
f11$allOthInheritance11<- f11$secondInheritance11 + f11$thirdInheritance11 
f13$allOthInheritance13<- f13$secondInheritance13 + f13$thirdInheritance13 


# create a unique ID variable (according to PSID instructions)
w$uniqueID <- (w$'1968IntNum'*1000) + w$'1968PersonNum'

w$age84[w$age84 == 999 |w$age84==0] <- NA
w$age89[w$age89 == 999 |w$age89==0] <- NA
w$age94[w$age94 == 999 |w$age94==0] <- NA
w$age99[w$age99 == 999 |w$age99==0] <- NA
w$age01[w$age01 == 999 |w$age01==0] <- NA
w$age03[w$age03 == 999 |w$age03==0] <- NA
w$age05[w$age05 == 999 |w$age05==0] <- NA
w$age07[w$age07 == 999 |w$age07==0] <- NA
w$age09[w$age09 == 999 |w$age09==0] <- NA
w$age11[w$age11 == 999 |w$age11==0] <- NA
w$age13[w$age13 == 999 |w$age13==0] <- NA

w$empStatus84[w$empStatus84 == 0 |w$empStatus84==9] <- NA
w$empStatus89[w$empStatus89 == 0 |w$empStatus89==9] <- NA
w$empStatus94[w$empStatus94 == 0 |w$empStatus94==9] <- NA
w$empStatus99[w$empStatus99 == 0 |w$empStatus99==9] <- NA
w$empStatus01[w$empStatus01 == 0 |w$empStatus01==9] <- NA
w$empStatus03[w$empStatus03 == 0 |w$empStatus03==9] <- NA
w$empStatus05[w$empStatus05 == 0 |w$empStatus05==9] <- NA
w$empStatus07[w$empStatus07 == 0 |w$empStatus07==9] <- NA
w$empStatus09[w$empStatus09 == 0 |w$empStatus09==9] <- NA
w$empStatus11[w$empStatus11 == 0 |w$empStatus11==9] <- NA
w$empStatus13[w$empStatus13 == 0 |w$empStatus13==9] <- NA

w$highestSchoolLev84[w$highestSchoolLev84 ==99 ] <- NA
w$highestSchoolLev89[w$highestSchoolLev89 ==99 ] <- NA
w$highestSchoolLev94[w$highestSchoolLev94 ==99 ] <- NA
w$highestSchoolLev99[w$highestSchoolLev99 ==99 ] <- NA
w$highestSchoolLev01[w$highestSchoolLev01 ==99 ] <- NA
w$highestSchoolLev03[w$highestSchoolLev03 ==99 ] <- NA
w$highestSchoolLev05[w$highestSchoolLev05 ==99 ] <- NA
w$highestSchoolLev07[w$highestSchoolLev07 ==99 ] <- NA
w$highestSchoolLev09[w$highestSchoolLev09 ==99 ] <- NA
w$highestSchoolLev11[w$highestSchoolLev11 ==99 ] <- NA
w$highestSchoolLev13[w$highestSchoolLev13 ==99 ] <- NA


intNum <- c()
years <- c('84','89','94','99','01','03','05','07','09','11','13')
for(i in years){
  intNum <- c(intNum, paste('intNum',i,sep=''))
}
j<-1
for(i in years){
  w <-merge(eval(as.name(paste("f",i,sep=''))),w,
            by=intNum[j], all=TRUE)
  j <-j+1
}
rm(intNum, i, j)

familyPanel <-w
rm(w, f84, f89, f94, f99, f01, f03, f05, f07, f09, f11, f13)

# active saving (http://simba.isr.umich.edu/cb.aspx?vList=V17610)

# 84-89
familyPanel$activeSaving89 <- familyPanel$annuityAdd89 +
  familyPanel$realEstateBought89 +
  familyPanel$realEstateImprovement89 +
  familyPanel$investFarmBusiness89 + 
  familyPanel$stocksPurchased89 +
  familyPanel$assetsRemoved89 +
  familyPanel$debtsAdded89 +
  familyPanel$impWealthWOE89 + 
  familyPanel$realEstateEquity84 + 
  familyPanel$farmBusiness84 + 
  familyPanel$stocks84 - 
  familyPanel$realEstateEquity89 -
  familyPanel$farmBusiness89 - 
  familyPanel$stocks89 - 
  familyPanel$pensionsCashed89 -
  familyPanel$realEstateSold89 -
  familyPanel$farmBusinessSold89 -
  familyPanel$stocksSold89 -
  familyPanel$debtsRemoved89 -
  familyPanel$assetsAdded89 -
  familyPanel$inheritanceReceived89 -
  familyPanel$allOthInheritance89 - 
  familyPanel$impWealthWOE84

#89-94
familyPanel$activeSaving94 <- familyPanel$annuityAdd94 +
  familyPanel$realEstateBought94 +
  familyPanel$realEstateImprovement94 +
  familyPanel$investFarmBusiness94 + 
  familyPanel$stocksPurchased94 +
  familyPanel$assetsRemoved94 +
  familyPanel$debtsAdded94 +
  familyPanel$impWealthWOE94 + 
  familyPanel$realEstateEquity89 + 
  familyPanel$farmBusiness89 + 
  familyPanel$stocks89 - 
  familyPanel$realEstateEquity94 -
  familyPanel$farmBusiness94 - 
  familyPanel$stocks94 - 
  familyPanel$pensionsCashed94 -
  familyPanel$realEstateSold94 -
  familyPanel$farmBusinessSold94 -
  familyPanel$stocksSold94 -
  familyPanel$debtsRemoved94 -
  familyPanel$assetsAdded94 -
  familyPanel$inheritanceReceived94 -
  familyPanel$allOthInheritance94 -
  familyPanel$impWealthWOE89

#94-99
familyPanel$activeSaving99 <- familyPanel$annuityAdd99 +
  familyPanel$realEstateBought99 +
  familyPanel$realEstateImprovement99 +
  familyPanel$investFarmBusiness99 + 
  familyPanel$stocksPurchased99 +
  familyPanel$assetsRemoved99 +
  familyPanel$debtsAdded99 +
  familyPanel$impWealthWOE99 + 
  familyPanel$realEstateEquity94 + 
  familyPanel$farmBusiness94 + 
  familyPanel$stocks94 - 
  familyPanel$realEstateEquity99 -
  familyPanel$farmBusiness99 - 
  familyPanel$stocks99 - 
  familyPanel$pensionsCashed99 -
  familyPanel$realEstateSold99 -
  familyPanel$farmBusinessSold99 -
  familyPanel$stocksSold99 -
  familyPanel$debtsRemoved99 -
  familyPanel$assetsAdded99 -
  familyPanel$inheritanceReceived99 -
  familyPanel$allOthInheritance99 -
  familyPanel$impWealthWOE94

#99-01
familyPanel$activeSaving01 <- familyPanel$annuityAdd01 +
  familyPanel$realEstateBought01 +
  familyPanel$realEstateImprovement01 +
  familyPanel$investFarmBusiness01 + 
  familyPanel$stocksPurchased01 +
  familyPanel$assetsRemoved01 +
  familyPanel$debtsAdded01 +
  familyPanel$impWealthWOE01 + 
  familyPanel$realEstateEquity99 + 
  familyPanel$farmBusiness99 + 
  familyPanel$stocks99 - 
  familyPanel$realEstateEquity01 -
  familyPanel$farmBusiness01 - 
  familyPanel$stocks01 - 
  familyPanel$pensionsCashed01 -
  familyPanel$realEstateSold01 -
  familyPanel$farmBusinessSold01 -
  familyPanel$stocksSold01 -
  familyPanel$debtsRemoved01 -
  familyPanel$assetsAdded01 -
  familyPanel$inheritanceReceived01 -
  familyPanel$allOthInheritance01 -
  familyPanel$impWealthWOE99

#01-03
familyPanel$activeSaving03 <- familyPanel$annuityAdd03 +
  familyPanel$realEstateBought03 +
  familyPanel$realEstateImprovement03 +
  familyPanel$investFarmBusiness03 + 
  familyPanel$stocksPurchased03 +
  familyPanel$assetsRemoved03 +
  familyPanel$debtsAdded03 +
  familyPanel$impWealthWOE03 + 
  familyPanel$realEstateEquity01 + 
  familyPanel$farmBusiness01 + 
  familyPanel$stocks01 - 
  familyPanel$realEstateEquity03 -
  familyPanel$farmBusiness03 - 
  familyPanel$stocks03 - 
  familyPanel$pensionsCashed03 -
  familyPanel$realEstateSold03 -
  familyPanel$farmBusinessSold03 -
  familyPanel$stocksSold03 -
  familyPanel$debtsRemoved03 -
  familyPanel$assetsAdded03 -
  familyPanel$inheritanceReceived03 -
  familyPanel$allOthInheritance03 -
  familyPanel$impWealthWOE01

#03-05
familyPanel$activeSaving05 <- familyPanel$annuityAdd05 +
  familyPanel$realEstateBought05 +
  familyPanel$realEstateImprovement05 +
  familyPanel$investFarmBusiness05 + 
  familyPanel$stocksPurchased05 +
  familyPanel$assetsRemoved05 +
  familyPanel$debtsAdded05 +
  familyPanel$impWealthWOE05 + 
  familyPanel$realEstateEquity03 + 
  familyPanel$farmBusiness03 + 
  familyPanel$stocks03 - 
  familyPanel$realEstateEquity05 -
  familyPanel$farmBusiness05 - 
  familyPanel$stocks05 - 
  familyPanel$pensionsCashed05 -
  familyPanel$realEstateSold05 -
  familyPanel$farmBusinessSold05 -
  familyPanel$stocksSold05 -
  familyPanel$debtsRemoved05 -
  familyPanel$assetsAdded05 -
  familyPanel$inheritanceReceived05 -
  familyPanel$allOthInheritance05 -
  familyPanel$impWealthWOE03

#05-07
familyPanel$activeSaving07 <- familyPanel$annuityAdd07 +
  familyPanel$realEstateBought07 +
  familyPanel$realEstateImprovement07 +
  familyPanel$investFarmBusiness07 + 
  familyPanel$stocksPurchased07 +
  familyPanel$assetsRemoved07 +
  familyPanel$debtsAdded07 +
  familyPanel$impWealthWOE07 + 
  familyPanel$realEstateEquity05 + 
  familyPanel$farmBusiness05 + 
  familyPanel$stocks05 - 
  familyPanel$realEstateEquity07 -
  familyPanel$farmBusiness07 - 
  familyPanel$stocks07 - 
  familyPanel$pensionsCashed07 -
  familyPanel$realEstateSold07 -
  familyPanel$farmBusinessSold07 -
  familyPanel$stocksSold07 -
  familyPanel$debtsRemoved07 -
  familyPanel$assetsAdded07 -
  familyPanel$inheritanceReceived07 -
  familyPanel$impWealthWOE05

#07-09
familyPanel$activeSaving09 <- familyPanel$annuityAdd09 +
  familyPanel$realEstateBought09 +
  familyPanel$realEstateImprovement09 +
  familyPanel$investFarmBusiness09 + 
  familyPanel$stocksPurchased09 +
  familyPanel$assetsRemoved09 +
  familyPanel$debtsAdded09 +
  familyPanel$impWealthWOE09 + 
  familyPanel$realEstateEquity07 + 
  familyPanel$farmBusiness07 + 
  familyPanel$stocks07 - 
  familyPanel$realEstateEquity09 -
  familyPanel$farmBusiness09 - 
  familyPanel$stocks09 - 
  familyPanel$pensionsCashed09 -
  familyPanel$realEstateSold09 -
  familyPanel$farmBusinessSold09 -
  familyPanel$stocksSold09 -
  familyPanel$debtsRemoved09 -
  familyPanel$assetsAdded09 -
  familyPanel$inheritanceReceived09 -
  familyPanel$allOthInheritance09 -
  familyPanel$impWealthWOE07

#09-11
familyPanel$activeSaving11 <- familyPanel$annuityAdd11 +
  familyPanel$realEstateBought11 +
  familyPanel$realEstateImprovement11 +
  familyPanel$investFarmBusiness11 + 
  familyPanel$stocksPurchased11 +
  familyPanel$assetsRemoved11 +
  familyPanel$debtsAdded11 +
  familyPanel$impWealthWOE11 + 
  familyPanel$realEstateEquity09 + 
  familyPanel$farmBusiness09 + 
  familyPanel$stocks09 - 
  familyPanel$realEstateEquity11 -
  familyPanel$farmBusiness11 - 
  familyPanel$stocks11 - 
  familyPanel$pensionsCashed11 -
  familyPanel$realEstateSold11 -
  familyPanel$farmBusinessSold11 -
  familyPanel$stocksSold11 -
  familyPanel$debtsRemoved11 -
  familyPanel$assetsAdded11 -
  familyPanel$inheritanceReceived11 -
  familyPanel$allOthInheritance11 -
  familyPanel$impWealthWOE09

#11-13
familyPanel$activeSaving13 <- familyPanel$annuityAdd13 +
  familyPanel$realEstateBought13 +
  familyPanel$realEstateImprovement13 +
  familyPanel$investFarmBusiness13 + 
  familyPanel$stocksPurchased13 +
  familyPanel$assetsRemoved13 +
  familyPanel$debtsAdded13 + 
  familyPanel$impWealthWOE13 + 
  familyPanel$realEstateEquity11 + 
  familyPanel$farmBusiness11 + 
  familyPanel$stocks11 - 
  familyPanel$realEstateEquity13 -
  familyPanel$farmBusiness13 - 
  familyPanel$stocks13 - 
  familyPanel$pensionsCashed13 -
  familyPanel$realEstateSold13 -
  familyPanel$farmBusinessSold13 -
  familyPanel$stocksSold13 -
  familyPanel$debtsRemoved13 -
  familyPanel$assetsAdded13 -
  familyPanel$inheritanceReceived13 -
  familyPanel$allOthInheritance13 -
  familyPanel$impWealthWOE11

# drop all of the surplus wealth data


familyPanel <- subset(familyPanel, select=c(uniqueID, stratification, primarySamplingUnit,
                                            longWeight84, longWeight89, longWeight94, longWeight99, longWeight01,longWeight03,
                                            longWeight05, longWeight07, longWeight09, longWeight11,longWeight13,
                                            activeSaving89, activeSaving94, activeSaving99, activeSaving01,activeSaving03,
                                            activeSaving05, activeSaving07, activeSaving09, activeSaving11,activeSaving13,
                                            age84, age89, age94, age99, age01,age03,
                                            age05, age07, age09, age11,age13,
                                            famIncome84, famIncome89, famIncome94, famIncome99, famIncome01,famIncome03,
                                            famIncome05, famIncome07, famIncome09, famIncome11, famIncome13,
                                            numInFam84, numInFam89, numInFam94, numInFam99, numInFam01, numInFam03,
                                            numInFam05, numInFam07, numInFam09, numInFam11, numInFam13,
                                            empStatus84, empStatus89, empStatus94, empStatus99, empStatus01, empStatus03,
                                            empStatus05, empStatus07, empStatus09, empStatus11, empStatus13,
                                            highestSchoolLev84, highestSchoolLev89, highestSchoolLev94, highestSchoolLev99, highestSchoolLev01, highestSchoolLev03,
                                            highestSchoolLev05, highestSchoolLev07, highestSchoolLev09, highestSchoolLev11, highestSchoolLev13,
                                            impWealthWOE84, impWealthWOE89, impWealthWOE94, impWealthWOE99, impWealthWOE01, impWealthWOE03,
                                            impWealthWOE05, impWealthWOE07, impWealthWOE09, impWealthWOE11, impWealthWOE13,
                                            sequenceNum84, sequenceNum89, sequenceNum94, sequenceNum99, sequenceNum01,
                                            sequenceNum03, sequenceNum05, sequenceNum07, sequenceNum09, sequenceNum11, sequenceNum13,
                                            hhRelStatus84, hhRelStatus89, hhRelStatus94, hhRelStatus94, hhRelStatus99, hhRelStatus01,
                                            hhRelStatus03, hhRelStatus05, hhRelStatus07, hhRelStatus09, hhRelStatus11, hhRelStatus13,
                                            intNum84,intNum89, intNum94, intNum99, intNum01, intNum01, intNum03, intNum05, intNum07,
                                            intNum09, intNum11, intNum13,
                                            impWealthWE84,impWealthWE89, impWealthWE94,impWealthWE99,impWealthWE01,impWealthWE03,
                                            impWealthWE05,impWealthWE07,impWealthWE09,impWealthWE11,impWealthWE13,
                                            activeSaving, allOthInheritance89
                                            
))

