# clear
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")
# load raw files
source("load.R")

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
                                       eval(as.symbol(paste("inheritanceReceived",i,sep='')))<9999997))
}
rm(i)
# PSID's active saving variable - drop NA
f89 <- subset(f89, f89$activeSaving <9999997)

w$uniqueID <- (w$'1968IntNum'*1000) + w$'1968PersonNum'
w <- subset(w, (w$sequenceNum84<=20 & w$hhRelStatus84==10)|
                  (w$sequenceNum89<=20 & w$hhRelStatus89==10) |
                  (w$sequenceNum94<=20 & w$hhRelStatus94==10) |
                  (w$sequenceNum99<=20 & w$hhRelStatus99==10) |
                  (w$sequenceNum01<=20 & w$hhRelStatus01==10) |
                  (w$sequenceNum03<=20 & w$hhRelStatus03==10) |
                  (w$sequenceNum05<=20 & w$hhRelStatus05==10) |
                  (w$sequenceNum07<=20 & w$hhRelStatus07==10) |
                  (w$sequenceNum09<=20 & w$hhRelStatus09==10) |
                  (w$sequenceNum11<=20 & w$hhRelStatus11==10) |
                  (w$sequenceNum13<=20 & w$hhRelStatus13==10) )
  
 

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
                                            activeSaving
                                            
))

# wide to long format

# unique ID and stratification ##################################################################################################################################################################
fP_ID <- familyPanel[c('uniqueID','stratification','primarySamplingUnit' )]

# activeSaving ##################################################################################################################################################################
fP_activeSaving <- familyPanel[c('uniqueID',
                                 'activeSaving89',
                                 'activeSaving94',
                                 'activeSaving99',
                                 'activeSaving01',
                                 'activeSaving03',
                                 'activeSaving05',
                                 'activeSaving07',
                                 'activeSaving09',
                                 'activeSaving11',
                                 'activeSaving13' )]
# Specify id.vars: the variables to keep but not split apart on
fP_activeSaving <- melt(fP_activeSaving, id.vars=c("uniqueID"))
fP_activeSaving$year <- ifelse(fP_activeSaving$variable=='activeSaving89',1989,
                               ifelse(fP_activeSaving$variable=='activeSaving94',1994,
                                      ifelse(fP_activeSaving$variable=='activeSaving99',1999,
                                             ifelse(fP_activeSaving$variable=='activeSaving01',2001,
                                                    ifelse(fP_activeSaving$variable=='activeSaving03',2003,
                                                           ifelse(fP_activeSaving$variable=='activeSaving05',2005,
                                                                  ifelse(fP_activeSaving$variable=='activeSaving07',2007,
                                                                         ifelse(fP_activeSaving$variable=='activeSaving09',2009,
                                                                                ifelse(fP_activeSaving$variable=='activeSaving11',2011,2013)))))))))

fP_activeSaving <- rename(fP_activeSaving, c('value' = 'activeSaving'))
fP_activeSaving <-subset(fP_activeSaving,select=-c(variable))


# PSID active saving ##################################################################################################################################################################
fP_PSIDas<- familyPanel[c('uniqueID',
                                 'activeSaving' )]
# Specify id.vars: the variables to keep but not split apart on
fP_PSIDas$year <- 1989
fP_PSIDas <- rename(fP_PSIDas, c('activeSaving' = 'PSIDas'))
# long weights ##################################################################################################################################################################
fP_longWeight <- familyPanel[c('uniqueID',
                               'longWeight84',
                               'longWeight89',
                               'longWeight94',
                               'longWeight99',
                               'longWeight01',
                               'longWeight03',
                               'longWeight05',
                               'longWeight07',
                               'longWeight09',
                               'longWeight11',
                               'longWeight13' )]

# Specify id.vars: the variables to keep but not split apart on
fP_longWeight <- melt(fP_longWeight, id.vars=c("uniqueID"))
fP_longWeight$year <- ifelse(fP_longWeight$variable=='longWeight84',1984,
                             ifelse(fP_longWeight$variable=='longWeight89',1989,
                                    ifelse(fP_longWeight$variable=='longWeight94',1994,
                                           ifelse(fP_longWeight$variable=='longWeight99',1999,
                                                  ifelse(fP_longWeight$variable=='longWeight01',2001,
                                                         ifelse(fP_longWeight$variable=='longWeight03',2003,
                                                                ifelse(fP_longWeight$variable=='longWeight05',2005,
                                                                       ifelse(fP_longWeight$variable=='longWeight07',2007,
                                                                              ifelse(fP_longWeight$variable=='longWeight09',2009,
                                                                                     ifelse(fP_longWeight$variable=='longWeight11',2011,2013))))))))))

fP_longWeight <- rename(fP_longWeight, c('value' = 'longWeight'))
fP_longWeight <-subset(fP_longWeight,select=-c(variable))

# impWealthWOE ##################################################################################################################################################################
fP_impWealthWOE <- familyPanel[c('uniqueID',
                                 'impWealthWOE84',
                                 'impWealthWOE89',
                                 'impWealthWOE94',
                                 'impWealthWOE99',
                                 'impWealthWOE01',
                                 'impWealthWOE03',
                                 'impWealthWOE05',
                                 'impWealthWOE07',
                                 'impWealthWOE09',
                                 'impWealthWOE11',
                                 'impWealthWOE13' )]

# Specify id.vars: the variables to keep but not split apart on
fP_impWealthWOE <- melt(fP_impWealthWOE, id.vars=c("uniqueID"))
fP_impWealthWOE$year <- ifelse(fP_impWealthWOE$variable=='impWealthWOE84',1984,
                               ifelse(fP_impWealthWOE$variable=='impWealthWOE89',1989,
                                      ifelse(fP_impWealthWOE$variable=='impWealthWOE94',1994,
                                             ifelse(fP_impWealthWOE$variable=='impWealthWOE99',1999,
                                                    ifelse(fP_impWealthWOE$variable=='impWealthWOE01',2001,
                                                           ifelse(fP_impWealthWOE$variable=='impWealthWOE03',2003,
                                                                  ifelse(fP_impWealthWOE$variable=='impWealthWOE05',2005,
                                                                         ifelse(fP_impWealthWOE$variable=='impWealthWOE07',2007,
                                                                                ifelse(fP_impWealthWOE$variable=='impWealthWOE09',2009,
                                                                                       ifelse(fP_impWealthWOE$variable=='impWealthWOE11',2011,2013))))))))))

fP_impWealthWOE <- rename(fP_impWealthWOE, c('value' = 'impWealthWOE'))
fP_impWealthWOE <-subset(fP_impWealthWOE,select=-c(variable))


# age ##################################################################################################################################################################
fP_age <- familyPanel[c('uniqueID',
                        'age84',
                        'age89',
                        'age94',
                        'age99',
                        'age01',
                        'age03',
                        'age05',
                        'age07',
                        'age09',
                        'age11',
                        'age13' )]


# Specify id.vars: the variables to keep but not split apart on
fP_age <- melt(fP_age, id.vars=c("uniqueID"))
fP_age$year <- ifelse(fP_age$variable=='age84',1984,
                      ifelse(fP_age$variable=='age89',1989,
                             ifelse(fP_age$variable=='age94',1994,
                                    ifelse(fP_age$variable=='age99',1999,
                                           ifelse(fP_age$variable=='age01',2001,
                                                  ifelse(fP_age$variable=='age03',2003,
                                                         ifelse(fP_age$variable=='age05',2005,
                                                                ifelse(fP_age$variable=='age07',2007,
                                                                       ifelse(fP_age$variable=='age09',2009,
                                                                              ifelse(fP_age$variable=='age11',2011,2013))))))))))


fP_age <- rename(fP_age, c('value' = 'age'))
fP_age <-subset(fP_age,select=-c(variable))

# family income ##################################################################################################################################################################
fP_famIncome <- familyPanel[c('uniqueID',
                        'famIncome84',
                        'famIncome89',
                        'famIncome94',
                        'famIncome99',
                        'famIncome01',
                        'famIncome03',
                        'famIncome05',
                        'famIncome07',
                        'famIncome09',
                        'famIncome11',
                        'famIncome13' )]


# Specify id.vars: the variables to keep but not split apart on
fP_famIncome <- melt(fP_famIncome, id.vars=c("uniqueID"))
fP_famIncome$year <- ifelse(fP_famIncome$variable=='famIncome84',1984,
                      ifelse(fP_famIncome$variable=='famIncome89',1989,
                             ifelse(fP_famIncome$variable=='famIncome94',1994,
                                    ifelse(fP_famIncome$variable=='famIncome99',1999,
                                           ifelse(fP_famIncome$variable=='famIncome01',2001,
                                                  ifelse(fP_famIncome$variable=='famIncome03',2003,
                                                         ifelse(fP_famIncome$variable=='famIncome05',2005,
                                                                ifelse(fP_famIncome$variable=='famIncome07',2007,
                                                                       ifelse(fP_famIncome$variable=='famIncome09',2009,
                                                                              ifelse(fP_famIncome$variable=='famIncome11',2011,2013))))))))))


fP_famIncome <- rename(fP_famIncome, c('value' = 'famIncome'))
fP_famIncome <-subset(fP_famIncome,select=-c(variable))




# number in family ##################################################################################################################################################################
fP_numInFam <- familyPanel[c('uniqueID',
                              'numInFam84',
                              'numInFam89',
                              'numInFam94',
                              'numInFam99',
                              'numInFam01',
                              'numInFam03',
                              'numInFam05',
                              'numInFam07',
                              'numInFam09',
                              'numInFam11',
                              'numInFam13' )]


# Specify id.vars: the variables to keep but not split apart on
fP_numInFam <- melt(fP_numInFam, id.vars=c("uniqueID"))
fP_numInFam$year <- ifelse(fP_numInFam$variable=='numInFam84',1984,
                            ifelse(fP_numInFam$variable=='numInFam89',1989,
                                   ifelse(fP_numInFam$variable=='numInFam94',1994,
                                          ifelse(fP_numInFam$variable=='numInFam99',1999,
                                                 ifelse(fP_numInFam$variable=='numInFam01',2001,
                                                        ifelse(fP_numInFam$variable=='numInFam03',2003,
                                                               ifelse(fP_numInFam$variable=='numInFam05',2005,
                                                                      ifelse(fP_numInFam$variable=='numInFam07',2007,
                                                                             ifelse(fP_numInFam$variable=='numInFam09',2009,
                                                                                    ifelse(fP_numInFam$variable=='numInFam11',2011,2013))))))))))


fP_numInFam <- rename(fP_numInFam, c('value' = 'numInFam'))
fP_numInFam <-subset(fP_numInFam,select=-c(variable))
# employment status ##################################################################################################################################################################
fP_empStatus <- familyPanel[c('uniqueID',
                              'empStatus84',
                              'empStatus89',
                              'empStatus94',
                              'empStatus99',
                              'empStatus01',
                              'empStatus03',
                              'empStatus05',
                              'empStatus07',
                              'empStatus09',
                              'empStatus11',
                              'empStatus13' )]



# Specify id.vars: the variables to keep but not split apart on
fP_empStatus <- melt(fP_empStatus, id.vars=c("uniqueID"))
fP_empStatus$year <- ifelse(fP_empStatus$variable=='empStatus84',1984,
                            ifelse(fP_empStatus$variable=='empStatus89',1989,
                                   ifelse(fP_empStatus$variable=='empStatus94',1994,
                                          ifelse(fP_empStatus$variable=='empStatus99',1999,
                                                 ifelse(fP_empStatus$variable=='empStatus01',2001,
                                                        ifelse(fP_empStatus$variable=='empStatus03',2003,
                                                               ifelse(fP_empStatus$variable=='empStatus05',2005,
                                                                      ifelse(fP_empStatus$variable=='empStatus07',2007,
                                                                             ifelse(fP_empStatus$variable=='empStatus09',2009,
                                                                                    ifelse(fP_empStatus$variable=='empStatus11',2011,2013))))))))))

fP_empStatus <- rename(fP_empStatus, c('value' = 'empStatus'))
fP_empStatus <-subset(fP_empStatus,select=-c(variable))

# highest school level reached ##################################################################################################################################################################
fP_highestSchoolLev <- familyPanel[c('uniqueID',
                                     'highestSchoolLev84',
                                     'highestSchoolLev89',
                                     'highestSchoolLev94',
                                     'highestSchoolLev99',
                                     'highestSchoolLev01',
                                     'highestSchoolLev03',
                                     'highestSchoolLev05',
                                     'highestSchoolLev07',
                                     'highestSchoolLev09',
                                     'highestSchoolLev11',
                                     'highestSchoolLev13' )]



# Specify id.vars: the variables to keep but not split apart on
fP_highestSchoolLev <- melt(fP_highestSchoolLev, id.vars=c("uniqueID"))
fP_highestSchoolLev$year <- ifelse(fP_highestSchoolLev$variable=='highestSchoolLev84',1984,
                                   ifelse(fP_highestSchoolLev$variable=='highestSchoolLev89',1989,
                                          ifelse(fP_highestSchoolLev$variable=='highestSchoolLev94',1994,
                                                 ifelse(fP_highestSchoolLev$variable=='highestSchoolLev99',1999,
                                                        ifelse(fP_highestSchoolLev$variable=='highestSchoolLev01',2001,
                                                               ifelse(fP_highestSchoolLev$variable=='highestSchoolLev03',2003,
                                                                      ifelse(fP_highestSchoolLev$variable=='highestSchoolLev05',2005,
                                                                             ifelse(fP_highestSchoolLev$variable=='highestSchoolLev07',2007,
                                                                                    ifelse(fP_highestSchoolLev$variable=='highestSchoolLev09',2009,
                                                                                           ifelse(fP_highestSchoolLev$variable=='highestSchoolLev11',2011,2013))))))))))

fP_highestSchoolLev <- rename(fP_highestSchoolLev, c('value' = 'highestSchoolLev'))
fP_highestSchoolLev <-subset(fP_highestSchoolLev,select=-c(variable))



# merge all of these files together

familyPanel <- merge(fP_ID, fP_longWeight, by='uniqueID')
familyPanel <- merge(familyPanel, fP_impWealthWOE, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_age, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_famIncome, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_numInFam, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_empStatus, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_highestSchoolLev, by = c('uniqueID', 'year'))
familyPanel <-merge(familyPanel,fP_activeSaving, by=c('uniqueID', 'year'), all=TRUE)
familyPanel <-merge(familyPanel,fP_PSIDas, by=c('uniqueID', 'year'), all=TRUE)



#save(familyPanel, file='famPanel.R')
#load(file='famPanel.R')
rm(fP_activeSaving, fP_age, fP_empStatus, fP_famIncome, fP_highestSchoolLev, fP_ID,
   fP_impWealthWOE, fP_longWeight, fP_numInFam, fP_PSIDas)

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
  
  # get some moments of my active saving data
  vars <- c("age","famIncome", "activeSaving", "numInFam", "empStatus", "highestSchoolLev", "impWealthWOE")
  niceNames <- c("Age", "Family Income", "Active Saving", "Number in Family", "Employment Status", "Highest School Level Achieved (years)", "Wealth (excluding housing equity)")
  niceName <- 1
  years <- c('1984','1989', '1994', '1999', '2001', '2003', '2005', '2007', '2009', '2011', '2013')
  yearsAS <- c('1989', '1994', '1999', '2001', '2003', '2005', '2007', '2009', '2011', '2013')
  setwd("/Users/Rachel/Documents/PhD/PhD\ Thesis/Chapter2/")
  for(j in vars){
    if(j=='activeSaving'){
      k <- grep(j,colnames(familyPanelSurveyAS$variables))
      # sd
      assign(paste(j,"Var",sep=''), svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurveyAS,svyvar))
      #N, min, max
      assign(paste("N",j,sep=''), c())
      min <- c()
      max <- c()
      assign(paste("sd",j,sep=''),c())
      assign(paste("se",j,sep=''),c())
      l <- 1
      for (i in yearsAS){
        # counts
        assign(paste("N", j,sep=''),c(eval(as.name(paste("N",j,sep=''))),
                                      nrow(subset(familyPanelSurveyAS, familyPanelSurveyAS$variables$year==i &familyPanelSurveyAS$variables$longWeight>0))))
        min <-c(min,min(subset(familyPanelSurveyAS,
                               familyPanelSurveyAS$variables$year==i & familyPanelSurveyAS$variables$longWeight>0)$variables[k]))
        max <- c(max,max(subset(familyPanelSurveyAS,
                                familyPanelSurveyAS$variables$year==i & familyPanelSurveyAS$variables$longWeight>0)$variables[k]))

        # means
        assign(paste(j,"Means",sep=''),svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurveyAS,svymean))
        # medians
        assign(paste(j,"Medians",sep=''),
               svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurveyAS,svyquantile, quantiles=0.5,ci=TRUE))
        # delta method to get SD and SE of SD
        assign(paste("sd",j,sep=''), c(eval(as.name(paste("sd",j,sep=''))),sqrt(diag(coef(eval(as.name(paste(j,"Var",sep=''))))))[l,l]))
        assign(paste("se",j,sep=''), c(eval(as.name(paste("se",j,sep=''))), sqrt(vcov(eval(as.name(paste(j,"Var",sep='')))))[i,i]/(4*coef(eval(as.name(paste(j,"Var",sep=''))))[i])))
        l <- l + 1
      }
      # put together the N columns
      assign(paste("N",j,sep=''),cbind(eval(as.name(paste("N",j,sep=''))),
                                       min,
                                       max,
                                       yearsAS))
      }
    else{
      k <- grep(j,colnames(familyPanelSurvey$variables))
      # sd
      assign(paste(j,"Var",sep=''), svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurvey,svyvar))
      #N, min, max
      assign(paste("N",j,sep=''), c())
      min <- c()
      max <- c()
      assign(paste("sd",j,sep=''),c())
      assign(paste("se",j,sep=''),c())
      l <- 1
      for (i in years){
        # counts
        assign(paste("N", j,sep=''),c(eval(as.name(paste("N",j,sep=''))),
                                      nrow(subset(familyPanelSurvey, familyPanelSurvey$variables$year==i &familyPanelSurvey$variables$longWeight>0))))
        min <-c(min,min(subset(familyPanelSurvey,
                               familyPanelSurvey$variables$year==i & familyPanelSurvey$variables$longWeight>0)$variables[k]))
        max <- c(max,max(subset(familyPanelSurvey,
                                familyPanelSurvey$variables$year==i & familyPanelSurvey$variables$longWeight>0)$variables[k]))

        # means
        assign(paste(j,"Means",sep=''),svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurvey,svymean))
        # medians
        assign(paste(j,"Medians",sep=''),
               svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurvey,svyquantile, quantiles=0.5,ci=TRUE))
        # delta method to get SD and SE of SD
        assign(paste("sd",j,sep=''), c(eval(as.name(paste("sd",j,sep=''))),sqrt(diag(coef(eval(as.name(paste(j,"Var",sep=''))))))[l,l]))
        assign(paste("se",j,sep=''), c(eval(as.name(paste("se",j,sep=''))), sqrt(vcov(eval(as.name(paste(j,"Var",sep='')))))[i,i]/(4*coef(eval(as.name(paste(j,"Var",sep=''))))[i])))
        l <- l + 1
      
      }
      # put together the N columns
      assign(paste("N",j,sep=''),cbind(eval(as.name(paste("N",j,sep=''))),
                                       min,
                                       max,
                                       years))
    }
    
    
    # rename the N columns
    assign(paste("N",j,sep=''), as.data.frame(eval(as.name(paste("N",j,sep='')))))
    assign(paste("N",j,sep=''), rename(eval(as.name(paste("N",j,sep=''))),
                                       c("V1"="N")))
    # rename the means
    assign(paste(j,"Means",sep=''),subset(eval(as.name(paste(j,"Means",sep=''))),select=-c(factor('year'))))
    assign(paste(j,"Means",sep=''), as.data.frame(eval(as.name(paste(j,"Means",sep='')))))
    assign(paste(j,"Means",sep=''), rename(eval(as.name(paste(j,"Means",sep=''))),
                                           c("eval(as.symbol(j))"="mean")))
    # rename the medians
    assign(paste(j,"Medians",sep=''),subset(eval(as.name(paste(j,"Medians",sep=''))),select=-c(factor('year'))))
    
    # rename the sds
    assign(paste(j,"SDs",sep=''), cbind(eval(as.name(paste("sd",j,sep=''))), eval(as.name(paste("se",j,sep='')))))
    assign(paste(j,"SDs",sep=''), as.data.frame(eval(as.name(paste(j,"SDs",sep='')))))
    assign(paste(j,"SDs",sep=''), rename(eval(as.name(paste(j,"SDs",sep=''))),
                                         c("V1"="sd",
                                           "V2"="se")))
    # put all together
    assign(paste(j,"ALL",sep=''),
           merge(eval(as.name(paste(j,"Means",sep=''))),
                 eval(as.name(paste(j,"SDs",sep=''))), by='row.names'))
    if(j=='activeSaving'){
      assign(paste(j,"ALL",sep=''),
             merge(eval(as.name(paste("N",j,sep=''))),
                   eval(as.name(paste(j,"ALL",sep=''))), by.x='yearsAS', by.y='Row.names'))
      assign(paste(j,"ALL",sep=''),
             merge(eval(as.name(paste(j,"Medians",sep=''))),
                   eval(as.name(paste(j,"ALL",sep=''))), by.x='row.names', by.y='yearsAS'))
      
      
    }else{
    assign(paste(j,"ALL",sep=''),
           merge(eval(as.name(paste("N",j,sep=''))),
                 eval(as.name(paste(j,"ALL",sep=''))), by.x='years', by.y='Row.names'))
    assign(paste(j,"ALL",sep=''),
           merge(eval(as.name(paste(j,"Medians",sep=''))),
                 eval(as.name(paste(j,"ALL",sep=''))), by.x='row.names', by.y='years'))
    }
    assign(paste(j,"ALL",sep=''),
           eval(as.name(paste(j,"ALL",sep='')))[,c(1,4,5,6,7,8,2,3,9,10)])
    assign(paste(j,"ALL",sep=''),rename(eval(as.name(paste(j,"ALL",sep=''))),
                                        c("Row.names"="year",
                                          "se.x"="SE",
                                          "sd" ="$\\sigma_{x}$",
                                          "se.y"="SE",
                                          "eval(as.symbol(j))"="median",
                                          "se"="SE"
                                        )))
    # spit out to latex
    xtable <- xtable(eval(as.name(paste(j,"ALL",sep=''))))
    digits(xtable) <-2
    caption(xtable) <-niceNames[niceName]
    print(xtable,include.rownames=FALSE, caption.placement="top",size="footnotesize",  sanitize.text.function = function(x) {x},type="latex",file = paste("moments", niceName,".txt",sep=''))                                  
    niceName <- niceName + 1
  }
  setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


rm(activeSavingMeans, activeSavingMedians, activeSavingSDs, activeSavingVar)
rm(ageALL, ageMeans, ageMedians, ageSDs, ageVar)
rm(empStatusALL, empStatusMeans, empStatusMedians, empStatusSDs, empStatusVar)
rm(famIncomeALL, famIncomeMeans, famIncomeMedians, famIncomeSDs, famIncomeVar)
rm(Nage,NactiveSaving, NempStatus, NfamIncome, NhighestSchoolLev, NimpWealthWOE, NnumInFam)
rm(highestSchoolLevALL, highestSchoolLevMeans, highestSchoolLevMedians, highestSchoolLevSDs, highestSchoolLevVar)
rm(numInFamALL, numInFamMeans, numInFamMedians, numInFamSDs, numInFamVar)
rm(impWealthWOEALL, impWealthWOEMeans, impWealthWOEMedians, impWealthWOESDs, impWealthWOEVar)
rm(xtable, i, j, k, l, max, min, niceName, niceNames, sdactiveSaving, sdage, sdempStatus, sdfamIncome, sdhighestSchoolLev, sdimpWealthWOE, sdnumInFam)
rm(seage, seactiveSaving, seempStatus, sefamIncome, sehighestSchoolLev, seimpWealthWOE, senumInFam)
rm(vars, years)


# compare my active savings data with the PSID one (only available in 1989)
PSIDTestData <- na.omit(familyPanel)
PSIDTestData <- subset(PSIDTestData, year==1989)
PSIDTestData <- subset(PSIDTestData, select=c('uniqueID',
                                              'primarySamplingUnit',
                                              'stratification',
                                              'longWeight',
                                              'PSIDas'))



#create a survey design 
PSIDSurvey <- svydesign(id=~primarySamplingUnit,
                               strat=~stratification, 
                               weights=~longWeight,
                               data=PSIDTestData,
                               nest=TRUE)


k <- grep('PSIDas',colnames(PSIDSurvey$variables))
#N, min, max
N_PSIDas <- nrow(subset(PSIDSurvey, PSIDSurvey$variables$longWeight>0))
min <- min(subset(PSIDSurvey, PSIDSurvey$variables$longWeight>0)$variables[k])
max <- max(subset(PSIDSurvey,
                  PSIDSurvey$variables$longWeight>0)$variables[k])
# sd
PSIDasVar <- svyvar(~PSIDas, PSIDSurvey)
# delta method to get SD and SE
sdPSIDas <- sqrt(coef(PSIDasVar))
sePSIDas <- sqrt(vcov(PSIDasVar))[1,1]/(4*vcov(PSIDasVar)[1])


# put together the N columns
N_PSIDas <- cbind(N_PSIDas,
                  min,
                  max)

# rename the N columns
N_PSIDas <- as.data.frame(N_PSIDas)
N_PSIDas <- rename(N_PSIDas, c("N_PSIDas"="N"))

# means
PSIDasMeans <- svymean(~PSIDas,PSIDSurvey)
PSIDasMeans <- as.data.frame(PSIDasMeans)

# rename the mean
PSIDasMeans <-rename(PSIDasMeans, c("PSIDas"='se'))


# medians
PSIDasMedians <- svyquantile(~PSIDas, PSIDSurvey,quantiles=0.5,ci=TRUE)$quantiles[1]
PSIDasMedians <- c(PSIDasMedians, attr(svyquantile(~PSIDas, PSIDSurvey,quantiles=0.5,ci=TRUE), "SE"))
PSIDasMedians <- as.data.frame(PSIDasMedians)

# sd
PSIDasSDs <- cbind(sdPSIDas, sePSIDas)
PSIDasSDs <- as.data.frame(PSIDasSDs)
# rename the sds
PSIDasSDs <- rename(PSIDasSDs, c("sdPSIDas"="sd",
                                 "sePSIDas"="se"))
# put all together
PSIDALL <- merge(PSIDasMeans, PSIDasSDs, by='row.names')
PSIDALL <- merge(N_PSIDas,PSIDALL)
PSIDALL <- subset(PSIDALL, select =-c(Row.names))
PSIDALL <- cbind(PSIDasMedians[2,1], PSIDALL)
PSIDALL <- cbind(PSIDasMedians[1,1], PSIDALL)
PSIDALL <- rename(PSIDALL, c("PSIDasMedians[2, 1]" = "SE","PSIDasMedians[1, 1]" = "median" ))
PSIDALL <- PSIDALL[,c(3,4,5,6,7,1,2,8,9)]
PSIDALL <- rename(PSIDALL,c("se.x"="SE",
                            "sd" ="$\\sigma_{x}$",
                            "se.y"="SE.2",
                            "SE" = "SE.1"))
PSIDALL$variable <- "PSID"
activeSavingTest <-subset(activeSavingALL[1,], select=-(year))
activeSavingTest$variable <-'mine'

PSIDALL <- rbind(PSIDALL, activeSavingTest)
PSIDALL <- PSIDALL[,c(10,1,2,3,4,5,6,7,8,9)]
PSIDALL <- rename(PSIDALL, c("SE.1"="SE","SE.2"="SE"))
# spit out to latex
xtable <- xtable(PSIDALL)
digits(xtable) <-2
caption(xtable) <-"Active Saving Comparison"
setwd("/Users/Rachel/Documents/PhD/PhD\ Thesis/Chapter2/")
print(xtable,include.rownames=FALSE, caption.placement="top",size="footnotesize",  sanitize.text.function = function(x) {x}, type="latex",file = 'ActiveSavingComparison.txt')                                  
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")



rm(PSIDTestData, familyPanelSurvey,PSIDSurvey)
rm(N_PSIDas)
rm(PSIDALL, PSIDasMeans, PSIDasMedians, PSIDasSDs)
rm(xtable, k, max, min, PSIDasVar, sdPSIDas, sePSIDas)

