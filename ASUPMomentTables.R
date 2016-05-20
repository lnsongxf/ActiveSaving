# This file spits out moments from the unbalanced panel constructed from PSID data
# need to run load.R and ASUPconstructFamPan.R and ASUPwideToLong.R first

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
setwd("/Users/Rachel/Documents/PhD/PhD\ Thesis/Chapter2")
for(j in vars){
  if(j=='activeSaving'){
    k <- grep(j,colnames(familyPanelSurveyAS$variables))
    #N, min, max
    assign(paste("N",j,sep=''), c())
    min <- c()
    max <- c()
    assign(paste("sd",j,sep=''),c())
    assign(paste("se",j,sep=''),c())
    assign(paste(j,"Means",sep=''),c())
    assign(paste(j,"Medians",sep=''),c())
    l <- 1
    for (i in yearsAS){
      # counts
      assign(paste("N", j,sep=''),c(eval(as.name(paste("N",j,sep=''))),
                                    nrow(subset(familyPanelSurveyAS, familyPanelSurveyAS$variables$year==i &
                                                  familyPanelSurveyAS$variables$longWeight>0 &
                                                  familyPanelSurveyAS$variables$hhHead==1))))
      min <-c(min,min(subset(familyPanelSurveyAS,
                             familyPanelSurveyAS$variables$year==i &
                               familyPanelSurveyAS$variables$longWeight>0 &
                               familyPanelSurveyAS$variables$hhHead==1)$variables[k]))
      max <- c(max,max(subset(familyPanelSurveyAS,
                              familyPanelSurveyAS$variables$year==i &
                                familyPanelSurveyAS$variables$longWeight>0 &
                                familyPanelSurveyAS$variables$hhHead==1)$variables[k]))
      
      # means
      #assign(paste(j,"Means",sep=''),svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurveyAS,svymean))
      temp <-svymean(~eval(as.symbol(j)),subset(familyPanelSurveyAS, familyPanelSurveyAS$variables$year==i &
                                                  familyPanelSurveyAS$variables$hhHead==1))
      tempM <- mean(temp)
      tempSE <- SE(temp)
      assign(paste(j,"Means",sep=''),rbind(eval(as.name(paste(j,'Means',sep=''))),
                                       merge(tempM,tempSE)))
      # medians
      #assign(paste(j,"Medians",sep=''),
      #       svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurveyAS,svyquantile, quantiles=0.5,ci=TRUE))
      temp <-svyquantile(~eval(as.symbol(j)),subset(familyPanelSurveyAS, familyPanelSurveyAS$variables$year==i &
                                                      familyPanelSurveyAS$variables$hhHead==1),quantiles=0.5, ci=TRUE)
      tempM <-temp$quantiles
      tempSE <- SE(temp)
      assign(paste(j,"Medians",sep=''),rbind(eval(as.name(paste(j,'Medians',sep=''))),merge(tempM,tempSE)))
      # sd
      #assign(paste(j,"Var",sep=''), svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurveyAS,svyvar))
      assign(paste(j,"Var",sep=''), svyvar(~eval(as.symbol(j)),subset(familyPanelSurveyAS, familyPanelSurveyAS$variables$year==i &
                                                                        familyPanelSurveyAS$variables$hhHead==1)))
      # delta method to get SD and SE of SD
      #assign(paste("sd",j,sep=''), c(eval(as.name(paste("sd",j,sep=''))),sqrt(diag(coef(eval(as.name(paste(j,"Var",sep=''))))))[l,l]))
      assign(paste("sd",j,sep=''), c(eval(as.name(paste("sd",j,sep=''))),sqrt(eval(as.name(paste(j,"Var",sep='')))[1])))
      assign(paste("se",j,sep=''), c(eval(as.name(paste("se",j,sep=''))),
                                     sqrt(vcov(eval(as.name(paste(j,"Var",sep='')))))/(4*eval(as.name(paste(j,"Var",sep='')))[1])))
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
    #N, min, max
    assign(paste("N",j,sep=''), c())
    min <- c()
    max <- c()
    assign(paste("sd",j,sep=''),c())
    assign(paste("se",j,sep=''),c())
    assign(paste(j,"Means",sep=''),c())
    assign(paste(j,"Medians",sep=''),c())
    l <- 1
    for (i in years){
      # counts
      assign(paste("N", j,sep=''),c(eval(as.name(paste("N",j,sep=''))),
                                    nrow(subset(familyPanelSurvey, familyPanelSurvey$variables$year==i &
                                                  familyPanelSurvey$variables$longWeight>0 &
                                                  familyPanelSurvey$variables$hhHead==1))))
      min <-c(min,min(subset(familyPanelSurvey,
                             familyPanelSurvey$variables$year==i &
                               familyPanelSurvey$variables$longWeight>0 &
                               familyPanelSurvey$variables$hhHead==1)$variables[k]))
      max <- c(max,max(subset(familyPanelSurvey,
                              familyPanelSurvey$variables$year==i & 
                                familyPanelSurvey$variables$longWeight>0 &
                                familyPanelSurvey$variables$hhHead==1)$variables[k]))
      
      # means
      #assign(paste(j,"Means",sep=''),svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurvey,svymean))
      temp <-svymean(~eval(as.symbol(j)),subset(familyPanelSurvey, familyPanelSurvey$variables$year==i &
                                                  familyPanelSurvey$variables$hhHead==1))
      tempM <- mean(temp)
      tempSE <- SE(temp)
      assign(paste(j,"Means",sep=''),rbind(eval(as.name(paste(j,'Means',sep=''))),
                                           merge(tempM,tempSE)))
      # medians
      #assign(paste(j,"Medians",sep=''),
      #       svyby(~eval(as.symbol(j)), ~factor(year),familyPanelSurvey,svyquantile, quantiles=0.5,ci=TRUE))
      temp <-svyquantile(~eval(as.symbol(j)),subset(familyPanelSurvey, familyPanelSurvey$variables$year==i &
                                                           familyPanelSurvey$variables$hhHead==1),quantiles=0.5, ci=TRUE)
      tempMedian <-temp$quantiles
      tempMedianSE <- SE(temp)
      assign(paste(j,"Medians",sep=''),rbind(eval(as.name(paste(j,'Medians',sep=''))),merge(tempMedian,tempMedianSE)))
      #sd
      assign(paste(j,"Var",sep=''), svyvar(~eval(as.symbol(j)),subset(familyPanelSurvey, familyPanelSurvey$variables$year==i &
                                                                        familyPanelSurvey$variables$hhHead==1)))
      # delta method to get SD and SE of SD
      assign(paste("sd",j,sep=''), c(eval(as.name(paste("sd",j,sep=''))),sqrt(eval(as.name(paste(j,"Var",sep='')))[1])))
      assign(paste("se",j,sep=''), c(eval(as.name(paste("se",j,sep=''))),
                                     sqrt(vcov(eval(as.name(paste(j,"Var",sep='')))))/(4*eval(as.name(paste(j,"Var",sep='')))[1])))
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
                                     c('V1'="N")))
  # rename the means
  #assign(paste(j,"Means",sep=''),subset(eval(as.name(paste(j,"Means",sep=''))),select=-c(factor('year'))))
  assign(paste(j,"Means",sep=''), as.data.frame(eval(as.name(paste(j,"Means",sep='')))))
  #assign(paste(j,"Means",sep=''), rename(eval(as.name(paste(j,"Means",sep=''))),
  #                                       c("eval(as.symbol(j))"="mean")))
  assign(paste(j,"Means",sep=''), rename(eval(as.name(paste(j,"Means",sep=''))),
                                         c('x'="mean", 'eval(as.symbol(j))'='se')))
  if(j=='activeSaving'){
    assign(paste(j,"Means",sep=''), cbind(as.data.frame(yearsAS),eval(as.name(paste(j,"Means",sep='')))))
  }else{
    assign(paste(j,"Means",sep=''), cbind(as.data.frame(years),eval(as.name(paste(j,"Means",sep='')))))}
  # rename the medians
  assign(paste(j,"Medians",sep=''), as.data.frame(eval(as.name(paste(j,"Medians",sep='')))))
  #assign(paste(j,"Medians",sep=''),subset(eval(as.name(paste(j,"Medians",sep=''))),select=-c(factor('year'))))
  assign(paste(j,"Medians",sep=''), rename(eval(as.name(paste(j,"Medians",sep=''))),
                                         c('0.5'="median", 'y'='se')))
  if(j=='activeSaving'){
    assign(paste(j,"Medians",sep=''), cbind(as.data.frame(yearsAS),eval(as.name(paste(j,"Medians",sep='')))))
  }else{
    assign(paste(j,"Medians",sep=''), cbind(as.data.frame(years),eval(as.name(paste(j,"Medians",sep='')))))}
  
  # rename the sds
  assign(paste(j,"SDs",sep=''), cbind(eval(as.name(paste("sd",j,sep=''))), eval(as.name(paste("se",j,sep='')))))
  assign(paste(j,"SDs",sep=''), as.data.frame(eval(as.name((paste(j,"SDs",sep=''))))))
  assign(paste(j,"SDs",sep=''), rename(eval(as.name(paste(j,"SDs",sep=''))),
                                       c("V1"="sd",
                                         "V2"="se")))
  if(j=='activeSaving'){
    assign(paste(j,"SDs",sep=''), cbind(as.data.frame(yearsAS),eval(as.name(paste(j,"SDs",sep='')))))
  }else{
    assign(paste(j,"SDs",sep=''), cbind(as.data.frame(years),eval(as.name(paste(j,"SDs",sep='')))))}
  # put all together
  if(j=='activeSaving'){
    assign(paste(j,"ALL",sep=''),
           merge(eval(as.name(paste(j,"Means",sep=''))),
                 eval(as.name(paste(j,"SDs",sep=''))),by='yearsAS'))
    assign(paste(j,"ALL",sep=''),
           merge(eval(as.name(paste("N",j,sep=''))),
                 eval(as.name(paste(j,"ALL",sep=''))), by='yearsAS'))
    assign(paste(j,"ALL",sep=''),
           merge(eval(as.name(paste(j,"Medians",sep=''))),
                 eval(as.name(paste(j,"ALL",sep=''))),by='yearsAS'))
    assign(paste(j,"ALL",sep=''),
           eval(as.name(paste(j,"ALL",sep='')))[,c(1,4,5,6,7,8,2,3,9,10)])
    assign(paste(j,"ALL",sep=''),rename(eval(as.name(paste(j,"ALL",sep=''))),
                                        c("yearsAS"="year",
                                          "se.x"="SE",
                                          "sd" ="$\\sigma_{x}$",
                                          "se.y"="SE",
                                          "se"="SE")))
    
  }else{
    assign(paste(j,"ALL",sep=''),
           merge(eval(as.name(paste(j,"Means",sep=''))),
                 eval(as.name(paste(j,"SDs",sep=''))),by='years'))
    assign(paste(j,"ALL",sep=''),
           merge(eval(as.name(paste("N",j,sep=''))),
                 eval(as.name(paste(j,"ALL",sep=''))), by='years'))
    assign(paste(j,"ALL",sep=''),
           merge(eval(as.name(paste(j,"Medians",sep=''))),
                 eval(as.name(paste(j,"ALL",sep=''))), by='years'))
    assign(paste(j,"ALL",sep=''),
           eval(as.name(paste(j,"ALL",sep='')))[,c(1,4,5,6,7,8,2,3,9,10)])
    assign(paste(j,"ALL",sep=''),rename(eval(as.name(paste(j,"ALL",sep=''))),
                                        c("years"="year",
                                          "se.x"="SE",
                                          "sd" ="$\\sigma_{x}$",
                                          "se.y"="SE",
                                          "se"="SE")))
  }

                                      
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
PSIDTestData <-subset(PSIDTestData,PSIDTestData$hhHead==1)
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
setwd("/Users/Rachel/Documents/PhD/PhD\ Thesis/Chapter2")
print(xtable,include.rownames=FALSE, caption.placement="top",size="footnotesize",  sanitize.text.function = function(x) {x}, type="latex",file = 'ActiveSavingComparison.txt')                                  
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")



rm(PSIDTestData,PSIDSurvey)
rm(N_PSIDas)
rm(PSIDALL, PSIDasMeans, PSIDasMedians, PSIDasSDs)
rm(xtable, k, max, min, PSIDasVar, sdPSIDas, sePSIDas)
rm(activeSavingALL, activeSavingTest, dataFP)
rm(tempMedian, tempSE, familyPanelSurvey, familyPanelSurveyAS, temp, tempM, tempMedianSE, yearsAS)
