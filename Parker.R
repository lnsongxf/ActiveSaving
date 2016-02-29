# Related to Parker - The Consumption Function Re-Estimated

rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data
library(data.table) # easy subsetting

source("ActiveSavingLoadData.R")


# gah, clean the individual years up - I should have done this a long time ago! Go back and sort this when the individual and family level files are merged!!
#for(i in years){
#  shortYear = substr(i,3,4)
#  for(j in years){
#    if(i==j){
#      next
#    }else{
#      shortYearj = substr(j, 3,4)
#      assign(paste("z", shortYear, sep=''), 
#             subset(eval(as.name(paste("z", shortYear, sep=''))), 
#                    select = -c(#eval(as.name(paste('intNum', shortYearj, sep=''))),
#                                #eval(as.name(paste('sequenceNum', shortYearj, sep=''))),
#                                #eval(as.name(paste('age', shortYearj, sep=''))),
#                                #eval(as.name(paste('empStatus', shortYearj, sep=''))),
#                                #eval(as.name(paste('hhRelStatus', shortYearj, sep=''))),
#                                eval(as.name(paste('longWeight', shortYearj, sep=''))))))
#                                                                                                             
#    }
#  }
#}


# drop any individuals with dumb age categories - NA, DK etc
for(i in years){
  shortYear <- substr(i,3,4)
  assign(paste("z",shortYear,sep=''),
         subset(eval(as.name(paste("z",shortYear,sep=''))),
                eval(as.name(paste("age",shortYear,sep='')))>0 & eval(as.name(paste("age",shortYear,sep='')))<126))
}
rm(shortYear,i)
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
  # number of adults
  assign(paste("DT", shortYear, sep='' ), as.data.table(eval(as.name(paste("z",shortYear,sep='')))))
  assign(paste("temp", shortYear, sep=''),
         eval(as.name(paste("DT", shortYear, sep='' )))[,sum(adult), by=eval(paste("intNum", shortYear, sep=''))])
  # sum the individual weights
  assign(paste("temp2", shortYear, sep=''),
         eval(as.name(paste("DT", shortYear, sep='' )))[,sum(indWeight), by=eval(paste("intNum", shortYear, sep=''))])
  }



# merge the original file and the num adults & weights files
for(i in years){
  shortYear <-substr(i,3,4)
  assign(paste("z",shortYear, sep=''), merge(eval(as.name(paste("z", shortYear, sep=''))), 
                                             eval(as.name(paste("temp",shortYear,sep=''))), 
                                             by = eval(paste('intNum', shortYear, sep=''))))
  assign(paste("z",shortYear, sep=''), rename(eval(as.name(paste("z", shortYear, sep=''))), 
                                             c('V1'='numAdults')))
  assign(paste("z",shortYear, sep=''), merge(eval(as.name(paste("z", shortYear, sep=''))), 
                                             eval(as.name(paste("temp2",shortYear,sep=''))), 
                                             by = eval(paste('intNum', shortYear, sep=''))))
  assign(paste("z",shortYear, sep=''), rename(eval(as.name(paste("z", shortYear, sep=''))), 
                                              c('V1'='OECDEquivAdults')))
  eval(call("rm",paste("DT",shortYear,sep="")))
  eval(call("rm",paste("temp",shortYear,sep="")))
  eval(call("rm",paste("temp2",shortYear,sep="")))
}


# calculate savings, change in income

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

# create an 'impActiveSaving' variable which is impWealth change year to year
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





# calculate adjusted family size
x8489$adjFamNum <- x8489$numInFam84*x8489$impActiveSaving/
  (x8489$famIncome84*(1-(x8489$numInFam84/x8489$OECDEquivAdults.x)) + (x8489$numInFam84/x8489$OECDEquivAdults.x)*x8489$impActiveSaving)
x8994$adjFamNum <- x8994$numInFam89*x8994$impActiveSaving/
  (x8994$famIncome89*(1-(x8994$numInFam89/x8994$OECDEquivAdults.x)) + (x8994$numInFam89/x8994$OECDEquivAdults.x)*x8994$impActiveSaving)
x9499$adjFamNum <- x9499$numInFam94*x9499$impActiveSaving/
  (x9499$famIncome94*(1-(x9499$numInFam94/x9499$OECDEquivAdults.x)) + (x9499$numInFam94/x9499$OECDEquivAdults.x)*x9499$impActiveSaving)
x9901$adjFamNum <- x9901$numInFam99*x9901$impActiveSaving/
  (x9901$famIncome99*(1-(x9901$numInFam99/x9901$OECDEquivAdults.x)) + (x9901$numInFam99/x9901$OECDEquivAdults.x)*x9901$impActiveSaving)
x0103$adjFamNum <- x0103$numInFam01*x0103$impActiveSaving/
  (x0103$famIncome01*(1-(x0103$numInFam01/x0103$OECDEquivAdults.x)) + (x0103$numInFam01/x0103$OECDEquivAdults.x)*x0103$impActiveSaving)
x0305$adjFamNum <- x0305$numInFam03*x0305$impActiveSaving/
  (x0305$famIncome03*(1-(x0305$numInFam03/x0305$OECDEquivAdults.x)) + (x0305$numInFam03/x0305$OECDEquivAdults.x)*x0305$impActiveSaving)
x0507$adjFamNum <- x0507$numInFam05*x0507$impActiveSaving/
  (x0507$famIncome05*(1-(x0507$numInFam05/x0507$OECDEquivAdults.x)) + (x0507$numInFam05/x0507$OECDEquivAdults.x)*x0507$impActiveSaving)



# create weighted savings
x8489$adjActiveSaving <- x8489$impActiveSaving/x8489$adjFamNum  
x8489 <- na.omit(x8489)
x8994$adjActiveSaving <- x8994$impActiveSaving/x8994$adjFamNum  
x8994 <- na.omit(x8994)
x9499$adjActiveSaving <- x9499$impActiveSaving/x9499$adjFamNum  
x9499 <- na.omit(x9499)
x9901$adjActiveSaving <- x9901$impActiveSaving/x9901$adjFamNum  
x9901 <- na.omit(x9901)
x0103$adjActiveSaving <- x0103$impActiveSaving/x0103$adjFamNum  
x0103 <- na.omit(x0103)
x0305$adjActiveSaving <- x0305$impActiveSaving/x0305$adjFamNum  
x0305 <- na.omit(x0305)
x0507$adjActiveSaving <- x0507$impActiveSaving/x0507$adjFamNum  
x0507 <- na.omit(x0507)

# drop top and bottom 1% & plot
par(mfrow = c(3, 3))
for(i in head(years, n=length(years)-1)){
  # three years
  shortYeari = substr(i,3,4)
  shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
  assign(
    paste("x",shortYeari, shortYearj,sep=''), 
    subset(eval(as.name(paste("x",shortYeari,shortYearj,sep=''))),
           eval(as.name(paste("x",shortYeari,shortYearj,sep='')))$'adjActiveSaving'<quantile(eval(as.name(paste("x",shortYeari,shortYearj,sep='')))$'adjActiveSaving', 0.99) &
             eval(as.name(paste("x",shortYeari,shortYearj,sep='')))$'adjActiveSaving'>quantile(eval(as.name(paste("x",shortYeari,shortYearj,sep='')))$'adjActiveSaving', 0.01)))
  plot(eval(as.name(paste("x",shortYeari,shortYearj,sep='')))$'adjActiveSaving')
}


