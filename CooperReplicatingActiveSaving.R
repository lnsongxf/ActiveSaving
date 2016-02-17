#File to replicate Cooper (2006) - calculating 'active savings'
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data

source("ActiveSavingLoadData.R")


# turn wide to long format for the survey weights
for(i in head(years, n=length(years)-1)){
    shortYeari = substr(i,3,4)
    shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
    assign(paste("x",shortYeari,shortYearj,sep=""), 
           gather(eval(as.name(paste("x",shortYeari,shortYearj,sep=""))),
                  weightYear,weight,eval(as.name(paste("longWeight",shortYeari,sep=""))):longWeight13))
}
rm(i,shortYeari,shortYearj)
# sort dissavers to savers, limit to matching uniqueIDs, weight & plot
#84-89, 89-94
# x8489sorted <- subset(x8489,x8489$hhHead89==1)
# x8994sorted <- subset(x8994,x8994$hhHead94==1)
# x8489_94 <- merge(x8489sorted, x8994sorted, by='uniqueID', all=FALSE)
# saving84_89_94 <-x8489_94[c('activeSaving.x', 'activeSaving.y')]
# saving84_89_94_sorted <- saving84_89_94[order(saving84_89_94$'activeSaving.x'),]

for(i in head(years, n=length(years)-2)){
  # three years
  shortYeari = substr(i,3,4)
  shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
  shortYeark = substr(years[which(years == i)[[1]]+2],3,4)
  yearString = paste(shortYeari, "_", shortYearj,"_", shortYeark,sep="")
  # merge by unique id
  assign(paste("x", yearString), 
         merge(eval(as.name(paste("x",shortYeari,shortYearj,sep=""))),eval(as.name(paste("x",shortYearj,shortYeark,sep=""))),by='uniqueID', all=FALSE))
  # keep the variables needed
  assign(paste("saving",yearString,sep=""), 
         eval(as.name(paste("x",yearString,sep="")))[c('impActiveSaving.x', 'impActiveSaving.y', 'weight.x', 'weightYear.x', 'primarySamplingUnit.y', 'stratification.y')])
  # keep only those associated with final year(in group) weight
  assign(paste("saving",yearString,sep=""),
         subset(eval(as.name(paste("saving",yearString,sep=""))),eval(as.name(paste("saving",yearString,sep="")))$weightYear.x==paste('longWeight',shortYeark,sep='')))
  # sort dissavers to savers
  assign(paste("saving",yearString,"_sorted",sep=''), 
         eval(as.name(paste("saving",yearString,sep='')))[order(eval(as.name(paste("saving",yearString,sep="")))$impActiveSaving.x),])  
  #weight
  assign(paste('y',yearString, sep=''), svydesign(
    id=~primarySamplingUnit.y , 
    strata=~stratification.y , 
    data=eval(as.name(paste("x",yearString,sep=''))), 
    weights=~weight.x, 
    nest = TRUE ))
}

#plot
par(mfrow = c(3, 3))
for(i in head(years, n=length(years)-2)){
  # three years
  shortYeari = substr(i,3,4)
  shortYearj = substr(years[which(years == i)[[1]]+1],3,4)
  shortYeark = substr(years[which(years == i)[[1]]+2],3,4)
  yearString = paste(shortYeari, "_", shortYearj,"_", shortYeark,sep="")
  #plot
  svyplot(log(impActiveSaving.x)~log(impActiveSaving.y),design=eval(as.name(paste("y",yearString,sep=''))) , style="transparent",
          pch=19,alpha=c(0,0.5), xlab=paste('Log active saving ', shortYeari, '-', shortYearj,sep=''),
          ylab=paste('Log active saving ', shortYearj, '-', shortYeark,sep=''))
}

assign(paste("x", shortYeari, "_", shortYearj,"_", shortYeark,sep=""), 
       merge(eval(as.name(paste("x",shortYeari,shortYearj,sep=""))),eval(as.name(paste("x",shortYearj,shortYeark,sep=""))),by='uniqueID', all=FALSE))
x84_89_94_t <- merge(x8489,x8994,by='uniqueID', all=FALSE)
saving84_89_94 <-x84_89_94[c('impActiveSaving.x', 'impActiveSaving.y', 'weight', 'weightYear', 'primarySamplingUnit.y', 'stratification.y')]
saving84_89_94 <- subset(saving84_89_94, saving84_89_94$weightYear=='longWeight89')

saving84_89_94_sorted <-saving84_89_94[order(saving84_89_94$'impActiveSaving.x'),]
saving84_89_94_sorted <- rename( saving84_89_94_sorted, c('impActiveSaving.x' = 'impActiveSaving8489',
                      'impActiveSaving.y' = 'impActiveSaving8994'))

#89-94, 94-99
# x9499sorted <- subset(x9499,x9499$hhHead99==1)
# x8994_99 <- merge(x8994sorted, x9499sorted, by='uniqueID', all=FALSE)
# saving89_94_99 <-x8994_99[c('activeSaving.x', 'activeSaving.y')]
# saving89_94_99_sorted <- saving89_94_99[order(saving89_94_99$'activeSaving.x'),]
x89_94_99 <- merge(x8994, x9499, by='uniqueID', all=FALSE)
saving89_94_99 <-x89_94_99[c('impActiveSaving.x', 'impActiveSaving.y')]
saving89_94_99_sorted <-saving89_94_99[order(saving89_94_99$'impActiveSaving.x'),]
saving89_94_99_sorted <- rename( saving89_94_99_sorted, c('impActiveSaving.x' = 'impActiveSaving8994',
                                                          'impActiveSaving.y' = 'impActiveSaving9499'))

#94-99, 99-01
x94_99_01 <- merge(x9499, x9901, by='uniqueID', all=FALSE)
saving94_99_01 <-x94_99_01[c('impActiveSaving.x', 'impActiveSaving.y')]
saving94_99_01_sorted <-saving94_99_01[order(saving94_99_01$'impActiveSaving.x'),]
saving94_99_01_sorted <- rename( saving94_99_01_sorted, c('impActiveSaving.x' = 'impActiveSaving9499',
                                                          'impActiveSaving.y' = 'impActiveSaving9901'))
#99-01, 01-03
x99_01_03 <- merge(x9901, x0103, by='uniqueID', all=FALSE)
saving99_01_03 <-x99_01_03[c('impActiveSaving.x', 'impActiveSaving.y')]
saving99_01_03_sorted <-saving99_01_03[order(saving99_01_03$'impActiveSaving.x'),]
saving99_01_03_sorted <- rename( saving99_01_03_sorted, c('impActiveSaving.x' = 'impActiveSaving9901',
                                                          'impActiveSaving.y' = 'impActiveSaving0103'))
#01-03, 03-05
x01_03_05 <- merge(x0103, x0305, by='uniqueID', all=FALSE)
saving01_03_05 <-x01_03_05[c('impActiveSaving.x', 'impActiveSaving.y')]
saving01_03_05_sorted <-saving01_03_05[order(saving01_03_05$'impActiveSaving.x'),]
saving01_03_05_sorted <- rename( saving01_03_05_sorted, c('impActiveSaving.x' = 'impActiveSaving0103',
                                                          'impActiveSaving.y' = 'impActiveSaving0305'))

#03-05, 05-07
x03_05_07 <- merge(x0305, x0507, by='uniqueID', all=FALSE)
saving03_05_07 <-x03_05_07[c('impActiveSaving.x', 'impActiveSaving.y')]
saving03_05_07_sorted <- saving03_05_07[order(saving03_05_07$'impActiveSaving.x'),]
saving03_05_07_sorted <- rename( saving03_05_07_sorted, c('impActiveSaving.x' = 'impActiveSaving0305',
                                                          'impActiveSaving.y' = 'impActiveSaving0507'))

#05-07, 07-09
x05_07_09 <- merge(x0507, x0709, by='uniqueID', all=FALSE)
saving05_07_09 <-x05_07_09[c('impActiveSaving.x', 'impActiveSaving.y')]
saving05_07_09_sorted <- saving05_07_09[order(saving05_07_09$'impActiveSaving.x'),]
saving05_07_09_sorted <- rename( saving05_07_09_sorted, c('impActiveSaving.x' = 'impActiveSaving0507',
                                                          'impActiveSaving.y' = 'impActiveSaving0709'))

#07-09, 09-11
x07_09_11 <- merge(x0709, x0911, by='uniqueID', all=FALSE)
saving07_09_11 <-x07_09_11[c('impActiveSaving.x', 'impActiveSaving.y')]
saving07_09_11_sorted <- saving07_09_11[order(saving07_09_11$'impActiveSaving.x'),]
saving07_09_11_sorted <- rename( saving07_09_11_sorted, c('impActiveSaving.x' = 'impActiveSaving0709',
                                                          'impActiveSaving.y' = 'impActiveSaving0911'))

#09-11, 11-13
x09_11_13 <- merge(x0911, x1113, by='uniqueID', all=FALSE)
saving09_11_13 <-x09_11_13[c('impActiveSaving.x', 'impActiveSaving.y')]
saving09_11_13_sorted <- saving09_11_13[order(saving09_11_13$'impActiveSaving.x'),]
saving09_11_13_sorted <- rename( saving09_11_13_sorted, c('impActiveSaving.x' = 'impActiveSaving0911',
                                                          'impActiveSaving.y' = 'impActiveSaving1113'))

#weight
y84_89_94 <- 
  svydesign( 
    ~primarySamplingUnit.y , 
    strata = ~stratification.y , 
    data = x84_89_94 , 
    weights = ~weight , 
    nest = TRUE 
  )


svyplot(log(impActiveSaving.x)~log(impActiveSaving.y),design=y84_89_94 , style="transparent",
         pch=19,alpha=c(0,0.5) )
#plot
plot(saving84_89_94_sorted)
plot(saving89_94_99_sorted)
plot(saving94_99_01_sorted)
plot(saving99_01_03_sorted)
plot(saving01_03_05_sorted)
plot(saving05_07_09_sorted)
plot(saving07_09_11_sorted)
plot(saving09_11_13_sorted)

# plot only savers

#84-89, 89-94
# x8489_94_pos <- subset(x8489_94, x8489_94$'activeSaving.x'>0 & x8489_94$'activeSaving.y'>0 )
# saving84_89_94_pos <-x8489_94_pos[c('activeSaving.x', 'activeSaving.y')]
# saving84_89_94_pos_sorted <- saving84_89_94_pos[order(saving84_89_94_pos$'activeSaving.x'),]
x84_89_94_pos <- subset(x84_89_94, x84_89_94$'impActiveSaving.x'>0 & x84_89_94$'impActiveSaving.y'>0 )
saving84_89_94_pos <-x84_89_94_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving84_89_94_pos_sorted <- saving84_89_94_pos[order(saving84_89_94_pos$'impActiveSaving.x'),]
saving84_89_94_pos_sorted <- rename( saving84_89_94_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving8489pos',
                                                          'impActiveSaving.y' = 'impActiveSaving8994pos'))

#89-94, 94-99
x89_94_99_pos <- subset(x89_94_99, x89_94_99$'impActiveSaving.x'>0 & x89_94_99$'impActiveSaving.y'>0 )
saving89_94_99_pos <-x89_94_99_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving89_94_99_pos_sorted <- saving89_94_99_pos[order(saving89_94_99_pos$'impActiveSaving.x'),]
saving89_94_99_pos_sorted <- rename( saving89_94_99_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving8994pos',
                                                                  'impActiveSaving.y' = 'impActiveSaving9499pos'))

#94-99, 99-01
x94_99_01_pos <- subset(x94_99_01, x94_99_01$'impActiveSaving.x'>0 & x94_99_01$'impActiveSaving.y'>0 )
saving94_99_01_pos <-x94_99_01_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving94_99_01_pos_sorted <- saving94_99_01_pos[order(saving94_99_01_pos$'impActiveSaving.x'),]
saving94_99_01_pos_sorted <- rename( saving94_99_01_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving9499pos',
                                                                  'impActiveSaving.y' = 'impActiveSaving9901pos'))

#99-01, 01-03
x99_01_03_pos <- subset(x99_01_03, x99_01_03$'impActiveSaving.x'>0 & x99_01_03$'impActiveSaving.y'>0 )
saving99_01_03_pos <-x99_01_03_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving99_01_03_pos_sorted <- saving99_01_03_pos[order(saving99_01_03_pos$'impActiveSaving.x'),]
saving99_01_03_pos_sorted <- rename( saving99_01_03_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving9901pos',
                                                                  'impActiveSaving.y' = 'impActiveSaving0103pos'))
#01-03, 03-05
x01_03_05_pos <- subset(x01_03_05, x01_03_05$'impActiveSaving.x'>0 & x01_03_05$'impActiveSaving.y'>0 )
saving01_03_05_pos <-x01_03_05_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving01_03_05_pos_sorted <- saving01_03_05_pos[order(saving01_03_05_pos$'impActiveSaving.x'),]
saving01_03_05_pos_sorted <- rename( saving01_03_05_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving0103pos',
                                                                  'impActiveSaving.y' = 'impActiveSaving0305pos'))
#03-05, 05-07
x03_05_07_pos <- subset(x03_05_07, x03_05_07$'impActiveSaving.x'>0 & x03_05_07$'impActiveSaving.y'>0 )
saving03_05_07_pos <-x03_05_07_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving03_05_07_pos_sorted <- saving03_05_07_pos[order(saving03_05_07_pos$'impActiveSaving.x'),]
saving03_05_07_pos_sorted <- rename( saving03_05_07_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving0305pos',
                                                                  'impActiveSaving.y' = 'impActiveSaving0507pos'))
#05-07, 07-09
x05_07_09_pos <- subset(x05_07_09, x05_07_09$'impActiveSaving.x'>0 & x05_07_09$'impActiveSaving.y'>0 )
saving05_07_09_pos <-x05_07_09_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving05_07_09_pos_sorted <- saving05_07_09_pos[order(saving05_07_09_pos$'impActiveSaving.x'),]
saving05_07_09_pos_sorted <- rename( saving05_07_09_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving0507pos',
                                                                  'impActiveSaving.y' = 'impActiveSaving0709pos'))
#07-09, 09-11
x07_09_11_pos <- subset(x07_09_11, x07_09_11$'impActiveSaving.x'>0 & x07_09_11$'impActiveSaving.y'>0 )
saving07_09_11_pos <-x07_09_11_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving07_09_11_pos_sorted <- saving07_09_11_pos[order(saving07_09_11_pos$'impActiveSaving.x'),]
saving07_09_11_pos_sorted <- rename( saving07_09_11_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving0709pos',
                                                                  'impActiveSaving.y' = 'impActiveSaving0911pos'))
#07-09, 09-11
x07_09_11_pos <- subset(x07_09_11, x07_09_11$'impActiveSaving.x'>0 & x07_09_11$'impActiveSaving.y'>0 )
saving07_09_11_pos <-x07_09_11_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving07_09_11_pos_sorted <- saving07_09_11_pos[order(saving07_09_11_pos$'impActiveSaving.x'),]
saving07_09_11_pos_sorted <- rename( saving07_09_11_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving0709pos',
                                                                  'impActiveSaving.y' = 'impActiveSaving0911pos'))
#09-11, 11-13
x09_11_13_pos <- subset(x09_11_13, x09_11_13$'impActiveSaving.x'>0 & x09_11_13$'impActiveSaving.y'>0 )
saving09_11_13_pos <-x09_11_13_pos[c('impActiveSaving.x', 'impActiveSaving.y')]
saving09_11_13_pos_sorted <- saving09_11_13_pos[order(saving09_11_13_pos$'impActiveSaving.x'),]
saving09_11_13_pos_sorted <- rename( saving09_11_13_pos_sorted, c('impActiveSaving.x' = 'impActiveSaving0911pos',
                                                                  'impActiveSaving.y' = 'impActiveSaving1113pos'))
#plot
par(mfrow = c(3, 3))
plot(log(saving84_89_94_pos_sorted))
mtext('84_89_94', side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(log(saving89_94_99_pos_sorted))
mtext('89_94_99', side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(log(saving94_99_01_pos_sorted))
mtext('94_99_01', side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(log(saving99_01_03_pos_sorted))
mtext('99_01_03', side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(log(saving01_03_05_pos_sorted))
mtext('01_03_05', side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(log(saving03_05_07_pos_sorted))
mtext('03_05_07', side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(log(saving05_07_09_pos_sorted))
mtext('05_07_09', side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(log(saving07_09_11_pos_sorted))
mtext('07_09_11', side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(log(saving09_11_13_pos_sorted))
mtext('09_11_13', side = 3, line = -1, adj = 0.1, cex = 0.6)


#plotting the first sd of the distribution of active saving

x84_89_dist <- subset(saving84_89_94$'impActiveSaving.x', saving84_89_94$'impActiveSaving.x'<sd(saving84_89_94$'impActiveSaving.x') & 
                                                                                           saving84_89_94$'impActiveSaving.x'>(-1*sd(saving84_89_94$'impActiveSaving.x') ))
x89_94_dist <- subset(saving84_89_94$'impActiveSaving.y', saving84_89_94$'impActiveSaving.y'<sd(saving84_89_94$'impActiveSaving.y') & 
                        saving84_89_94$'impActiveSaving.y'>(-1*sd(saving84_89_94$'impActiveSaving.y') ))

x94_99_dist <- subset(saving94_99_01$'impActiveSaving.x', saving94_99_01$'impActiveSaving.x'<sd(saving94_99_01$'impActiveSaving.x') & 
                        saving94_99_01$'impActiveSaving.x'>(-1*sd(saving94_99_01$'impActiveSaving.x') ))
x99_01_dist <- subset(saving94_99_01$'impActiveSaving.y', saving94_99_01$'impActiveSaving.y'<sd(saving94_99_01$'impActiveSaving.y') & 
                        saving94_99_01$'impActiveSaving.y'>(-1*sd(saving94_99_01$'impActiveSaving.y') ))

x01_03_dist <- subset(saving01_03_05$'impActiveSaving.x', saving01_03_05$'impActiveSaving.x'<sd(saving01_03_05$'impActiveSaving.x') & 
                        saving01_03_05$'impActiveSaving.x'>(-1*sd(saving01_03_05$'impActiveSaving.x') ))
x03_05_dist <- subset(saving01_03_05$'impActiveSaving.y', saving01_03_05$'impActiveSaving.y'<sd(saving01_03_05$'impActiveSaving.y') & 
                        saving01_03_05$'impActiveSaving.y'>(-1*sd(saving01_03_05$'impActiveSaving.y') ))

x05_07_dist <- subset(saving03_05_07$'impActiveSaving.x', saving03_05_07$'impActiveSaving.x'<sd(saving03_05_07$'impActiveSaving.x') & 
                        saving03_05_07$'impActiveSaving.x'>(-1*sd(saving03_05_07$'impActiveSaving.x') ))
x07_09_dist <- subset(saving03_05_07$'impActiveSaving.y', saving03_05_07$'impActiveSaving.y'<sd(saving03_05_07$'impActiveSaving.y') & 
                        saving03_05_07$'impActiveSaving.y'>(-1*sd(saving03_05_07$'impActiveSaving.y') ))

x09_11_dist <- subset(saving09_11_13$'impActiveSaving.x', saving09_11_13$'impActiveSaving.x'<sd(saving09_11_13$'impActiveSaving.x') & 
                        saving09_11_13$'impActiveSaving.x'>(-1*sd(saving09_11_13$'impActiveSaving.x') ))
x11_13_dist <- subset(saving09_11_13$'impActiveSaving.y', saving09_11_13$'impActiveSaving.y'<sd(saving09_11_13$'impActiveSaving.y') & 
                        saving09_11_13$'impActiveSaving.y'>(-1*sd(saving09_11_13$'impActiveSaving.y') ))
par(mfrow = c(3, 3))
hist(x84_89_dist)
hist(x89_94_dist)
hist(x94_99_dist)
hist(x99_01_dist)
hist(x01_03_dist)
hist(x03_05_dist)
hist(x05_07_dist)
hist(x07_09_dist)
hist(x09_11_dist)
hist(x11_13_dist)
#normality test: null=normal




# survey design
wealth <-svydesign(id=~primarySamplingUnit, strat=~stratification, weights=~weight, data=x8489, nest=TRUE)


#plotting the wealth distribution
wealth84 <-x8489$'impWealthWE84'

svyhist(~impWealthWE84,subset(wealth, weightYear=='longWeight84'),main="", col="grey80", xlab="Wealth 84", breaks=200)
svyplot(iron~trnsfern,style="grayhex",dhanes,xlab="Transferrin",ylab="Iron",legend=2)
      
      svyhist(~BPXSAR, subset(nhanes,RIDAGEYR>20& BPXSAR>0),main="", col="grey80", xlab="Systolic BP (mmHg)")
      lines(svysmooth(~BPXSAR, bandwidth=5, subset(nhanes,RIDAGEYR>20& BPXSAR>0)),lwd=2)
#histogram
hist(wealth84, breaks=200)
#density estimate
plot(density(wealth84),main="Density estimate of data")
test <- subset(wealth84, wealth84>0)
plot(density(test),main="Density estimate of data")
# cdf estimate
plot(ecdf(wealth84),main="Empirical cumulative distribution function")
#standardise & normal q-q plot
z<-(wealth84-mean(wealth84))/sd(wealth84) ## standardized data
qqnorm(z) ## drawing the QQplot
abline(0,1) ## drawing a 45-degree reference line

#weibull q-q
z.wei<-rweibull(n=5273,shape=3, scale=1) ## theorical quantiles from a Weibull population with known paramters shape=2 e scale=1
qqplot(z.wei,z,main="QQ-plot distr. Weibull") ## QQ-plot
abline(0,1) ## a 45-degree reference line is plotted





shapiro.test(saving84_89_94$'impActiveSaving.x')