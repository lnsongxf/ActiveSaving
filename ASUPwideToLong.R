# takes the subset of PSID data from wide to long format
# need to run load.R and ASUPconstructFamPan.R first


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

# hhHead variables - sequenceNum ##################################################################################################################################################################
fP_sequenceNum <- familyPanel[c('uniqueID',
                                     'sequenceNum84',
                                     'sequenceNum89',
                                     'sequenceNum94',
                                     'sequenceNum99',
                                     'sequenceNum01',
                                     'sequenceNum03',
                                     'sequenceNum05',
                                     'sequenceNum07',
                                     'sequenceNum09',
                                     'sequenceNum11',
                                     'sequenceNum13' )]



# Specify id.vars: the variables to keep but not split apart on
fP_sequenceNum <- melt(fP_sequenceNum, id.vars=c("uniqueID"))
fP_sequenceNum$year <- ifelse(fP_sequenceNum$variable=='sequenceNum84',1984,
                                   ifelse(fP_sequenceNum$variable=='sequenceNum89',1989,
                                          ifelse(fP_sequenceNum$variable=='sequenceNum94',1994,
                                                 ifelse(fP_sequenceNum$variable=='sequenceNum99',1999,
                                                        ifelse(fP_sequenceNum$variable=='sequenceNum01',2001,
                                                               ifelse(fP_sequenceNum$variable=='sequenceNum03',2003,
                                                                      ifelse(fP_sequenceNum$variable=='sequenceNum05',2005,
                                                                             ifelse(fP_sequenceNum$variable=='sequenceNum07',2007,
                                                                                    ifelse(fP_sequenceNum$variable=='sequenceNum09',2009,
                                                                                           ifelse(fP_sequenceNum$variable=='sequenceNum11',2011,2013))))))))))

fP_sequenceNum <- rename(fP_sequenceNum, c('value' = 'sequenceNum'))
fP_sequenceNum <-subset(fP_sequenceNum,select=-c(variable))

# hhHead variables - hhRelStatus ##################################################################################################################################################################
fP_hhRelStatus <- familyPanel[c('uniqueID',
                                     'hhRelStatus84',
                                     'hhRelStatus89',
                                     'hhRelStatus94',
                                     'hhRelStatus99',
                                     'hhRelStatus01',
                                     'hhRelStatus03',
                                     'hhRelStatus05',
                                     'hhRelStatus07',
                                     'hhRelStatus09',
                                     'hhRelStatus11',
                                     'hhRelStatus13' )]



# Specify id.vars: the variables to keep but not split apart on
fP_hhRelStatus <- melt(fP_hhRelStatus, id.vars=c("uniqueID"))
fP_hhRelStatus$year <- ifelse(fP_hhRelStatus$variable=='hhRelStatus84',1984,
                                   ifelse(fP_hhRelStatus$variable=='hhRelStatus89',1989,
                                          ifelse(fP_hhRelStatus$variable=='hhRelStatus94',1994,
                                                 ifelse(fP_hhRelStatus$variable=='hhRelStatus99',1999,
                                                        ifelse(fP_hhRelStatus$variable=='hhRelStatus01',2001,
                                                               ifelse(fP_hhRelStatus$variable=='hhRelStatus03',2003,
                                                                      ifelse(fP_hhRelStatus$variable=='hhRelStatus05',2005,
                                                                             ifelse(fP_hhRelStatus$variable=='hhRelStatus07',2007,
                                                                                    ifelse(fP_hhRelStatus$variable=='hhRelStatus09',2009,
                                                                                           ifelse(fP_hhRelStatus$variable=='hhRelStatus11',2011,2013))))))))))

fP_hhRelStatus <- rename(fP_hhRelStatus, c('value' = 'hhRelStatus'))
fP_hhRelStatus <-subset(fP_hhRelStatus,select=-c(variable))



# merge all of these files together

familyPanel <- merge(fP_ID, fP_longWeight, by='uniqueID')
familyPanel <- merge(familyPanel, fP_impWealthWOE, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_age, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_famIncome, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_numInFam, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_empStatus, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_highestSchoolLev, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_sequenceNum, by = c('uniqueID', 'year'))
familyPanel <- merge(familyPanel, fP_hhRelStatus, by = c('uniqueID', 'year'))
familyPanel <-merge(familyPanel,fP_activeSaving, by=c('uniqueID', 'year'), all=TRUE)
familyPanel <-merge(familyPanel,fP_PSIDas, by=c('uniqueID', 'year'), all=TRUE)

# make one hhHead identifier - 1 if hhHead, 0 otherwise
familyPanel$hhHead <- ifelse((familyPanel$sequenceNum<=20 & familyPanel$hhRelStatus==10),1,0)
familyPanel <- subset(familyPanel, select=-c(sequenceNum, hhRelStatus))  

#save(familyPanel, file='famPanel.R')
#load(file='famPanel.R')
rm(fP_activeSaving, fP_age, fP_empStatus, fP_famIncome, fP_highestSchoolLev, fP_ID,
   fP_impWealthWOE, fP_longWeight, fP_numInFam, fP_PSIDas, fP_hhRelStatus, fP_sequenceNum)


