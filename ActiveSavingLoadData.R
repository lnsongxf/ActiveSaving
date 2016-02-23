#Load the PSID data necessary to compute active savings, combine with the wealth supplement, rename variables.
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data

# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN

# load the individual cross-year file
load( "ind.rda" )

# limit the file to only the variables needed
ind.KeepVars <-
  c( 
    'one' ,        # column with all ones
    'er30001' , 	# 1968 interview number
    'er30002' ,   # 1968 person number
    'er31997' ,		# primary sampling unit variable 
    'er31996' ,		# stratification variable
    'er30429',    # interview number, 1984 
    'er30606',    # interview number, 1989  
    'er33101',    # interview number, 1994
    'er33501' , 	# interview number, 1999
    'er33601',    # interview number, 2001
    'er33701',    # interview number, 2003
    'er33801',    # interview number, 2005
    'er33901',    # interview number, 2007
    'er34001',    # interview number, 2009
    'er34101',    # interview number, 2011
    'er34201',    # interview number, 2013
    'er30430',    # household head, 1984
    'er30607',    # household head, 1989
    'er33102',    # household head, 1994 
    'er33502',  	# household head, 1999
    'er33602',    # household head, 2001 
    'er33702',    # household head, 2003
    'er33802',    # household head, 2005
    'er33902',    # household head, 2007
    'er34002',    # household head, 2009
    'er34102',    # household head, 2011
    'er34202',    # household head, 2013
    'er32000',		# sex
    'er30441',  	# employment status in 1984
    'er30616',  	# employment status in 1989
    'er33111',  	# employment status in 1994
    'er33512',    # employment status in 1999
    'er33612',    # employment status in 2001
    'er33712',    # employment status in 2003
    'er33813',    # employment status in 2005
    'er33913',    # employment status in 2007
    'er34016',    # employment status in 2009
    'er34116',    # employment status in 2011
    'er34216',    # employment status in 2013
    'er30402',  	# age in 1984
    'er30609',  	# age in 1989
    'er33104',  	# age in 1994
    'er33504',  	# age in 1999
    'er33604',  	# age in 2001
    'er33704',  	# age in 2003
    'er33804',  	# age in 2005
    'er33904',  	# age in 2007
    'er34004',  	# age in 2009
    'er34104',  	# age in 2011
    'er34204',  	# age in 2013
    'er30431',  	# household relationship status, 1984
    'er30608',  	# household relationship status, 1989
    'er33103',  	# household relationship status, 1994
    'er33503',  	# household relationship status, 1999 
    'er33603',  	# household relationship status, 2001
    'er33703',  	# household relationship status, 2003
    'er33803',  	# household relationship status, 2005
    'er33903',  	# household relationship status, 2007
    'er34003',  	# household relationship status, 2009
    'er34103',  	# household relationship status, 2011
    'er34203',  	# household relationship status, 2013
    'er33222',		# current school level
    'er33227',		# highest school level completed
    'er30462',    # 1984 longitudinal weight
    'er30641',    # 1989 longitudinal weight
    'er33119',		# 1994 longitudinal weight
    'er33546',		# 1999 longitudinal weight
    'er33637',  	# 2001 longitudinal weight
    'er33740',  	# 2003 longitudinal weight
    'er33848',  	# 2005 longitudinal weight
    'er33950',  	# 2007 longitudinal weight 
    'er34045',  	# 2009 longitudinal weight
    'er34154',  	# 2011 longitudinal weight
    'er34268'  	  # 2013 longitudinal weight
  )

# create a "skinny"	data.frame object that only contains the
# columns you need for this analysis,
# specified in the KeepVars character vector
w <- ind[ , ind.KeepVars ]


# rename all the variables something useful
w <- rename(w, c("er30001" = "1968IntNum",
                 'er30002' = "1968PersonNum",
                 'er31997' = "primarySamplingUnit",
                 'er31996' = "stratification",
                 'er30429' = "intNum84", 
                 'er30606' = "intNum89",  
                 'er33101' = "intNum94",
                 'er33501' = "intNum99",
                 'er33601' = "intNum01",
                 'er33701' = "intNum03",
                 'er33801' = "intNum05",
                 'er33901' = "intNum07",
                 'er34001' = "intNum09",
                 'er34101' = "intNum11",
                 'er34201' = "intNum13",
                 'er30430' = "sequenceNum84",
                 'er30607' = "sequenceNum89",
                 'er33102' = "sequenceNum94", 
                 'er33502' = "sequenceNum99",
                 'er33602' = "sequenceNum01",
                 'er33702' = "sequenceNum03",
                 'er33802' = "sequenceNum05",
                 'er33902' = "sequenceNum07",
                 'er34002' = "sequenceNum09",
                 'er34102' = "sequenceNum11",
                 'er34202' = "sequenceNum13",
                 'er32000' = "sex",
                 'er30441' = "empStatus84",
                 'er30616' = "empStatus89",
                 'er33111' = "empStatus94",
                 'er33512' = "empStatus99",
                 'er33612' = "empStatus01",
                 'er33712' = "empStatus03",
                 'er33813' = "empStatus05",
                 'er33913' = "empStatus07",
                 'er34016' = "empStatus09",
                 'er34116' = "empStatus11",
                 'er34216' = "empStatus13",
                 'er30402' = "age84",
                 'er30609' = "age89",
                 'er33104' = "age94",
                 'er33504' = "age99",
                 'er33604' = "age01",
                 'er33704' = "age03",
                 'er33804' = "age05",
                 'er33904' = "age07",
                 'er34004' = "age09",
                 'er34104' = "age11",
                 'er34204' = "age13",
                 'er30431' = "hhRelStatus84",
                 'er30608' = "hhRelStatus89",
                 'er33103' = "hhRelStatus94",
                 'er33503' = "hhRelStatus99", 
                 'er33603' = "hhRelStatus01",
                 'er33703' = "hhRelStatus03",
                 'er33803' = "hhRelStatus05",
                 'er33903' = "hhRelStatus07",
                 'er34003' = "hhRelStatus09",
                 'er34103' = "hhRelStatus11",
                 'er34203' = "hhRelStatus13",
                 'er33222' = "currSchoolLev",
                 'er33227' = "highestSchoolLev",
                 'er30462' = "longWeight84",
                 'er30641' = "longWeight89",
                 'er33119' = "longWeight94",
                 'er33546' = "longWeight99",
                 'er33637' = "longWeight01",
                 'er33740' = "longWeight03",
                 'er33848' = "longWeight05",
                 'er33950' = "longWeight07",
                 'er34045' = "longWeight09",
                 'er34154' = "longWeight11",
                 'er34268' = "longWeight13"
))

# remove the original data.frame object then free up memory
rm( ind ) ; gc()

# Describe which variables to keep from the family level (f) and wealth supplement files (w)
# 1984
f84.KeepVars <-
  c(
    'v10002',   # family interview number
    'v10908',   # farm/business
    'v10913',   # stocks
    'v10899',   # other real estate
    'v10903',   # vehicles
    'v10020',   # mortgage debt
    'v10923',   # other assets
    'v10447',   # moved?
    'v10018',   # house value
    'v10418',   # number in family
    'v11022',   # family income
    'v10003'    # state
  )


w84.KeepVars <-
  c(
    'V10002',  # family interview number
    'S105',    # checking/saving account
    'S107',    # other debt
    'S120',    # home equity
    'S117'     # imputed wealth with equity
  )

# 1989
f89.KeepVars <-
  c( 
    'v16302',   # family interview number
    'v17323',   # farm/business
    'v17326',   # stocks
    'v17318',   # other real estate
    'v17320',   # vehicles
    'v16326',   # mortage debt
    'v17332',   # other assets
    'v16649',   # moved?
    'v16324',   # house value
    'v16630',   # number in family
    'v17533',   # family income
    'v16303'    # state
    
  )
w89.KeepVars <-
  c(
    'V16302',  # family interview number
    'S205',    # checking/saving account
    'S207',    # other debt
    'S220',    # home equity
    'S217'     # imputed wealth with equity
  )

# 1994
f94.KeepVars <-
  c(
    'er2002',   # family interview number
    'er3731',   # farm/business
    'er3736',   # stocks
    'er3722',   # other real estate
    'er3726',   # vehicles 
    'er2037',   # mortgage debt
    'er3748',   # other assets
    'er2062',   # moved?
    'er2033',   # house value
    'er2006',   # number in family
    'er4153',   # family income
    'er4156'    # state
    
  )
w94.KeepVars <-
  c(
    'ER2002',  # family interview number
    'S305',    # checking/saving account
    'S307',    # other debt
    'S320',    # home equity
    'S317'     # imputed wealth with equity
  )


# 1999
f99.KeepVars <-
  c( 
    'er13002',   # family interview number
    'er15002',   # farm/business
    'er15007',   # stocks
    'er14993',   # other real estate
    'er14997',   # vehicles
    'er13047',   # mortgage debt
    'er15026',   # other assets
    'er13077',   # moved?
    'er13041',   # house value
    'er13009',   # number in family
    'er16462',   # family income
    'er13004'     # state
    
  )
w99.KeepVars <-
  c(
    'ER13002', # family interview number
    'S405',    # checking/saving account
    'S407',    # other debt
    'S420',    # home equity
    'S417'     # imputed wealth with equity
  )

# 2001
f01.KeepVars <-
  c(
    'er17002',   # family interview number 
    'er19198',   # farm/business
    'er19203',   # stocks
    'er19189',   # other real estate
    'er19193',   # vehicles
    'er17052',   # mortgage debt
    'er19222',   # other assets
    'er17088',   # moved?
    'er17044',   # house value
    'er17012',   # number in family
    'er20456',   # family income
    'er17004'    # state
    
  )
w01.KeepVars <-
  c(
    'ER17002', # family interview number
    'S505',    # checking/saving account
    'S507',    # other debt
    'S520',    # home equity
    'S517'     # imputed wealth with equity
  )

# 2003
f03.KeepVars <-
  c(
    'er21002',   # family interview number
    'er22563',   # farm/business
    'er22568',   # stocks
    'er22554',   # other real estate
    'er22558',   # vehicles
    'er21051',   # mortgage debt 
    'er22617',   # other assets 
    'er21117',   # moved?
    'er21043',   # house value
    'er21016',   # number in family
    'er24099',   # family income
    'er21003'    # state
  )
w03.KeepVars <-
  c(
    'ER21002', # family interview number
    'S605',    # checking/saving account
    'S607',    # other debt
    'S620',    # home equity
    'S617'     # imputed wealth with equity
  )

# 2005
f05.KeepVars <-
  c(
    'er25002',   # family interview number
    'er26544',   # farm/business
    'er26549',   # stocks
    'er26535',   # other real estate
    'er26539',   # vehicles
    'er25042',   # mortgage debt
    'er26598',   # other assets
    'er25098',   # moved?
    'er25029',   # house value
    'er25016',   # number in family
    'er28037',   # family income
    'er25003'    # state
    
  )

w05.KeepVars <-
  c(
    'ER25002', # family interview number
    'S705',    # checking/saving account
    'S707',    # other debt
    'S720',    # home equity
    'S717'     # imputed wealth with equity
  )

# 2007
f07.KeepVars <-
  c(
    'er36002',   # family interview number
    'er37562',   # farm/business 
    'er37567',   # stocks
    'er37553',   # other real estate
    'er37557',   # vehicles
    'er36042',   # mortgage debt
    'er37616',   # other assets
    'er36103',   # moved?
    'er36029',   # house value
    'er36016',   # number in family
    'er41027',   # family income
    'er36003'    # state
    
  )
w07.KeepVars <-
  c(
    'ER36002', # family interview number
    'S805',    # value of checking/saving account
    'S807',    # other debt
    'S820',    # home equity
    'S817'     # imputed wealth with equity
  )

# 2009
f09.KeepVars <-
  c(
    'er42002',   # family interview number    
    'er43553',   # farm/business
    'er46942',   # checking/savings
    'er43558',   # stocks
    'er43544',   # other real estate 
    'er46966',   # home equity
    'er43548',   # vehicles
    'er42043',   # mortgage debt
    'er43607',   # other assets
    'er42132',   # moved?
    'er43612',   # other debt
    'er42030',   # house value
    'er46970',   # imputed wealth with equity
    'er42016',   # number in family
    'er46935',   # family income
    'er42003'    # state
    
  )
# 2011
f11.KeepVars <-
  c(
    'er47302',   # family interview number
    'er48878',   # farm/business
    'er52350',   # checking/savings
    'er48883',   # stocks
    'er48869',   # other real estate
    'er52390',   # home equity
    'er48873',   # vehicles
    'er47348',   # mortgage debt
    'er48932',   # other assets
    'er47440',   # moved?
    'er47330',   # house value
    'er52372',   # credit card debt
    'er48945',   # student debt
    'er52380',   # medical debt
    'er52384',   # legal debt
    'er52376',   # loans from relatives 
    'er52394',   # imputed wealth with equity
    'er47316',   # number in family
    'er52343',   # family income
    'er47303'    # state
    
  )
# 2013
f13.KeepVars <-
  c(
    'er53002',   # family interview number
    'er54625',   # farm/business
    'er58161',   # checking/savings
    'er54634',   # stocks
    'er58165',   # other real estate
    'er58207',   # home equity
    'er54620',   # vehicles
    'er53048',   # mortgage debt
    'er54682',   # other assets
    'er53140',   # moved?
    'er53030',   # house value
    'er58185',   # credit card debt
    'er54697',   # student debt
    'er58193',   # medical debt
    'er58197',   # legal debt
    'er58189',   # loans from relatives
    'er58211',   # imputed wealth with equity
    'er53016',   # number in family
    'er58152',   # family income
    'er53003'    # state
  )

# extract data 
years <- c('1984', '1989', '1994', '1999', '2001', '2003', '2005', '2007', '2009', '2011', '2013')
for(i in years)
{
  #from family level files
  load(paste("fam",i,".rda",sep=""))
  assign(paste("f",substr(i, 3, 4), sep=""), eval(as.name(paste("fam",i,sep="")))[ , eval(as.name(paste("f",substr(i, 3, 4),".KeepVars",sep=""))) ])
  eval(call("rm",paste("fam",i,sep="")))
  if(eval(i)<2009){
  #from extra wealth supplement files (not included in main release from '84-'07)
  assign(paste("wealth",i, sep=""), read.csv(paste(i,"WealthSupplement.csv", sep="")))
  assign(paste("w",substr(i,3,4),sep=""), eval(as.name(paste("wealth",i,sep="")))[ , eval(as.name(paste("w",substr(i, 3, 4),".KeepVars",sep=""))) ])
  eval(call("rm",paste("wealth",i,sep=""))); gc()  
  } 
}
rm(i)

# merge the family and wealth supplement files
# using the interview number field available in both tables (NB. the excel import is in upper case!)
f84 <- merge( f84 , w84 , by.x = 'v10002', by.y = 'V10002')
f89 <- merge( f89 , w89 , by.x = 'v16302', by.y = 'V16302')
f94 <- merge( f94 , w94 , by.x = 'er2002', by.y = 'ER2002')
f99 <- merge( f99 , w99 , by.x = 'er13002', by.y = 'ER13002')
f01 <- merge( f01 , w01 , by.x = 'er17002', by.y = 'ER17002')
f03 <- merge( f03 , w03 , by.x = 'er21002', by.y = 'ER21002')
f05 <- merge( f05 , w05 , by.x = 'er25002', by.y = 'ER25002')
f07 <- merge( f07 , w07 , by.x = 'er36002', by.y = 'ER36002')

# get rid of supplemental data, we don't need them anymore
rm(w84, w89, w94, w99, w01, w03, w05, w07) ; gc()



# merge the family and individual-level files,
# using the interview number field available in both tables

z84 <- merge( f84 , w , by.x = 'v10002' , by.y = 'intNum84')
z89 <- merge( f89 , w , by.x = 'v16302' , by.y = 'intNum89')
z94 <- merge( f94 , w , by.x = 'er2002' , by.y = 'intNum94')
z99 <- merge( f99 , w , by.x = 'er13002' , by.y = 'intNum99')
z01 <- merge( f01 , w , by.x = 'er17002' , by.y = 'intNum01')
z03 <- merge( f03 , w , by.x = 'er21002' , by.y = 'intNum03')
z05 <- merge( f05 , w , by.x = 'er25002' , by.y = 'intNum05')
z07 <- merge( f07 , w , by.x = 'er36002' , by.y = 'intNum07')
z09 <- merge( f09 , w , by.x = 'er42002' , by.y = 'intNum09')
z11 <- merge( f11 , w , by.x = 'er47302' , by.y = 'intNum11')
z13 <- merge( f13 , w , by.x = 'er53002' , by.y = 'intNum13')

# get rid of separate family and individual data, we don't need them anymore
rm(f84, f89, f94, f99, f01, f03, f05, f07, f09, f11, f13, w)
rm(f84.KeepVars, f89.KeepVars, f94.KeepVars, f99.KeepVars, f01.KeepVars,
   f03.KeepVars, f05.KeepVars, f07.KeepVars, f09.KeepVars, f11.KeepVars, f13.KeepVars,
   w84.KeepVars, w89.KeepVars, w94.KeepVars, w99.KeepVars, w01.KeepVars, w03.KeepVars,
   w05.KeepVars, w07.KeepVars, ind.KeepVars) ; gc()

# get rid of repeated data from individual level file - keep only those pertinent to the year file
for(i in years){
  shortYeari <- substr(i,3,4)
  for(j in years){
    shortYearj <- substr(j,3,4)
    if(shortYearj!=shortYeari){
      assign(paste("z", shortYeari, sep=''), subset(eval(as.name(paste("z", shortYeari, sep=''))), select=-c(eval(as.name(paste('intNum', shortYearj, sep=''))), 
                                                                                                             eval(as.name(paste('sequenceNum', shortYearj, sep=''))),
                                                                                                             eval(as.name(paste('empStatus', shortYearj, sep=''))),
                                                                                                             eval(as.name(paste('age', shortYearj, sep=''))),
                                                                                                             eval(as.name(paste('hhRelStatus', shortYearj, sep=''))),
                                                                                                             eval(as.name(paste('longWeight', shortYearj, sep='')))
                                                                                                             ))) 
    } 
  }
}

# rename remaining variables something useful
z84 <- rename( z84, c('v10002' = 'intNum',
                      'v10908' = 'farmBusiness',
                      'v10913' = 'stocks',
                      'v10899' = 'othRealEstate',
                      'v10903' = 'vehicles',
                      'v10020' = 'mortgageDebt',
                      'v10923' = 'othAssets',
                      'v10447' = 'wtrMoved',
                      'v10018' = 'houseValue',
                      'S105' = 'checkingAccount',
                      'S107'= 'othDebt',
                      'S120'= 'homeEquity',
                      'S117'= 'impWealthWE',
                      'v10418' = 'numInFam',
                      'v11022' = 'famIncome',
                      'v10003' = 'state'))

z89 <- rename( z89, c('v16302' = 'intNum',
                      'v17323' = 'farmBusiness',
                      'v17326' = 'stocks',
                      'v17318' = 'othRealEstate',
                      'v17320' = 'vehicles',
                      'v16326' = 'mortgageDebt',
                      'v17332' = 'othAssets',
                      'v16649' = 'wtrMoved',
                      'v16324' = 'houseValue',
                      'S205' = 'checkingAccount',
                      'S207' = 'othDebt',
                      'S220' = 'homeEquity',
                      'S217'= 'impWealthWE',
                      'v16630' = 'numInFam',
                      'v17533' = 'famIncome',
                      'v16303' = 'state'))

z94 <- rename(z94, c('er2002' = 'intNum',
                     'er3731' = 'farmBusiness',
                     'er3736' = 'stocks',
                     'er3722' = 'othRealEstate',
                     'er3726' = 'vehicles',
                     'er2037' = 'mortgageDebt',
                     'er3748' = 'othAssets',
                     'er2062' = 'wtrMoved',
                     'er2033' = 'houseValue',
                     'S305' = 'checkingAccount',
                     'S307' = 'othDebt',
                     'S320' = 'homeEquity',
                     'S317'= 'impWealthWE',
                     'er2006' = 'numInFam',
                     'er4153' = 'famIncome',
                     'er4156' = 'state'))

z99 <- rename( z99, c('er13002' = 'intNum',
                      'er15002' = 'farmBusiness',
                      'er15007' = 'stocks',
                      'er14993' = 'othRealEstate',
                      'er14997' = 'vehicles',
                      'er13047' = 'mortgageDebt',
                      'er15026' = 'othAssets',
                      'er13077' = 'wtrMoved',
                      'er13041' = 'houseValue',
                      'S405' = 'checkingAccount',
                      'S407' = 'othDebt',
                      'S420' = 'homeEquity',
                      'S417'= 'impWealthWE',
                      'er13009' = 'numInFam',
                      'er16462' = 'famIncome',
                      'er13004' = 'state'))

z01 <- rename(z01, c('er17002' = 'intNum', 
                     'er19198' = 'farmBusiness',
                     'er19203' = 'stocks',
                     'er19189' = 'othRealEstate',
                     'er19193' = 'vehicles',
                     'er17052' = 'mortgageDebt',
                     'er19222' = 'othAssets',
                     'er17088' = 'wtrMoved',
                     'er17044' = 'houseValue',
                     'S505' = 'checkingAccount',
                     'S507' = 'othDebt'
                     'S520' = 'homeEquity',
                     'S517'= 'impWealthWE',
                     'er17012' = 'numInFam',
                     'er20456' = 'famIncome',
                     'er17004' = 'state'))

z03 <- rename(z03, c('er21002' = 'intNum',
                     'er22563' = 'farmBusiness',
                     'er22568' = 'stocks',
                     'er22554' = 'othRealEstate',
                     'er22558' = 'vehicles',
                     'er21051' = 'mortgageDebt', 
                     'er22617' = 'othAssets', 
                     'er21117' = 'wtrMoved',
                     'er21043' = 'houseValue',
                     'S605' = 'checkingAccount',
                     'S607' = 'othDebt',
                     'S620' = 'homeEquity',
                     'S617'= 'impWealthWE',
                     'er21016' = 'numInFam',
                     'er24099' = 'famIncome',
                     'er21003' = 'state'))

z05 <- rename( z05, c('er25002' = 'intNum',
                      'er26544' = 'farmBusiness',
                      'er26549' = 'stocks',
                      'er26535' = 'othRealEstate',
                      'er26539' = 'vehicles',
                      'er25042' = 'mortgageDebt',
                      'er26598' = 'othAssets',
                      'er25098' = 'wtrMoved',
                      'er25029' = 'houseValue',
                      'S705' = 'checkingAccount',
                      'S707' = 'othDebt',
                      'S720' = 'homeEquity',
                      'S717'= 'impWealthWE',
                      'er25016' = 'numInFam',
                      'er28037' = 'famIncome',
                      'er25003' = 'state'))

z07 <- rename(z07, c('er36002' = 'intNum',
                     'er37562' = 'farmBusiness', 
                     'er37567' = 'stocks',
                     'er37553' = 'othRealEstate',
                     'er37557' = 'vehicles',
                     'er36042' = 'mortgageDebt',
                     'er37616' = 'othAssets',
                     'er36103' = 'wtrMoved',
                     'er36029' = 'houseValue',
                     'S805' = 'checkingAccount',
                     'S807' = 'othDebt',
                     'S820' = 'homeEquity',
                     'S817'= 'impWealthWE',
                     'er36016' = 'numInFam',
                     'er41027' = 'famIncome',
                     'er36003' = 'state'))


z09 <- rename(z09, c('er42002' = 'intNum',
                     'er43553' = 'farmBusiness',
                     'er46942' = 'checkingAccount',
                     'er43558' = 'stocks',
                     'er43544' = 'othRealEstate', 
                     'er46966' = 'homeEquity',
                     'er43548' = 'vehicles',
                     'er42043' = 'mortgageDebt',
                     'er43607' = 'othAssets',
                     'er42132' = 'wtrmoved',
                     'er43612' = 'othDebt',
                     'er42030' = 'houseValue',
                     'er46970'= 'impWealthWE',
                     'er42016' = 'numInFam',
                     'er46935' = 'famIncome',
                     'er42003' = 'state'))

z11 <- rename(z11, c('er47302' = 'intNum',
                     'er48878' = 'farmBusiness',
                     'er52350' = 'checkingAccount',
                     'er48883' = 'stocks',
                     'er48869' = 'othRealEstate',
                     'er52390' = 'homeEquity',
                     'er48873' = 'vehicles',
                     'er47348' = 'mortgageDebt',
                     'er48932' = 'otherAssets',
                     'er47440' = 'wtrMoved',
                     'er47330' = 'houseValue',
                     'er52372' = 'cCardDebt',
                     'er48945' = 'studentDebt',
                     'er52380' = 'medicalDebt',
                     'er52384' = 'legalDebt',
                     'er52376' = 'loansFromRelatives',
                     'er52394'= 'impWealthWE',
                     'er47316' = 'numInFam',
                     'er52343' = 'famIncome',
                     'er47303' = 'state'))

z13 <- rename( z13, c('er53002' = 'intNum',
                      'er54625' = 'farmBusiness',
                      'er58161' = 'checkingSavings',
                      'er54634' = 'stocks',
                      'er58165' = 'othRealEstate',
                      'er58207' = 'homeEquity',
                      'er54620' = 'vehicles',
                      'er53048' = 'mortgageDebt',
                      'er54682' = 'othAssets',
                      'er53140' = 'wtrMoved',
                      'er53030' = 'houseValue',
                      'er58185' = 'cCardDebt',
                      'er54697' = 'studentDebt',
                      'er58193' = 'medicalDebt',
                      'er58197' = 'legalDebt',
                      'er58189' = 'loansFromRelatives',
                      'er58211'= 'impWealthWE',
                      'er53016' = 'numInFam',
                      'er58152' = 'famIncome',
                      'er53003' = 'state'))

# create one debt variable for 2011 & 2013
z11$othDebt = z11$cCardDebt + z11$studentDebt + z11$medicalDebt + z11$legalDebt + z11$loansFromRelatives
z13$othDebt13 = z13$cCardDebt + z13$studentDebt + z13$medicalDebt + z13$legalDebt + z13$loansFromRelatives

# drop other debt files
z11 <- subset(z11, select = -c(cCardDebt, studentDebt, medicalDebt, legalDebt, loansFromRelatives) )
z13 <- subset(z13, select = -c(cCardDebt, studentDebt, medicalDebt, legalDebt, loansFromRelatives) )



# maintain only record which have the same household head year to year - this means no splitoffs
# To create a single year Head file: Select individuals with Relationship to Head of "Head"
# (a code value of 1 for 1968-1982; code 10 from 1983 onward) and with values for Sequence Number
# in the range 1-20.
#The combination of the 1968 ID and the person number uniquely identify each individual. 
#To identify an individual across waves use the 1968 ID and Person Number 68 Summary Variables ER30001 and ER30002. Though you can combine them uniquely in many ways we find that many researchers use the following method:
#(ER30001 * 1000) + ER30002
#(1968 ID multiplied by 1000) plus Person Number 68
#"er30001" = "1968IntNum"
#'er30002' = "1968PersonNum"
#'
#'Each family unit in a specific wave is assigned a unique "Family Interview (ID) Number" valid for that wave only.
#' In addition, each family also has a "1968 Family Identifier", also known as the "1968 ID". 
#' This is the Family Interview (ID) Number that was assigned to the original family in the 1968 interviewing wave.
#' When sample members in any family move out and establish their own household, we interview them
#'(these families are called "splitoffs", in the first year they are formed). These new "splitoff" families have
#'the same 1968 ID as the family they moved out of, and keep that same 1968 ID each year. All families with the 
#'same 1968 ID contain at least one of the original members from the 1968 family or their lineal
#' descendents born after 1968.


# variable = 1 if household head, 0 otherwise

z84$hhHead84 <- ifelse( z84$'hhRelStatus84' == 10 
                        & z84$'sequenceNum84' <= 20, 1, 0 )
z89$hhHead89 <- ifelse( z89$'hhRelStatus89' == 10 
                        & z89$'sequenceNum89' <= 20, 1, 0 )
z94$hhHead94 <- ifelse( z94$'hhRelStatus94' == 10 
                        & z94$'sequenceNum94' <= 20, 1, 0 )
z99$hhHead99 <- ifelse( z99$'hhRelStatus99' == 10 
                        & z99$'sequenceNum99' <= 20, 1, 0 )
z01$hhHead01 <- ifelse( z01$'hhRelStatus01' == 10 
                        & z01$'sequenceNum01' <= 20, 1, 0 )
z03$hhHead03 <- ifelse( z03$'hhRelStatus03' == 10 
                        & z03$'sequenceNum03' <= 20, 1, 0 )
z05$hhHead05 <- ifelse( z05$'hhRelStatus05' == 10 
                        & z05$'sequenceNum05' <= 20, 1, 0 )
z07$hhHead07 <- ifelse( z07$'hhRelStatus07' == 10 
                        & z07$'sequenceNum07' <= 20, 1, 0 )
z09$hhHead09 <- ifelse( z09$'hhRelStatus09' == 10 
                        & z09$'sequenceNum09' <= 20, 1, 0 )
z11$hhHead11 <- ifelse( z11$'hhRelStatus11' == 10 
                        & z11$'sequenceNum11' <= 20, 1, 0 )
z13$hhHead13 <- ifelse( z13$'hhRelStatus13' == 10 
                        & z13$'sequenceNum13' <= 20, 1, 0 )



# create a unique identifier variable for each individual
z84$uniqueID <- (z84$'1968IntNum' * 1000) + z84$'1968PersonNum'
z89$uniqueID <- (z89$'1968IntNum' * 1000) + z89$'1968PersonNum'
z94$uniqueID <- (z94$'1968IntNum' * 1000) + z94$'1968PersonNum'
z99$uniqueID <- (z99$'1968IntNum' * 1000) + z99$'1968PersonNum'
z01$uniqueID <- (z01$'1968IntNum' * 1000) + z01$'1968PersonNum'
z03$uniqueID <- (z03$'1968IntNum' * 1000) + z03$'1968PersonNum'
z05$uniqueID <- (z05$'1968IntNum' * 1000) + z05$'1968PersonNum'
z07$uniqueID <- (z07$'1968IntNum' * 1000) + z07$'1968PersonNum'
z09$uniqueID <- (z09$'1968IntNum' * 1000) + z09$'1968PersonNum'
z11$uniqueID <- (z11$'1968IntNum' * 1000) + z11$'1968PersonNum'
z13$uniqueID <- (z13$'1968IntNum' * 1000) + z13$'1968PersonNum'



rm(i,j,shortYear, shortYeari, shortYearj)



