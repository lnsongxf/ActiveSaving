#File to replicate Cooper (2006) - calculating 'active savings'
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



# rename remaining variables something useful
z84 <- rename( z84, c('v10002' = 'intNum84',
                      'v10908' = 'farmBusiness84',
                      'v10913' = 'stocks84',
                      'v10899' = 'othRealEstate84',
                      'v10903' = 'vehicles84',
                      'v10020' = 'mortgageDebt84',
                      'v10923' = 'othAssets84',
                      'v10447' = 'wtrMoved84',
                      'v10018' = 'houseValue84',
                      'S105' = 'checkingAccount84',
                      'S107'= 'othDebt84',
                      'S120'= 'homeEquity84',
                      'S117'= 'impWealthWE84',
                      'v10418' = 'numInFam',
                      'v11022' = 'famIncome',
                      'v10003' = 'state'))

z89 <- rename( z89, c('v16302' = 'intNum89',
                      'v17323' = 'farmBusiness89',
                      'v17326' = 'stocks89',
                      'v17318' = 'othRealEstate89',
                      'v17320' = 'vehicles89',
                      'v16326' = 'mortgageDebt89',
                      'v17332' = 'othAssets89',
                      'v16649' = 'wtrMoved89',
                      'v16324' = 'houseValue89',
                      'S205' = 'checkingAccount89',
                      'S207' = 'othDebt89',
                      'S220' = 'homeEquity89',
                      'S217'= 'impWealthWE89',
                      'v16630' = 'numInFam',
                      'v17533' = 'famIncome',
                      'v16303' = 'state'))

z94 <- rename(z94, c('er2002' = 'intNum94',
                     'er3731' = 'farmBusiness94',
                     'er3736' = 'stocks94',
                     'er3722' = 'othRealEstate94',
                     'er3726' = 'vehicles94',
                     'er2037' = 'mortgageDebt94',
                     'er3748' = 'othAssets94',
                     'er2062' = 'wtrMoved94',
                     'er2033' = 'houseValue94',
                     'S305' = 'checkingAccount94',
                     'S307' = 'othDebt94',
                     'S320' = 'homeEquity94',
                     'S317'= 'impWealthWE94',
                     'er2006' = 'numInFam',
                     'er4153' = 'famIncome',
                     'er4156' = 'state'))

z99 <- rename( z99, c('er13002' = 'intNum99',
                      'er15002' = 'farmBusiness99',
                      'er15007' = 'stocks99',
                      'er14993' = 'othRealEstate99',
                      'er14997' = 'vehicles99',
                      'er13047' = 'mortgageDebt99',
                      'er15026' = 'othAssets99',
                      'er13077' = 'wtrMoved99',
                      'er13041' = 'houseValue99',
                      'S405' = 'checkingAccount99',
                      'S407' = 'othDebt99',
                      'S420' = 'homeEquity99',
                      'S417'= 'impWealthWE99',
                      'er13009' = 'numInFam',
                      'er16462' = 'famIncome',
                      'er13004' = 'state'))

z01 <- rename(z01, c('er17002' = 'intNum01', 
                     'er19198' = 'farmBusiness01',
                     'er19203' = 'stocks01',
                     'er19189' = 'othRealEstate01',
                     'er19193' = 'vehicles01',
                     'er17052' = 'mortgageDebt01',
                     'er19222' = 'othAssets01',
                     'er17088' = 'wtrMoved01',
                     'er17044' = 'houseValue01',
                     'S505' = 'checkingAccount01',
                     'S507' = 'othDebt01',
                     'S520' = 'homeEquity01',
                     'S517'= 'impWealthWE01',
                     'er17012' = 'numInFam',
                     'er20456' = 'famIncome',
                     'er17004' = 'state'))

z03 <- rename(z03, c('er21002' = 'intNum03',
                     'er22563' = 'farmBusiness03',
                     'er22568' = 'stocks03',
                     'er22554' = 'othRealEstate03',
                     'er22558' = 'vehicles03',
                     'er21051' = 'mortgageDebt03', 
                     'er22617' = 'othAssets03', 
                     'er21117' = 'wtrMoved03',
                     'er21043' = 'houseValue03',
                     'S605' = 'checkingAccount03',
                     'S607' = 'othDebt03',
                     'S620' = 'homeEquity03',
                     'S617'= 'impWealthWE03',
                     'er21016' = 'numInFam',
                     'er24099' = 'famIncome',
                     'er21003' = 'state'))

z05 <- rename( z05, c('er25002' = 'intNum05',
                      'er26544' = 'farmBusiness05',
                      'er26549' = 'stocks05',
                      'er26535' = 'othRealEstate05',
                      'er26539' = 'vehicles05',
                      'er25042' = 'mortgageDebt05',
                      'er26598' = 'othAssets05',
                      'er25098' = 'wtrMoved05',
                      'er25029' = 'houseValue05',
                      'S705' = 'checkingAccount05',
                      'S707' = 'othDebt05',
                      'S720' = 'homeEquity05',
                      'S717'= 'impWealthWE05',
                      'er25016' = 'numInFam',
                      'er28037' = 'famIncome',
                      'er25003' = 'state'))

z07 <- rename(z07, c('er36002' = 'intNum07',
                     'er37562' = 'farmBusiness07', 
                     'er37567' = 'stocks07',
                     'er37553' = 'othRealEstate07',
                     'er37557' = 'vehicles07',
                     'er36042' = 'mortgageDebt07',
                     'er37616' = 'othAssets07',
                     'er36103' = 'wtrMoved07',
                     'er36029' = 'houseValue07',
                     'S805' = 'checkingAccount07',
                     'S807' = 'othDebt07',
                     'S820' = 'homeEquity07',
                     'S817'= 'impWealthWE07',
                     'er36016' = 'numInFam',
                     'er41027' = 'famIncome',
                     'er36003' = 'state'))


z09 <- rename(z09, c('er42002' = 'intNum09',
                     'er43553' = 'farmBusiness09',
                     'er46942' = 'checkingAccount09',
                     'er43558' = 'stocks09',
                     'er43544' = 'othRealEstate09', 
                     'er46966' = 'homeEquity09',
                     'er43548' = 'vehicles09',
                     'er42043' = 'mortgageDebt09',
                     'er43607' = 'othAssets09',
                     'er42132' = 'wtrmoved09',
                     'er43612' = 'othDebt09',
                     'er42030' = 'houseValue09',
                     'er46970'= 'impWealthWE09',
                     'er42016' = 'numInFam',
                     'er46935' = 'famIncome',
                     'er42003' = 'state'))

z11 <- rename(z11, c('er47302' = 'intNum11',
                     'er48878' = 'farmBusiness11',
                     'er52350' = 'checkingAccount11',
                     'er48883' = 'stocks11',
                     'er48869' = 'othRealEstate11',
                     'er52390' = 'homeEquity11',
                     'er48873' = 'vehicles11',
                     'er47348' = 'mortgageDebt11',
                     'er48932' = 'otherAssets11',
                     'er47440' = 'wtrMoved11',
                     'er47330' = 'houseValue11',
                     'er52372' = 'cCardDebt11',
                     'er48945' = 'studentDebt11',
                     'er52380' = 'medicalDebt11',
                     'er52384' = 'legalDebt11',
                     'er52376' = 'loansFromRelatives11',
                     'er52394'= 'impWealthWE11',
                     'er47316' = 'numInFam',
                     'er52343' = 'famIncome',
                     'er47303' = 'state'))

z13 <- rename( z13, c('er53002' = 'intNum13',
                      'er54625' = 'farmBusiness13',
                      'er58161' = 'checkingSavings13',
                      'er54634' = 'stocks13',
                      'er58165' = 'othRealEstate13',
                      'er58207' = 'homeEquity13',
                      'er54620' = 'vehicles13',
                      'er53048' = 'mortgageDebt13',
                      'er54682' = 'othAssets13',
                      'er53140' = 'wtrMoved13',
                      'er53030' = 'houseValue13',
                      'er58185' = 'cCardDebt13',
                      'er54697' = 'studentDebt13',
                      'er58193' = 'medicalDebt13',
                      'er58197' = 'legalDebt13',
                      'er58189' = 'loansFromRelatives13',
                      'er58211'= 'impWealthWE13',
                      'er53016' = 'numInFam',
                      'er58152' = 'famIncome',
                      'er53003' = 'state'))
# create one debt variable for 2011 & 2013
z11$othDebt11 = z11$cCardDebt11 + z11$studentDebt11 + z11$medicalDebt11 + z11$legalDebt11 + z11$loansFromRelatives11
z13$othDebt13 = z13$cCardDebt13 + z13$studentDebt13 + z13$medicalDebt13 + z13$legalDebt13 + z13$loansFromRelatives13

# drop other debt files
z11 <- subset(z11, select = -c(cCardDebt11, studentDebt11, medicalDebt11, legalDebt11, loansFromRelatives11) )
z13 <- subset(z13, select = -c(cCardDebt13, studentDebt13, medicalDebt13, legalDebt13, loansFromRelatives13) )



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





# merge two timepoints together,
# by matching unique ID (and individual variables)
mergeCriteria = c('uniqueID', 'one',
                  'intNum84',  'intNum89',  'intNum94',  'intNum99',  'intNum01',
                  'intNum03',  'intNum05',  'intNum07',  'intNum09',  'intNum11',
                  'intNum13',
                  '1968IntNum', '1968PersonNum', 'primarySamplingUnit', 'stratification',
                  'sequenceNum84', 'sequenceNum89', 'sequenceNum94', 'sequenceNum99', 'sequenceNum01', 
                  'sequenceNum03', 'sequenceNum05', 'sequenceNum07', 'sequenceNum09', 'sequenceNum11', 
                  'sequenceNum13',
                  'sex', 
                  'empStatus84', 'empStatus89', 'empStatus94', 'empStatus99', 'empStatus01', 
                  'empStatus03', 'empStatus05', 'empStatus07', 'empStatus09', 'empStatus11', 
                  'empStatus13',
                  'age84', 'age89', 'age94', 'age99', 'age01',
                  'age03', 'age05', 'age07', 'age09', 'age11', 
                  'age13',
                  'hhRelStatus84', 'hhRelStatus89', 'hhRelStatus94', 'hhRelStatus99', 'hhRelStatus01', 
                  'hhRelStatus03', 'hhRelStatus05', 'hhRelStatus07', 'hhRelStatus09', 'hhRelStatus11', 
                  'hhRelStatus13',
                  'currSchoolLev', 'highestSchoolLev',
                  'longWeight84', 'longWeight89', 'longWeight94', 'longWeight99', 'longWeight01',
                  'longWeight03', 'longWeight05', 'longWeight07', 'longWeight09', 'longWeight11',
                  'longWeight13'
)

# 84 & 89
x8489 <- merge( z84 , z89, by = mergeCriteria, all = FALSE )
# 89 & 94
x8994 <- merge( z89 , z94, by = mergeCriteria, all = FALSE )
# 94 & 99
x9499 <- merge( z94 , z99, by = mergeCriteria, all = FALSE )
# 99 & 01
x9901 <- merge( z99 , z01, by = mergeCriteria, all = FALSE )
# 01 & 03
x0103 <- merge( z01 , z03, by = mergeCriteria, all = FALSE )
# 03 & 05
x0305 <- merge( z03 , z05, by = mergeCriteria, all = FALSE )
# 05 & 07
x0507 <- merge( z05 , z07, by = mergeCriteria, all = FALSE )
# 07 & 09
x0709 <- merge( z07 , z09, by = mergeCriteria, all = FALSE )
# 09 & 11
x0911 <- merge( z09 , z11, by = mergeCriteria, all = FALSE )
# 11 & 13
x1113 <- merge( z11 , z13, by = mergeCriteria, all = FALSE )

rm(mergeCriteria)

# restrict sample to being responsive in each year & nationally representative
#x8489 <-subset(x8489, intNum84>0 & intNum89>0 & longWeight84>0 & longWeight89>0)
#x8994 <-subset(x8994, intNum89>0 & intNum94>0 & longWeight89>0 & longWeight94>0)
#x9499 <-subset(x9499, intNum94>0 & intNum99>0 & longWeight94>0 & longWeight99>0)
#x9901 <-subset(x9901, intNum99>0 & intNum01>0 & longWeight99>0 & longWeight01>0)
#x0103 <-subset(x0103, intNum01>0 & intNum03>0 & longWeight01>0 & longWeight03>0)
#x0305 <-subset(x0305, intNum03>0 & intNum05>0 & longWeight03>0 & longWeight05>0)
#x0507 <-subset(x0507, intNum05>0 & intNum07>0 & longWeight05>0 & longWeight07>0)
#x0709 <-subset(x0709, intNum07>0 & intNum09>0 & longWeight07>0 & longWeight09>0)
#x0911 <-subset(x0911, intNum09>0 & intNum11>0 & longWeight09>0 & longWeight11>0)
#x1113 <-subset(x1113, intNum11>0 & intNum13>0 & longWeight11>0 & longWeight13>0)

# restrict sample to being responsive in each year
x8489 <-subset(x8489, intNum84>0 & intNum89>0)
x8994 <-subset(x8994, intNum89>0 & intNum94>0)
x9499 <-subset(x9499, intNum94>0 & intNum99>0)
x9901 <-subset(x9901, intNum99>0 & intNum01>0)
x0103 <-subset(x0103, intNum01>0 & intNum03>0)
x0305 <-subset(x0305, intNum03>0 & intNum05>0)
x0507 <-subset(x0507, intNum05>0 & intNum07>0)
x0709 <-subset(x0709, intNum07>0 & intNum09>0)
x0911 <-subset(x0911, intNum09>0 & intNum11>0)
x1113 <-subset(x1113, intNum11>0 & intNum13>0)


# # get rid of DK/refused/NA values - for the self-created wealth variable
# x8489 <-subset(x8489, farmBusiness84<9999996 & farmBusiness89<9999996 
#                & stocks84<9999996 & stocks89<9999996 
#                & vehicles84<999997 & vehicles89<999997 
#                & othAssets84<9999997 & othAssets89<9999997 
#                & othRealEstate84<9999997) & othRealEstate89<9999997)
# 
# x8994 <-subset(x8994, farmBusiness89<9999996 & farmBusiness94<9999996 
#                & stocks89<9999996 & stocks94<9999996 
#                & vehicles89<999997 & vehicles94<999997 
#                & othAssets89<9999997 & othAssets94<9999997
#                & othRealEstate89<9999997) & othRealEstate94<9999997)
# 
# x9499 <-subset(x9499, farmBusiness94<9999996 & farmBusiness99<9999996 
#                & stocks94<9999996 & stocks99<9999996 
#                & vehicles94<999997 & vehicles99<999997 
#                & othAssets94<9999997 & othAssets99<9999997
#                & othRealEstate94<9999997) & othRealEstate99<9999997)
# 
# x0507 <-subset(x0507, farmBusiness05<9999996 & farmBusiness07<9999996 
#                & stocks05<9999996 & stocks07<9999996 
#                & vehicles05<999997 & vehicles07<999997 
#                & othAssets05<9999997 & othAssets07<9999997
#                & othRealEstate05<9999997) & othRealEstate07<9999997)
# 
# x0709 <-subset(x0709, farmBusiness07<9999996 & farmBusiness09<9999996 
#                & stocks07<9999996 & stocks09<9999996 
#                & vehicles07<999997 & vehicles09<999997 
#                & othAssets07<9999997 & othAssets09<9999997
#                & othRealEstate07<9999997) & othRealEstate09<9999997)
# only keep those records whose head of household has not changed between waves
# keep each record whose hhHead89's uniqueID = hhHead84's uniqueID

# make a list of interview numbers in 1989 such that the household head
# is the same as the household head in 1984. By interview number means
# we retain all family records, not just those of the hh head.

#84-89
# familySameHead8489 <- c()
# for(intNum in unique(x8489$'intNum89')){
#   temp1 <- subset(x8489, x8489$'intNum89' ==  intNum & x8489$'hhHead89' == 1)
#   if (dim(temp1)[1] != 1){
#     next
#   } else{
#     if(temp1$'hhHead84'==1){
#     familySameHead8489 <- c(familySameHead8489, intNum)
#   }
#   }
# }
# rm(temp1, intNum)
# 
# #89-94
# familySameHead8994 <- c()
# for(intNum in unique(x8994$'intNum94')){
#   temp1 <- subset(x8994, x8994$'intNum94' ==  intNum & x8994$'hhHead94' == 1)
#   if (dim(temp1)[1] != 1){
#     next
#   } else{
#     if(temp1$'hhHead89'==1){
#       familySameHead8994 <- c(familySameHead8994, intNum)
#     }
#   }
# }
# rm(temp1, intNum)
# 
# #94-99
# familySameHead9499 <- c()
# for(intNum in unique(x9499$'intNum99')){
#    temp1 <- subset(x9499, x9499$'intNum99' ==  intNum & x9499$'hhHead99' == 1)
#    if (dim(temp1)[1] != 1){
#      next
#    } else{
#      if(temp1$'hhHead94'==1){
#        familySameHead9499 <- c(familySameHead9499, intNum)
#      }
#    }
#  }
#  rm(temp1, intNum)

#  #05-07
#  familySameHead0507 <- c()
#  for(intNum in unique(x0709$'intNum07')){
#     temp1 <- subset(x0507, x0507$'intNum07' ==  intNum & x0507$'hhHead07' == 1)
#     if (dim(temp1)[1] != 1){
#       next
#     } else{
#       if(temp1$'hhHead05'==1){
#         familySameHead0507 <- c(familySameHead0507, intNum)
#       }
#     }
#   }
#   rm(temp1, intNum)

# restrict sample to having the same household head in adjacent years
x8489 <- subset(x8489, x8489$hhHead84==1 & x8489$hhHead89==1 )
x8994 <- subset(x8994, x8994$hhHead89==1 & x8994$hhHead94==1 )
x9499 <- subset(x9499, x9499$hhHead94==1 & x9499$hhHead99==1 )
x9901 <- subset(x9901, x9901$hhHead99==1 & x9901$hhHead01==1 )
x0103 <- subset(x0103, x0103$hhHead01==1 & x0103$hhHead03==1 )
x0305 <- subset(x0305, x0305$hhHead03==1 & x0305$hhHead05==1 )
x0507 <- subset(x0507, x0507$hhHead05==1 & x0507$hhHead07==1 )
x0709 <- subset(x0709, x0709$hhHead07==1 & x0709$hhHead09==1 )
x0911 <- subset(x0911, x0911$hhHead09==1 & x0911$hhHead11==1 )
x1113 <- subset(x1113, x1113$hhHead11==1 & x1113$hhHead13==1 )




# save the familySameHead variables (because those for loops are slow as fuck - is there a better way??)
#save(familySameHead8489, familySameHead8994, familySameHead9499, familySameHead0507, file = "familySameHeads.RData")

# load( "familySameHeads.RData" )
# x8489 <-subset(x8489, x8489$'intNum89' %in% familySameHead8489)
# x8994 <-subset(x8994, x8994$'intNum94' %in% familySameHead8994)
# x9499 <-subset(x9499, x9499$'intNum99' %in% familySameHead9499)
# x0507 <-subset(x0507, x0507$'intNum07' %in% familySameHead0507)

# calculate active saving

# #if wtrMoved89 - I should do something if they moved - should I?
# x8489$activeSaving <- x8489$'farmBusiness89' - x8489$'farmBusiness84' + 
#   x8489$'checkingAccount89'- x8489$'checkingAccount84' + 
#   x8489$'othRealEstate89' - x8489$'othRealEstate84' + 
#   x8489$'stocks89' - x8489$'stocks84' + 
#   x8489$'vehicles89' - x8489$'vehicles84' + 
#   x8489$'othAssets89' - x8489$'othAssets84' - 
#   (x8489$'othDebt89' - x8489$'othDebt84') + 
#   x8489$'homeEquity89' - x8489$'homeEquity84'
# 
# x8994$activeSaving <- x8994$'farmBusiness94' - x8994$'farmBusiness89' + 
#   x8994$'checkingAccount94'- x8994$'checkingAccount89' + 
#   x8994$'othRealEstate94' - x8994$'othRealEstate89' + 
#   x8994$'stocks94' - x8994$'stocks89' + 
#   x8994$'vehicles94' - x8994$'vehicles89' + 
#   x8994$'othAssets94' - x8994$'othAssets89' - 
#   (x8994$'othDebt94' - x8994$'othDebt89') + 
#   x8994$'homeEquity94' - x8994$'homeEquity89'
# 
# x9499$activeSaving <- x9499$'farmBusiness99' - x9499$'farmBusiness94' + 
#   x9499$'checkingAccount99'- x9499$'checkingAccount94' + 
#   x9499$'othRealEstate99' - x9499$'othRealEstate94' + 
#   x9499$'stocks99' - x9499$'stocks94' + 
#   x9499$'vehicles99' - x9499$'vehicles94' + 
#   x9499$'othAssets99' - x9499$'othAssets94' - 
#   (x9499$'othDebt99' - x9499$'othDebt94') + 
#   x9499$'homeEquity99' - x9499$'homeEquity94'
# 
# x0507$activeSaving <- x0507$'farmBusiness07' - x0507$'farmBusiness05' + 
#   x0507$'checkingAccount07'- x0507$'checkingAccount05' + 
#   x0507$'othRealEstate07' - x0507$'othRealEstate05' + 
#   x0507$'stocks07' - x0507$'stocks05' + 
#   x0507$'vehicles07' - x0507$'vehicles05' + 
#   x0507$'othAssets07' - x0507$'othAssets05' - 
#   (x0507$'othDebt07' - x0507$'othDebt05') + 
#   x0507$'homeEquity07' - x0507$'homeEquity05'


# create an 'impActiveSaving' variable which is impWealth change year to year
# get rid of NA values (some observations don't have savings data)
x8489$impActiveSaving <-x8489$'impWealthWE89' - x8489$'impWealthWE84'
x8489<-na.omit(x8489)
x8994$impActiveSaving <-x8994$'impWealthWE94' - x8994$'impWealthWE89'
x8994<-na.omit(x8994)
x9499$impActiveSaving <-x9499$'impWealthWE99' - x9499$'impWealthWE94'
x9499<-na.omit(x9499)
x9901$impActiveSaving <-x9901$'impWealthWE01' - x9901$'impWealthWE99'
x9901<-na.omit(x9901)
x0103$impActiveSaving <-x0103$'impWealthWE03' - x0103$'impWealthWE01'
x0103<-na.omit(x0103)
x0305$impActiveSaving <-x0305$'impWealthWE05' - x0305$'impWealthWE03'
x0305<-na.omit(x0305)
x0507$impActiveSaving <-x0507$'impWealthWE07' - x0507$'impWealthWE05'
x0507<-na.omit(x0507)
x0709$impActiveSaving <-x0709$'impWealthWE09' - x0709$'impWealthWE07'
x0709<-na.omit(x0709)
x0911$impActiveSaving <-x0911$'impWealthWE11' - x0911$'impWealthWE09'
x0911<-na.omit(x0911)
x1113$impActiveSaving <-x1113$'impWealthWE13' - x1113$'impWealthWE11'
x1113<-na.omit(x1113)

