#Load the PSID data necessary to compute active savings, combine with the wealth supplement, rename variables.
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/Research ideas/Nonlinearity/PSID/ActiveSaving/")


library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data
library(reshape2)  # load package needed to go from wide to long format
library(np) # non parametric library


# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN

# load the individual cross-year file
load( "ind.rda" )

# limit the file to only the variables needed
ind.KeepVars <-
  c('er30001' ,   # 1968 interview number
    'er30002' ,   # 1968 person number
    'er31997' ,    # primary sampling unit variable 
    'er31996' ,  	# stratification variable
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
    'er30443',		# highest school level completed, 1984
    'er30620',  	# highest school level completed, 1989
    'er33115',  	# highest school level completed, 1994
    'er33516',  	# highest school level completed, 1999
    'er33616',  	# highest school level completed, 2001
    'er33716',  	# highest school level completed, 2003
    'er33817',  	# highest school level completed, 2005
    'er33917',  	# highest school level completed, 2007
    'er34020',  	# highest school level completed, 2009
    'er34119',  	# highest school level completed, 2011
    'er34230'    	# highest school level completed, 2013
  )


# create a "skinny"	data.frame object that only contains the
# columns you need for this analysis,
# specified in the KeepVars character vector
w <- ind[ , ind.KeepVars ]




# remove the original data.frame object then free up memory
rm( ind ) ; gc()


# Describe which variables to keep from the family level (f) and wealth supplement files (w)
# 1984
f84.KeepVars <-
  c(
    'v10002',   # family interview number
    'v10908',   # farm/business
    'v10913',   # stocks
    'v10903',   # vehicles
    'v10020',   # mortgage debt
    'v10923',   # other assets
    'v10447',   # moved?
    'v10018',   # house value
    'v10418',   # number in family
    'v11022',   # family income
    'v10003',   # state
    'v11079',   # longitudinal weight
    'v10899'   # real estate equity
    
  )


w84.KeepVars <-
  c(
    'V10002',  # family interview number
    'S105',    # checking/saving account
    'S107',    # other debt
    'S120',    # home equity
    'S117',    # imputed wealth with equity
    'S116'    # imputed wealth without equity
  )

# 1989
f89.KeepVars <-
  c( 
    'v16302',   # family interview number
    'v17323',   # farm/business
    'v17326',   # stocks
    'v17320',   # vehicles
    'v16326',   # mortage debt
    'v17332',   # other assets
    'v16649',   # moved?
    'v16324',   # house value
    'v16630',   # number in family
    'v17533',   # family income
    'v16303',   # state
    'v17612',   # longitudinal weight
    'v17340',   # added to private annuities 
    'v17346',   # real estate bought
    'v17355',   # invested in farm/business 
    'v17352',   # additions/repairs to real estate 
    'v17365',   # stocks bought
    'v17371',   # assets removed by movers out 
    'v17379',   # debts added by movers in
    'v17318',   # real estate equity 
    'v17343',   # pensions/annuities cashed in
    'v17349',   # real estate sold 
    'v17358',   # farm/business sold
    'v17368',   # stocks sold
    'v17373',   # debts removed by movers out
    'v17377',   # assets added by movers in
    'v17384',   # inheritance recieved
    'v17610',   # activeSaving (computed by PSID, for comparison)
    'v17609',   # total wealth 84
    'v17389'   # total wealth 89
  )

w89.KeepVars <-
  c(
    'V16302',  # family interview number
    'S205',    # checking/saving account
    'S207',    # other debt
    'S220',    # home equity
    'S217',    # imputed wealth with equity
    'S216'    # imputed wealth without equity
  )

# 1994
f94.KeepVars <-
  c(
    'er2002',   # family interview number
    'er3731',   # farm/business
    'er3736',   # stocks
    'er3726',   # vehicles 
    'er2037',   # mortgage debt
    'er3748',   # other assets
    'er2062',   # moved?
    'er2033',   # house value
    'er2006',   # number in family
    'er4153',   # family income
    'er4156',   # state
    'er4160',   # longitudinal weight
    'er3758',   # added to private annuities
    'er3774',   # real estate bought
    'er3788',   # invested in farm/business
    'er3784',   # additions/repairs to real estate
    'er3805',   # stocks bought 
    'er3817',   # assets removed by movers out
    'er3832',   # debts added by movers in
    'er3722',   # real estate equity
    'er3763',   # pensions/annuities cashed in
    'er3779',   # real estate sold 
    'er3793',   # farm/business sold
    'er3811',   # stocks sold
    'er3822',   # debts removed by movers out
    'er3827',   # assets added by movers in
    'er3838'   # inheritance recieved
  )



w94.KeepVars <-
  c(
    'ER2002',  # family interview number
    'S305',    # checking/saving account
    'S307',    # other debt
    'S320',    # home equity
    'S317',    # imputed wealth with equity
    'S316'    # imputed wealth without equity
  )


# 1999
f99.KeepVars <-
  c( 
    'er13002',   # family interview number
    'er15002',   # farm/business
    'er15007',   # stocks
    'er14997',   # vehicles
    'er13047',   # mortgage debt
    'er15026',   # other assets
    'er13077',   # moved?
    'er13041',   # house value
    'er13009',   # number in family
    'er16462',   # family income
    'er13004',   # state
    'er16518',   # longitudinal weight
    'er15036',   # added to private annuities
    'er15052',   # real estate bought
    'er15066',   # invested in farm/business
    'er15062',   # additions/repairs to real estate
    'er15083',   # stocks bought
    'er15095',   # assets removed by movers out
    'er15111',   # debts added by movers in
    'er14993',   # real estate equity
    'er15041',   # pensions/annuities cashed in
    'er15057',   # real estate sold 
    'er15071',   # farm/business sold
    'er15089',   # stocks sold
    'er15100',   # debts removed by movers out 
    'er15106',   # assets added by movers in
    'er15117'   # inheritance received
  )
w99.KeepVars <-
  c(
    'ER13002', # family interview number
    'S405',    # checking/saving account
    'S407',    # other debt
    'S420',    # home equity
    'S417',    # imputed wealth with equity
    'S416'    # imputed wealth without equity
  )

# 2001
f01.KeepVars <-
  c(
    'er17002',   # family interview number 
    'er19198',   # farm/business
    'er19203',   # stocks
    'er19193',   # vehicles
    'er17052',   # mortgage debt
    'er19222',   # other assets
    'er17088',   # moved?
    'er17044',   # house value
    'er17012',   # number in family
    'er20456',   # family income
    'er17004',   # state
    'er20394',   # longitudinal weight
    'er19232',   # added to private annuities 
    'er19248',   # real estate bought 
    'er19262',   # invested in farm/business
    'er19258',   # additions/repairs to real estate
    'er19279',   # stocks bought 
    'er19291',   # assets removed by movers out 
    'er19307',   # debts added by movers in
    'er19189',   # real estate equity
    'er19237',   # pensions/annuities cashed in
    'er19253',   # real estate sold
    'er19267',   # farm/business sold
    'er19285',   # stocks sold
    'er19296',   # debt removed by movers out
    'er19302',   # assets added by movers in
    'er19313'   # inheritance received
  )
w01.KeepVars <-
  c(
    'ER17002', # family interview number
    'S505',    # checking/saving account
    'S507',    # other debt
    'S520',    # home equity
    'S517',    # imputed wealth with equity
    'S516'    # imputed wealth without equity
  )

# 2003
f03.KeepVars <-
  c(
    'er21002',   # family interview number
    'er22563',   # farm/business
    'er22568',   # stocks
    'er22558',   # vehicles
    'er21051',   # mortgage debt 
    'er22617',   # other assets 
    'er21117',   # moved?
    'er21043',   # house value
    'er21016',   # number in family
    'er24099',   # family income
    'er21003',   # state
    'er24179',   # longitudinal weight
    'er22627',   # added to private annuities
    'er22643',   # real estate bought
    'er22657',   # invested in farm/business
    'er22653',   # additions/repairs to real estate 
    'er22674',   # stocks bought
    'er22686',   # assets removed by movers out 
    'er22702',   # debts added by movers in
    'er22554',   # real estate equity 
    'er22632',   # pensions/annuities cashed in
    'er22648',   # real estate sold 
    'er22662',   # farm/business sold
    'er22680',   # stocks sold
    'er22691',   # debts removed by movers out
    'er22697',   # assets added by movers in
    'er22708'   # inheritance received
  )
w03.KeepVars <-
  c(
    'ER21002', # family interview number
    'S605',    # checking/saving account
    'S607',    # other debt
    'S620',    # home equity
    'S617',    # imputed wealth with equity
    'S616'    # imputed wealth without equity
  )

# 2005
f05.KeepVars <-
  c(
    'er25002',   # family interview number
    'er26544',   # farm/business
    'er26549',   # stocks
    'er26539',   # vehicles
    'er25042',   # mortgage debt
    'er26598',   # other assets
    'er25098',   # moved?
    'er25029',   # house value
    'er25016',   # number in family
    'er28037',   # family income
    'er25003',   # state
    'er28078',   # longitudinal weight
    'er26608',   # added to private annuities
    'er26624',   # real estate bought 
    'er26638',   # invested in farm/business
    'er26634',   # additions/repairs to real estate
    'er26655',   # stocks bought
    'er26667',   # assets removed by movers out
    'er26683',   # debts added by movers in
    'er26535',   # real estate equity
    'er26613',   # pensions/annuities cashed in
    'er26629',   # real estate sold
    'er26643',   # farm/business sold
    'er26661',   # stocks sold
    'er26672',   # debts removed by movers out
    'er26678',   # assets added by movers in
    'er26689'   # inheritance received
  )

w05.KeepVars <-
  c(
    'ER25002', # family interview number
    'S705',    # checking/saving account
    'S707',    # other debt
    'S720',    # home equity
    'S717',    # imputed wealth with equity
    'S716'    # imputed wealth without equity
  )

# 2007
f07.KeepVars <-
  c(
    'er36002',   # family interview number
    'er37562',   # farm/business 
    'er37567',   # stocks
    'er37557',   # vehicles
    'er36042',   # mortgage debt
    'er37616',   # other assets
    'er36103',   # moved?
    'er36029',   # house value
    'er36016',   # number in family
    'er41027',   # family income
    'er36003',   # state
    'er41069',   # longitudinal weight
    'er37626',   # added to private annuities
    'er37642',   # real estate bought 
    'er37656',   # invested in farm/business 
    'er37652',   # additions/repairs to real estate 
    'er37673',   # stocks bought 
    'er37685',   # assets removed by movers out
    'er37701',   # debts added by movers in
    'er37553',   # real estate equity
    'er37631',   # pensions/annuities cashed in
    'er37647',   # real estate sold
    'er37661',   # farm/business sold
    'er37679',   # stocks sold
    'er37690',   # debts removed by movers out
    'er37696',   # assets added by movers in
    'er37707'   # inheritance received
  )
w07.KeepVars <-
  c(
    'ER36002', # family interview number
    'S805',    # value of checking/saving account
    'S807',    # other debt
    'S820',    # home equity
    'S817',    # imputed wealth with equity
    'S816'    # imputed wealth without equity
  )

# 2009
f09.KeepVars <-
  c(
    'er42002',   # family interview number    
    'er43553',   # farm/business
    'er46942',   # checking/savings
    'er43558',   # stocks
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
    'er42003',   # state
    'er47012',   # longitudinal weight
    'er43617',   # added to private annuities
    'er43633',   # real estate bought
    'er43647',   # invested in farm/business
    'er43643',   # additions/repairs to real estate
    'er43664',   # stocks bought
    'er43676',   # assets removed by movers out
    'er43692',   # debts added by movers in
    'er43544',   # real estate equity
    'er43622',   # pensions/annuities cashed in
    'er43638',   # real estate sold
    'er43652',   # farm/business sold
    'er43670',   # stocks sold
    'er43681',   # debts removed by movers out
    'er43687',   # assets added by movers in
    'er43698',   # inheritance received
    'er46968'   # imputed wealth without equity 
  )
# 2011
f11.KeepVars <-
  c(
    'er47302',   # family interview number
    'er48878',   # farm/business
    'er52350',   # checking/savings
    'er48883',   # stocks
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
    'er47303',   # state
    'er52436',   # longitudinal weight
    'er48962',   # added to private annuities 
    'er48978',   # real estate bought 
    'er48992',   # invested in farm/business
    'er48988',   # additions/repairs to real estate 
    'er49009',   # stocks bought  
    'er49021',   # assets removed by movers out 
    'er49037',   # debts added by movers in 
    'er48869',   # real estate equity
    'er48967',   # pensions/annuities cashed in 
    'er48983',   # real estate sold
    'er48997',   # farm/business sold
    'er49015',   # stocks sold
    'er49026',   # debts removed by movers out
    'er49032',   # assets added by movers in
    'er49043',   # inheritance received
    'er52392'   # imputed wealth without equity
  )
# 2013
f13.KeepVars <-
  c(
    'er53002',   # family interview number
    'er54625',   # farm/business
    'er58161',   # checking/savings
    'er54634',   # stocks
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
    'er53003',   # state
    'er58257',   # longitudinal weight
    'er54724',   # added to private annuities
    'er54740',   # real estate bought
    'er54754',   # invested in farm/business
    'er54750',   # additions/repairs to real estate
    'er54764',   # stocks bought
    'er54777',   # assets removed by movers out
    'er54793',   # debts added by movers in
    'er58165',   # real estate assets
    'er58167',   # real estate debt (asset-debt = equity)
    'er54729',   # pensions/anuities cashed in
    'er54745',   # real estate sold
    'er54759',   # farm/business sold
    'er54770',   # stocks sold
    'er54782',   # debts removed by movers out
    'er54788',   # assets added by movers in 
    'er54799',   # inheritance received
    'er58209'   # imputed wealth without equity
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
rm(i,years)

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


#keep only the household head from the individual level file
#w <-subset(w,sequenceNum84==01 &
#             sequenceNum89==01 &
#             sequenceNum94==01 &
#             sequenceNum99==01 &
#             sequenceNum01==01 &
#             sequenceNum05==01 &
#             sequenceNum03==01 &
#             sequenceNum07==01 &
#             sequenceNum09==01 &
#             sequenceNum11==01 &
#             sequenceNum13==01)

# create the unbalanced panel:

# merge the family and individual-level files,
# using the interview number field available in both tables

years <- c('84','89','94','99','01','03','05','07','09','11','13')
byx <- c('v10002','v16302','er2002','er13002','er17002','er21002','er25002','er36002','er42002','er47302','er53002')
intNum <-c('er30429','er30606','er33101','er33501','er33601','er33701','er33801','er33901','er34001','er34101','er34201')
sequenceNum <- c( 'er30430','er30607','er33102','er33502','er33602','er33702','er33802','er33902','er34002','er34102','er34202')
j<-1
for(i in years){
  assign(paste("f",i,sep=''),
         merge(eval(as.name(paste("f",i,sep=''))),w,
               by.x=byx[j], by.y=intNum[j]))
  # keep heads only
  assign(paste("f",i,sep=''), subset(eval(as.name(paste('f',i,sep=''))), eval(as.symbol(sequenceNum[j]))==01))
  j <-j+1
}

# create a unique identifier variable for each individual
f84$uniqueID <- (f84$'er30001'*1000) + f84$'er30002'
f89$uniqueID <- (f89$'er30001'*1000) + f89$'er30002'
f94$uniqueID <- (f94$'er30001'*1000) + f94$'er30002'
f99$uniqueID <- (f99$'er30001'*1000) + f99$'er30002'
f01$uniqueID <- (f01$'er30001'*1000) + f01$'er30002'
f03$uniqueID <- (f03$'er30001'*1000) + f03$'er30002'
f05$uniqueID <- (f05$'er30001'*1000) + f05$'er30002'
f07$uniqueID <- (f07$'er30001'*1000) + f07$'er30002'
f09$uniqueID <- (f09$'er30001'*1000) + f09$'er30002'
f11$uniqueID <- (f11$'er30001'*1000) + f11$'er30002'
f13$uniqueID <- (f13$'er30001'*1000) + f13$'er30002'

# temporarily take out individual level files
for(i in years){
  assign(paste('f',i,sep=''),subset(eval(as.name(paste('f',i,sep=''))), select=-c(er30001,er30002,er31997,er31996,er30430,
                                                                                  er30607,er33102,er33502,er33602,er33702,er33802,er33902,er34002,
                                                                                  er34102,er34202,er32000,er30441,er30616,er33111,er33512,er33612,
                                                                                  er33712,er33813,er33913,er34016,er34116,er34216,er30402,er30609,
                                                                                  er33104,er33504,er33604,er33704,er33804,er33904,er34004,er34104,
                                                                                  er34204,er30431,er30608,er33103,er33503,er33603,er33703,er33803,
                                                                                  er33903,er34003,er34103,er34203,er30443,er30620,er33115,er33516,
                                                                                  er33616,er33716,er33817,er33917,er34020,er34119,er34230 )))
}
f84<- subset(f84, select=-c(er30606,er33101,er33501,
                            er33601,er33701,er33801,er33901,er34001,er34101,er34201 ))
f89<- subset(f89, select=-c(er30429,er33101,er33501,
                            er33601,er33701,er33801,er33901,er34001,er34101,er34201 )) 
f94<- subset(f94, select=-c(er30429,er30606,er33501,
                            er33601,er33701,er33801,er33901,er34001,er34101,er34201 )) 
f99<- subset(f99, select=-c(er30429,er30606,er33101,
                            er33601,er33701,er33801,er33901,er34001,er34101,er34201 ))
f01<- subset(f01, select=-c(er30429,er30606,er33101,
                            er33501,er33701,er33801,er33901,er34001,er34101,er34201 ))
f03<- subset(f03, select=-c(er30429,er30606,er33101,
                            er33501,er33601,er33801,er33901,er34001,er34101,er34201 ))
f05<- subset(f05, select=-c(er30429,er30606,er33101,
                            er33501,er33601,er33701,er33901,er34001,er34101,er34201 ))
f07<- subset(f07, select=-c(er30429,er30606,er33101,
                            er33501,er33601,er33701,er33801,er34001,er34101,er34201 ))
f09<- subset(f09, select=-c(er30429,er30606,er33101,
                            er33501,er33601,er33701,er33801,er33901,er34101,er34201 ))
f11<- subset(f11, select=-c(er30429,er30606,er33101,
                            er33501,er33601,er33701,er33801,er33901,er34001,er34201 ))
f13<- subset(f13, select=-c(er30429,er30606,er33101,
                            er33501,er33601,er33701,er33801,er33901,er34101))


#er30429,er30606,er33101,er33501,
er33601,er33701,er33801,er33901,er34001,er34101,er34201
# rename all the individual variables something useful
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
                 'er30443' = "highestSchoolLev84",
                 'er30620' = "highestSchoolLev89",
                 'er33115' = "highestSchoolLev94",
                 'er33516' = "highestSchoolLev99",
                 'er33616' = "highestSchoolLev01",
                 'er33716' = "highestSchoolLev03",
                 'er33817' = "highestSchoolLev05",
                 'er33917' = "highestSchoolLev07",
                 'er34020' = "highestSchoolLev09",
                 'er34119' = "highestSchoolLev11",
                 'er34230' = "highestSchoolLev13"
))

# merge the family files from year to year
for(i in head(years, n=length(years)-1)){
  # two
  j = years[which(years == i)[[1]]+1]
  assign(paste('f',i,j,sep=''),
         merge(eval(as.name(paste('f',i,sep=''))), eval(as.name(paste("f",j,sep=''))), by='uniqueID'))
}


familyPanel <- merge( f84 , w , by.x = 'v10002' , by.y = 'intNum84')
familyPanel <- merge( f89 , familyPanel , by.x = 'v16302' , by.y = 'intNum89')
familyPanel <- merge( f94 , familyPanel , by.x = 'er2002' , by.y = 'intNum94')
familyPanel <- merge( f99 , familyPanel , by.x = 'er13002' , by.y = 'intNum99')
familyPanel <- merge( f01 , familyPanel , by.x = 'er17002' , by.y = 'intNum01')
familyPanel <- merge( f03 , familyPanel , by.x = 'er21002' , by.y = 'intNum03')
familyPanel <- merge( f05 , familyPanel , by.x = 'er25002' , by.y = 'intNum05')
familyPanel <- merge( f07 , familyPanel , by.x = 'er36002' , by.y = 'intNum07')
familyPanel <- merge( f09 , familyPanel , by.x = 'er42002' , by.y = 'intNum09')
familyPanel <- merge( f11 , familyPanel , by.x = 'er47302' , by.y = 'intNum11')
familyPanel <- merge( f13 , familyPanel , by.x = 'er53002' , by.y = 'intNum13')

