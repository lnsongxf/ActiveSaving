#Load the PSID data necessary to compute active savings, combine with the wealth supplement, rename variables.

# load the individual cross-year file
load( "ind.rda" )

# limit the file to only the variables needed
ind.KeepVars <-
  c('er30001' ,   # 1968 interview number
    'er30002' ,   # 1968 person number
    'er31997' ,    # primary sampling unit variable 
    'er31996' ,    # stratification variable
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


# 1985 - just whether moved
f85.KeepVars <-
  c(
    'v11102',   # family interview number 
    'v11628'    # moved? 
  )
# 1986 - just whether moved
f86.KeepVars <-
  c(
    'v12502',   # family interview number 
    'v13037'    # moved?
  )
# 1987 - just whether moved
f87.KeepVars <-
  c(
    'v13702',   # family interview number  
    'v14140'    # moved? 
  )
# 1988 - just whether moved
f88.KeepVars <-
  c(
    'v14802',   # family interview number 
    'v15148'    # moved? 
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
    'v17389',   # total wealth 89
    'v17387'   # value of all other inheritances
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

# 1990 - just whether moved
f90.KeepVars <-
  c(
    'v17702',   # family interview number 
    'v18087'    # moved? 
  )

# 1991 - just whether moved
f91.KeepVars <-
  c(
    'v19002',   # family interview number 
    'v19387'    # moved? 
  )

# 1992 - just whether moved
f92.KeepVars <-
  c(
    'v20302',   # family interview number 
    'v20687'    # moved? 
  )

# 1993 - just whether moved
f93.KeepVars <-
  c(
    'v21602',   # family interview number 
    'v22441'    # moved? 
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
    'er3838',   # inheritance recieved
    'er3843',   # 2nd inheritance
    'er3848'   # 3rd inheritance 
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

# 1995 - just whether moved
f95.KeepVars <-
  c(
    'er5002',   # family interview number
    'er5061'    # moved?  
  )

# 1996 - just whether moved
f96.KeepVars <-
  c(
    'er7002',   # family interview number 
    'er7155'    # moved?
  )

# 1997 - just whether moved
f97.KeepVars <-
  c(
    'er10002',   # family interview number 
    'er10072'    # moved?
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
    'er15117',   # inheritance received
    'er15122',   # 2nd inheritance
    'er15127'   # 3rd inheritance
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
    'er19313',   # inheritance received
    'er19318',   # 2nd inheritance
    'er19323'   # 3rd inheritance  
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
    'er22708',   # inheritance received
    'er22713',   # 2nd inheritance
    'er22718'   # 3rd inheritance 
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
    'er26689',   # inheritance received
    'er26694',   # 2nd inheritance
    'er26699'   # 3rd inheritance
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
    'er37707',   # inheritance received
    'er37712',   # 2nd inheritance
    'er37717'   # 3rd inheritance
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
    'er46968',   # imputed wealth without equity 
    'er43703',   # 2nd inheritance
    'er43708'   # 3rd inheritance
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
    'er52392',   # imputed wealth without equity
    'er49048',   # 2nd inheritance
    'er49053'   # 3rd inheritance
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
    'er58209',   # imputed wealth without equity
    'er54804',   # 2nd inheritance
    'er54809'   # 3rd inheritance
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
rm(f84.KeepVars, f89.KeepVars, f94.KeepVars, f99.KeepVars, f01.KeepVars,
   f03.KeepVars, f05.KeepVars, f07.KeepVars, f09.KeepVars, f11.KeepVars, f13.KeepVars,
   w84.KeepVars, w89.KeepVars, w94.KeepVars, w99.KeepVars, w01.KeepVars, w03.KeepVars,
   w05.KeepVars, w07.KeepVars, ind.KeepVars) ; gc()
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

# rename variables something useful

f84 <- rename(f84, c('v10002' = 'intNum84',
                     'v10908' = 'farmBusiness84',
                     'v10913' = 'stocks84',
                     'v10903' = 'vehicles84',
                     'v10020' = 'mortgageDebt84',
                     'v10923' = 'othAssets84',
                     'v10447' = 'wtrMoved84',
                     'v10018' = 'houseValue84',
                     'S105' = 'checkingAccount84',
                     'S107'= 'othDebt84',
                     'S120'= 'homeEquity84',
                     'S117'= 'impWealthWE84',
                     'v10418' = 'numInFam84',
                     'v11022' = 'famIncome84',
                     'v10003' = 'state84',
                     'v11079' = 'longWeight84',
                     'v10899'  = 'realEstateEquity84',
                     'S116' = 'impWealthWOE84'))
f89 <- rename(f89, c('v16302' = 'intNum89',
                     'v17323' = 'farmBusiness89',
                     'v17326' = 'stocks89',
                     'v17320' = 'vehicles89',
                     'v16326' = 'mortgageDebt89',
                     'v17332' = 'othAssets89',
                     'v16649' = 'wtrMoved89',
                     'v16324' = 'houseValue89',
                     'S205' = 'checkingAccount89',
                     'S207' = 'othDebt89',
                     'S220' = 'homeEquity89',
                     'S217'= 'impWealthWE89',
                     'v16630' = 'numInFam89',
                     'v17533' = 'famIncome89',
                     'v16303' = 'state89',
                     'v17612' = 'longWeight89',
                     'v17371' = 'assetsRemoved89',
                     'v17379'  = 'debtsAdded89',
                     'v17318'  = 'realEstateEquity89',
                     'v17343' = 'pensionsCashed89',
                     'v17349' = 'realEstateSold89',
                     'v17358' = 'farmBusinessSold89',
                     'v17368' = 'stocksSold89',
                     'v17373' = 'debtsRemoved89',
                     'v17377' = 'assetsAdded89',
                     'v17384' = 'inheritanceReceived89',
                     'S216' = 'impWealthWOE89',
                     'v17609' = 'totalWealth84',
                     'v17389' = 'totalWealth89',
                     'v17340' =  'annuityAdd89',
                     'v17346' = 'realEstateBought89',
                     'v17355' = 'investFarmBusiness89',
                     'v17352' = 'realEstateImprovement89',
                     'v17365' = 'stocksPurchased89',
                     'v17610' = 'activeSaving',
                     'v17387' = 'allOthInheritance89'))
f94 <- rename(f94, c('er2002' = 'intNum94',
                     'er3731' = 'farmBusiness94',
                     'er3736' = 'stocks94',
                     'er3726' = 'vehicles94',
                     'er2037' = 'mortgageDebt94',
                     'er3748' = 'othAssets94',
                     'er2062' = 'wtrMoved94',
                     'er2033' = 'houseValue94',
                     'S305' = 'checkingAccount94',
                     'S307' = 'othDebt94',
                     'S320' = 'homeEquity94',
                     'S317'= 'impWealthWE94',
                     'er2006' = 'numInFam94',
                     'er4153' = 'famIncome94',
                     'er4156' = 'state94',
                     'er4160' = 'longWeight94',
                     'er3758' =  'annuityAdd94',
                     'er3774' = 'realEstateBought94',
                     'er3788' = 'investFarmBusiness94',
                     'er3784' = 'realEstateImprovement94',
                     'er3805' = 'stocksPurchased94',
                     'er3817' = 'assetsRemoved94',
                     'er3832'  = 'debtsAdded94',
                     'er3722'  = 'realEstateEquity94',
                     'er3763' = 'pensionsCashed94',
                     'er3779' = 'realEstateSold94',
                     'er3793' = 'farmBusinessSold94',
                     'er3811' = 'stocksSold94',
                     'er3822' = 'debtsRemoved94',
                     'er3827' = 'assetsAdded94',
                     'er3838' = 'inheritanceReceived94',
                     'S316' = 'impWealthWOE94',
                     'er3843' = 'secondInheritance94',
                     'er3848' = 'thirdInheritance94'))
f99 <- rename(f99, c('er13002' = 'intNum99',
                     'er15002' = 'farmBusiness99',
                     'er15007' = 'stocks99',
                     'er14997' = 'vehicles99',
                     'er13047' = 'mortgageDebt99',
                     'er15026' = 'othAssets99',
                     'er13077' = 'wtrMoved99',
                     'er13041' = 'houseValue99',
                     'S405' = 'checkingAccount99',
                     'S407' = 'othDebt99',
                     'S420' = 'homeEquity99',
                     'S417'= 'impWealthWE99',
                     'er13009' = 'numInFam99',
                     'er16462' = 'famIncome99',
                     'er13004' = 'state99',
                     'er16518' = 'longWeight99',
                     'er15036' =  'annuityAdd99',
                     'er15052' = 'realEstateBought99',
                     'er15066' = 'investFarmBusiness99',
                     'er15062' = 'realEstateImprovement99',
                     'er15083' = 'stocksPurchased99',
                     'er15095' = 'assetsRemoved99',
                     'er15111' = 'debtsAdded99',
                     'er14993' = 'realEstateEquity99',
                     'er15041' = 'pensionsCashed99',
                     'er15057' = 'realEstateSold99',
                     'er15071' = 'farmBusinessSold99',
                     'er15089' = 'stocksSold99',
                     'er15100' = 'debtsRemoved99',
                     'er15106' = 'assetsAdded99',
                     'er15117' = 'inheritanceReceived99',
                     'S416' = 'impWealthWOE99',
                     'er15122' = 'secondInheritance99',
                     'er15127' = 'thirdInheritance99'))                     
f01 <- rename(f01, c('er17002' = 'intNum01', 
                     'er19198' = 'farmBusiness01',
                     'er19203' = 'stocks01',
                     'er19193' = 'vehicles01',
                     'er17052' = 'mortgageDebt01',
                     'er19222' = 'othAssets01',
                     'er17088' = 'wtrMoved01',
                     'er17044' = 'houseValue01',
                     'S505' = 'checkingAccount01',
                     'S507' = 'othDebt01',
                     'S520' = 'homeEquity01',
                     'S517'= 'impWealthWE01',
                     'er17012' = 'numInFam01',
                     'er20456' = 'famIncome01',
                     'er17004' = 'state01',
                     'er20394' = 'longWeight01',
                     'er19232' =  'annuityAdd01',
                     'er19248' = 'realEstateBought01',
                     'er19262' = 'investFarmBusiness01', 
                     'er19258' = 'realEstateImprovement01',
                     'er19279' = 'stocksPurchased01',
                     'er19291' = 'assetsRemoved01',
                     'er19307' = 'debtsAdded01',
                     'er19189' = 'realEstateEquity01',
                     'er19237' = 'pensionsCashed01',
                     'er19253' = 'realEstateSold01',
                     'er19267' = 'farmBusinessSold01',
                     'er19285' = 'stocksSold01',
                     'er19296' = 'debtsRemoved01',
                     'er19302' = 'assetsAdded01',
                     'er19313' = 'inheritanceReceived01',
                     'S516' = 'impWealthWOE01',
                     'er19318' = 'secondInheritance01',
                     'er19323' = 'thirdInheritance01'))
f03 <- rename(f03, c('er21002' = 'intNum03',
                     'er22563' = 'farmBusiness03',
                     'er22568' = 'stocks03',
                     'er22558' = 'vehicles03',
                     'er21051' = 'mortgageDebt03', 
                     'er22617' = 'othAssets03', 
                     'er21117' = 'wtrMoved03',
                     'er21043' = 'houseValue03',
                     'S605' = 'checkingAccount03',
                     'S607' = 'othDebt03',
                     'S620' = 'homeEquity03',
                     'S617'= 'impWealthWE03',
                     'er21016' = 'numInFam03',
                     'er24099' = 'famIncome03',
                     'er21003' = 'state03',
                     'er24179' = 'longWeight03',
                     'er22627' =  'annuityAdd03',
                     'er22643' = 'realEstateBought03',
                     'er22657' = 'investFarmBusiness03',
                     'er22653' = 'realEstateImprovement03',
                     'er22674' = 'stocksPurchased03',
                     'er22686' = 'assetsRemoved03',
                     'er22702' = 'debtsAdded03',
                     'er22554' = 'realEstateEquity03',
                     'er22632' = 'pensionsCashed03',
                     'er22648' = 'realEstateSold03',
                     'er22662' = 'farmBusinessSold03',
                     'er22680' = 'stocksSold03',
                     'er22691' = 'debtsRemoved03',
                     'er22697' = 'assetsAdded03',
                     'er22708' = 'inheritanceReceived03',
                     'S616' = 'impWealthWOE03',
                     'er22713' = 'secondInheritance03',
                     'er22718' = 'thirdInheritance03'))
f05 <- rename(f05, c('er25002' = 'intNum05',
                     'er26544' = 'farmBusiness05',
                     'er26549' = 'stocks05',
                     'er26539' = 'vehicles05',
                     'er25042' = 'mortgageDebt05',
                     'er26598' = 'othAssets05',
                     'er25098' = 'wtrMoved05',
                     'er25029' = 'houseValue05',
                     'S705' = 'checkingAccount05',
                     'S707' = 'othDebt05',
                     'S720' = 'homeEquity05',
                     'S717'= 'impWealthWE05',
                     'er25016' = 'numInFam05',
                     'er28037' = 'famIncome05',
                     'er25003' = 'state05',
                     'er28078' = 'longWeight05',
                     'er26608' =  'annuityAdd05',
                     'er26624' = 'realEstateBought05',
                     'er26638' = 'investFarmBusiness05',
                     'er26634' = 'realEstateImprovement05',
                     'er26655' = 'stocksPurchased05',
                     'er26667' = 'assetsRemoved05',
                     'er26683' = 'debtsAdded05',
                     'er26535' = 'realEstateEquity05',
                     'er26613' = 'pensionsCashed05',
                     'er26629' = 'realEstateSold05',
                     'er26643' = 'farmBusinessSold05',
                     'er26661' = 'stocksSold05',
                     'er26672' = 'debtsRemoved05',
                     'er26678' = 'assetsAdded05',
                     'er26689' = 'inheritanceReceived05',
                     'S716' = 'impWealthWOE05',
                     'er26694' = 'secondInheritance05',
                     'er26699' = 'thirdInheritance05'))
f07 <- rename(f07, c('er36002' = 'intNum07',
                     'er37562' = 'farmBusiness07', 
                     'er37567' = 'stocks07',
                     'er37557' = 'vehicles07',
                     'er36042' = 'mortgageDebt07',
                     'er37616' = 'othAssets07',
                     'er36103' = 'wtrMoved07',
                     'er36029' = 'houseValue07',
                     'S805' = 'checkingAccount07',
                     'S807' = 'othDebt07',
                     'S820' = 'homeEquity07',
                     'S817'= 'impWealthWE07',
                     'er36016' = 'numInFam07',
                     'er41027' = 'famIncome07',
                     'er36003' = 'state07',
                     'er41069' = 'longWeight07',
                     'er37626' =  'annuityAdd07',
                     'er37642' = 'realEstateBought07',
                     'er37656' = 'investFarmBusiness07',
                     'er37652' = 'realEstateImprovement07',
                     'er37673' = 'stocksPurchased07',
                     'er37685' = 'assetsRemoved07',
                     'er37701' = 'debtsAdded07',
                     'er37553' = 'realEstateEquity07',
                     'er37631' = 'pensionsCashed07',
                     'er37647' = 'realEstateSold07',
                     'er37661' = 'farmBusinessSold07',
                     'er37679' = 'stocksSold07',
                     'er37690' = 'debtsRemoved07',
                     'er37696' = 'assetsAdded07',
                     'er37707' = 'inheritanceReceived07',
                     'S816' = 'impWealthWOE07',
                     'er37712' = 'secondInheritance07',
                     'er37717' = 'thirdInheritance07'))
f09 <- rename(f09, c('er42002' = 'intNum09',
                     'er43553' = 'farmBusiness09',
                     'er46942' = 'checkingAccount09',
                     'er43558' = 'stocks09',
                     'er46966' = 'homeEquity09',
                     'er43548' = 'vehicles09',
                     'er42043' = 'mortgageDebt09',
                     'er43607' = 'othAssets09',
                     'er42132' = 'wtrMoved09',
                     'er43612' = 'othDebt09',
                     'er42030' = 'houseValue09',
                     'er46970'= 'impWealthWE09',
                     'er42016' = 'numInFam09',
                     'er46935' = 'famIncome09',
                     'er42003' = 'state09',
                     'er47012' = 'longWeight09',
                     'er43617' =  'annuityAdd09',
                     'er43633' = 'realEstateBought09',
                     'er43647' = 'investFarmBusiness09',
                     'er43643' = 'realEstateImprovement09',
                     'er43664' = 'stocksPurchased09',
                     'er43676' = 'assetsRemoved09',
                     'er43692' = 'debtsAdded09',
                     'er43544' = 'realEstateEquity09',
                     'er43622' = 'pensionsCashed09',
                     'er43638' = 'realEstateSold09',
                     'er43652' = 'farmBusinessSold09',
                     'er43670' = 'stocksSold09',
                     'er43681' = 'debtsRemoved09',
                     'er43687' = 'assetsAdded09',
                     'er43698' = 'inheritanceReceived09',
                     'er46968' = 'impWealthWOE09',
                     'er43703' = 'secondInheritance09',
                     'er43708' = 'thirdInheritance09'))
f11 <- rename(f11, c('er47302' = 'intNum11',
                     'er48878' = 'farmBusiness11',
                     'er52350' = 'checkingAccount11',
                     'er48883' = 'stocks11',
                     'er52390' = 'homeEquity11',
                     'er48873' = 'vehicles11',
                     'er47348' = 'mortgageDebt11',
                     'er48932' = 'othAssets11',
                     'er47440' = 'wtrMoved11',
                     'er47330' = 'houseValue11',
                     'er52372' = 'cCardDebt11',
                     'er48945' = 'studentDebt11',
                     'er52380' = 'medicalDebt11',
                     'er52384' = 'legalDebt11',
                     'er52376' = 'loansFromRelatives11',
                     'er52394'= 'impWealthWE11',
                     'er47316' = 'numInFam11',
                     'er52343' = 'famIncome11',
                     'er47303' = 'state11',
                     'er52436' = 'longWeight11',
                     'er48962' =  'annuityAdd11',
                     'er48978' = 'realEstateBought11', 
                     'er48992' = 'investFarmBusiness11',
                     'er48988' = 'realEstateImprovement11',
                     'er49009' = 'stocksPurchased11',
                     'er49021' = 'assetsRemoved11',
                     'er49037' = 'debtsAdded11',
                     'er48869' = 'realEstateEquity11',
                     'er48967' = 'pensionsCashed11',
                     'er48983' = 'realEstateSold11',
                     'er48997' = 'farmBusinessSold11',
                     'er49015' = 'stocksSold11',
                     'er49026' = 'debtsRemoved11',
                     'er49032' = 'assetsAdded11',
                     'er49043' = 'inheritanceReceived11',
                     'er52392' = 'impWealthWOE11',
                     'er49048' = 'secondInheritance11',
                     'er49053' = 'thirdInheritance11'))
f13 <- rename(f13, c('er53002' = 'intNum13',
                     'er54625' = 'farmBusiness13',
                     'er58161' = 'checkingSavings13',
                     'er54634' = 'stocks13',
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
                     'er58211' = 'impWealthWE13',
                     'er53016' = 'numInFam13',
                     'er58152' = 'famIncome13',
                     'er53003' = 'state13',
                     'er58257' = 'longWeight13',
                     'er54724' =  'annuityAdd13',
                     'er54740' = 'realEstateBought13',
                     'er54754' = 'investFarmBusiness13',
                     'er54750' = 'realEstateImprovement13',
                     'er54764' = 'stocksPurchased13',
                     'er54777' = 'assetsRemoved13',
                     'er54793' = 'debtsAdded13',
                     'er58165' = 'realEstateAssets13',
                     'er58167' = 'realEstateDebt13',
                     'er54729' = 'pensionsCashed13',
                     'er54745' = 'realEstateSold13',
                     'er54759' = 'farmBusinessSold13',
                     'er54770' = 'stocksSold13',
                     'er54782' = 'debtsRemoved13',
                     'er54788' = 'assetsAdded13',
                     'er54799' = 'inheritanceReceived13',
                     'er58209' = 'impWealthWOE13',
                     'er54804' = 'secondInheritance13',
                     'er54809' = 'thirdInheritance13'))
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