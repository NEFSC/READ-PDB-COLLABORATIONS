# setup keyring and test with an oracle pull
# min-yang initial example
# liz modified august 2025

################################################################################################################

# Step 0: install the keyring package in R

install.packages("keyring")
library("keyring") 
stopifnot(has_keyring_support()==TRUE)

library(keyring)
library(DBI)
library(ROracle)

################################################################################################################

# Step 1: **key_set()**  Set your credentials and dbconnect arguments.
# the key_set command will open a dialog box, 
# prompting you to enter the value to set each key to.
# Note: the box will always say "Passowrd" regardless of what you are setting
key_set("nefsc_user")  # enter your oracle username
key_set("nefsc_pw")    # enter your oracle password


# save the service name, port, and host
# if you don't know these values, open SQL_Developer and expand TNS 
##   (see yellow highlighting in *SQL_Developer_View.png*)
# 1 - expand TNS
# 2 - right click on one of the connections and scroll down to 'Properties' (bottom of that list of options); 
##  the info you need will be in the bottom 'Details' of the pop-up window (see *OracleConnection_Fields.png*)

key_set("nefsc_host")    
key_set("nefsc_port")    
key_set("nefsc_servicename") 

# ! Note: when you change your oracle password, you have to update that with key_set() again:
# key_set("nefsc_pw")  # enter your updated password, if it has changed

################################################################################################################

# Step 2: **key_get()** Test (make sure these return the expected information)
key_get("nefsc_user")  
key_get("nefsc_pw")    
key_get("nefsc_host")  
key_get("nefsc_port")   
key_get("nefsc_servicename")  


################################################################################################################

# Step 3: You would start from here from now on, unless you need to reset any of the values.


#====== get the values in the keyring  ====================

nefsc_pw <- key_get("nefsc_pw")
nefsc_user <- key_get('nefsc_user') 
nefsc_host <- key_get("nefsc_host")
nefsc_port <- key_get("nefsc_port")
nefsc_servicename <- key_get("nefsc_servicename")




#====== Test a pull from survey  ==========================

mode.schema<-"FSCS2"

sole <- paste0("(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=", nefsc_host ,")",
               "(PORT=", nefsc_port, "))(CONNECT_DATA=(SERVICE_NAME =", nefsc_servicename, ")))")


oracle.login <- ROracle::dbConnect(dbDriver("Oracle"), 
                                   username = nefsc_user, 
                                   password = nefsc_pw, 
                                   dbname = sole)



lw.units=dbGetQuery(oracle.login, paste0("select * ",
                                              "from ",mode.schema,".nefsc_species_primary_lw_meas"))


haddock <- which(substr(lw.units$SPECIES_NAME, 1,13)=='Melanogrammus')
# haddock <- which(substr(lw.units$SPECIES_NAME, 1,24)=='Melanogrammus aegelfinus')
lw.units[haddock,]

cod <- which(substr(lw.units$SPECIES_NAME, 1,5)=='Gadus')
lw.units[cod,]

yt <- which(substr(lw.units$SPECIES_NAME, 1,7)=='Limanda')
lw.units[yt,]

pk <- which(substr(lw.units$SPECIES_NAME, 1,10)=='Pollachius')
lw.units[pk,]

pl <- which(substr(lw.units$SPECIES_NAME, 1,15)=='Hippoglossoides')
lw.units[pl,]

witch <- which(substr(lw.units$SPECIES_NAME, 1,14)=='Glyptocephalus')
lw.units[witch,]

winter <- which(substr(lw.units$SPECIES_NAME, 1,18)=='Pseudopleuronectes')
lw.units[winter,]

summer <- which(substr(lw.units$SPECIES_NAME, 1,12)=='Paralichthys')
lw.units[summer,]

halibut <- which(substr(lw.units$SPECIES_NAME, 1,12)=='Hippoglossus')
lw.units[halibut,]


#====== Test a pull from stockeff  ==========================

mode.schema<-"STOCKEFF"

sole <- paste0("(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=", nefsc_host ,")",
               "(PORT=", nefsc_port, "))(CONNECT_DATA=(SERVICE_NAME =", nefsc_servicename, ")))")


oracle.login <- ROracle::dbConnect(dbDriver("Oracle"), 
                                   username = nefsc_user, 
                                   password = nefsc_pw, 
                                   dbname = sole)



max.year.pull=dbGetQuery(oracle.login, paste0("select max(year) ",
                                             "from ",mode.schema,".mv_cf_landings"))

max.year.pull





