#######################
#Can we quantify ocean acidification in the subtropical North Atlantic ocean?
#1) Is surface pCO2 increasing?
#2) Is surface ocean water pH decreasing?
#3) Is ocean seawater saturation state with respect to aragonite decreasing?
#######################
#Load Libraries
library(tidyverse)
library(seacarb)
#######################
library(readr)
bats_bottle <- read_delim("bats_bottle.txt", 
                          delim = "\t", escape_double = FALSE, 
                          col_names = FALSE, trim_ws = TRUE, skip = 60)
#Skip 60, No first row as names, delimiter:Tab
##Import column names from BATS and assign to Data
colnames(bats_bottle)<- colnames(read_csv("bats_bottle.txt",skip=59))
#check its done correctly
View(bats_bottle)
#I have data frame BATS data with column names for variables
######################
#yyyymmdd = Year Month Day   
#decy   = Decimal Year     
#time   = Time (hhmm)      
#latN   = Latitude (Deg N) 
#lonW   = Longitude (Deg W)
#Depth  = Depth (m)                  
#Temp   = Temperature ITS-90 (C)    
#CTD_S  = CTD Salinity (PSS-78)      
#Sal1   = Salinity-1 (PSS-78)        
#Sig-th = Sigma-Theta (kg/m^3)       
#O2(1)  = Oxygen-1 (umol/kg)          
#OxFixT = Oxygen Fix Temp (C)        
#Anom1  = Oxy Anomaly-1 (umol/kg)    
#CO2    = dissolved inorganic carbon (umol/kg)              
#Alk    = Alkalinity (uequiv)        
#NO31   = Nitrate+Nitrite-1 (umol/kg)
#NO21   = Nitrite-1 (umol/kg)        
#PO41   = Phosphate-1 (umol/kg)      
#Si1    = Silicate-1 (umol/kg)       
#POC    = POC (ug/kg)                
#PON    = PON (ug/kg)                
#TOC    = TOC (umol/kg)                
#TN     = TN (umol/kg)  
#Bact   = Bacteria enumeration (cells*10^8/kg)   
#POP    = POP (umol/kg)
#TDP    = Total dissolved Phosphorus (nmol/kg)
#SRP    = Low-level phosphorus (nmol/kg)
#BSi    = Particulate biogenic silica (umol/kg)
#LSi    = Particulate lithogenic silica  (umol/kg)
#Pro    = Prochlorococcus (cells/ml)
#Syn    = Synechococcus (cells/ml)
#Piceu  = Picoeukaryotes (cells/ml)
#Naneu  = Nanoeukaryotes (cells/ml)
####################################
#Calculate CO2 chemistry parameters
?carb

#carb(flag, var1, var2, S=35, T=25, Patm=1, P=0, Pt=0, Sit=0,
     #k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential", 
     #warn="y", eos="eos80", long=1.e20, lat=1.e20)
#We have TA, DIC, T,S, Pt, Sit, but not pressure. 

#First calculate pressure using TEOS-10
?gsw
#p=gsw_p_from_z(z,lat)
bats_co2=
  bats_bottle %>% 
  mutate(P_dbar=gsw_p_from_z(Depth*-1,latN))
View(bats_co2)
#We have all of variables that we need to calculate the surface seawater chemistry at BATS station, but we need to be very careful about our units. 
#We have TA, DIC, T,S, Pt, Sit, Pressure. 
#What are units of these and what does CO2SYS need?
#TA uequiv, which is (umol/kg) and need moles/kg
#DIC umol/kg, and we need mol/kg
#T is in degrees C and we need degrees C
#S is in PSS and we will use EOS80
#Pt is in umol/kh and we need mol/kg
#Sit is in umol/kg and we need mol/kg
#P_dbar is in dbar and we need bar

#We need to convert units scaling when using CO2SYS





