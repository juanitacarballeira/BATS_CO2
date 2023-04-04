#######################
#Can we quantify ocean acidification in the subtropical North Atlantic ocean?
#1) Is surface pCO2 increasing?
#2) Is surface ocean water pH decreasing?
#3) Is ocean seawater saturation state with respect to aragonite decreasing?
#######################
#Load Libraries
#install.packages("performance")
#install.packages("see")
#install.packages("tidyverse")
library(performance)
library(tidyverse)
library(seacarb)
#######################

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
#Alk*10^-6

#DIC umol/kg, and we need mol/kg
#CO2*10^-6

#T is in degrees C and we need degrees C
#Temp

#S is in PSS and we will use EOS80
#Sal1

#Pt is in umol/kg and we need mol/kg
#PO41*10^-6

#Sit is in umol/kg and we need mol/kg
#Si1*10^-6

#P_dbar is in dbar and we need bar
#P_dbar*10^-1

#We need to convert units scaling when using CO2SYS
#Lab report
#Intro>Methods>Results>Discussion mini-lab


####continuación 
#flag=15 ALK and DIC given
?carb

bats_co2sys=
  bats_co2%>%
  filter(Alk!=-999,CO2!=-999,Sal1!=-999,Temp!=-999,
         P_dbar!=-999,PO41!=-999,Si1!=-999)%>%
  rename(TotalC=CO2) %>% 
 mutate(carb(flag=15, Alk*10^-6, TotalC*10^-6, 
         S=Sal1, T=Temp,
         Patm=1,P=P_dbar*10^-1,
         Pt=PO41*10^-6, Sit=Si1*10^-6,
         k1k2="l", kf="pf", ks="d", pHscale="T",
         b="u74", gas="potential", 
         warn="y", eos="eos80",long=360-lonW,lat=latN))

#filter for only the surface ocean 
bats_co2sys_surf=bats_co2sys%>%
  filter(Depth<100) #select only upper 100 meters

#1)Is surface ocean pCO2 increasing

bats_co2plot=
  bats_co2sys_surf %>% 
  ggplot()+
  geom_point(mapping=aes(x=decy,y=pCO2insitu))+
  xlab('Time (y)')+
  ylab('pCO2 (uatm)')+
  scale_x_continuous(limits=c(1980,2023,10))+
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color="black"),
      plot.title=element_text(hjust=.5,size=14),
      text = element_text(size=13))+
  geom_smooth(aes(x=decy, y=pCO2insitu),method = "lm")
bats_co2plot

check=bats_co2sys_surf %>% 
  fiter(pCO2<200)
#p<0.05 means its significant

pco2_model=lm(pCO2insitu~decy,data=bats_co2sys_surf)
summary(pco2_model)



#For Thursday
#how to check model performance
#how to plot model predictions

#For homework
#Make decimal year vs pco2 plot pretty
#Could also include methods text for co2 sys lab report



#using pco2 to predict year
#if decimal year first meaning you will predict year using co2


#360-longW=Degrees longitude to the East. In this case is necessary

  
#####Using performance check model made

check_model(pco2_model)
summary(pco2_model)
anova(pco2_model)

#the tidy way with dplyr but we need to rename outputs
bats_co2sys_surf_pred=
bats_co2sys_surf%>%
  mutate(predict(pco2_model,interval='confidence',level=0.95))

#the base R way with cbind and do not need to rename outputs
bats_co2sys_surf_pred=
cbind(bats_co2sys_surf,predict(pco2_model, interval='confidence', level=0,95))

bats_co2sys_surf %>% 
  ggplot()+
  geom_point(mapping=aes(x=decy,y=pCO2insitu))+
  theme_classic()

#There is a seasonal cycle in surface ocean pCO2 at Bats with higher pco2 observed in late summer to early fall and lower pco2 observed i late winter and early spring.Consistent, detectable (p<0.001) annual increase in pco2 by 1.85+-0.07 uatm/ year, 
#also shows plot with model figure caption describes plot (points=data, line=model, shaded region 95% confidence intervals)

##How can we improve pCO2 predictions
#What might be some valuable predictors of stmospheric pco2
#Temperature- measure of atmospheric pco2, also partial pressure pco2 seaewater
#Seasons (month vs. winter)
#Salinity-impacts solubility
#Dissolved oxygen-photosynthesis and respiration impact on DO and CO2
#Saltier water has a lower pCO2
#Years: annual increase atmospheric CO2
#Nutrients: also related to photosynthesis

#create a year and month column
bats_co2sys_surf$year=
  as.numeric(substr(bats_co2sys_surf$yyyymmdd,1,4))
             
bats_co2sys_surf$month=
  as.numeric(substr(bats_co2sys_surf$yyyymmdd,5,6))

view(bats_co2sys_surf)

#we can build many models to fit our data
#more predictors=higher r^2

m1=lm(pCO2insitu~decy,data=bats_co2sys_surf)
m2=lm(pCO2insitu~year+month,data=bats_co2sys_surf)
m3=lm(pCO2insitu~year+month+Temp,data=bats_co2sys_surf)
summary(m1) #r^2=0.3
summary(m2) #r^2=0.4
summary(m3) #r^2=0.7
#AIC(m1,m2,m3) #use AIC to slect better models (lower model=better model)

#bats_surf_sub=
  #bats_co2sys_surf %>% 
  #select(year,month,Temp,Sal1,'O2(1)','NO31','NO21','PO41') %>% 
  #replace_with_na_all(condition=~.x==-999)
#bats_surf_sub=bats_surf_sub[complete.cases(bats_surf_sub),]

bats_surf_sub=
  bats_co2sys_surf%>%
  filter(year!=-999,month!=-999,Sal1!=-999,Temp!=-999,`O2(1)`!=-999,NO31!=-999,NO21!=-999,PO41!=-999)
##Now we all have our predictors without -999 missing data
view(bats_surf_sub)

step(lm(pCO2insitu~1,data=bats_surf_sub),
     direction="forward",
     trace=1,
     scope= ~year+month+Sal1+Temp+ `O2(1)`+`NO31`+ `NO21`+`PO41`)

model_AIC= lm(pCO2insitu~Temp+ year+`NO31`+ Sal1+ `NO21`,data=bats_surf_sub)
summary(model_AIC)
check_model(model_AIC)

                
                
                
                
                
                
                
                
                
                
                
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              

