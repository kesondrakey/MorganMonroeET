#Overall Goal: Calculate ET for the Morgan Monroe Flux Tower; Later goal: Forecast ET with weather forecast data

#Step 1
#Open Data File for the US-MMS Flux Tower (https://ameriflux.lbl.gov/sites/siteinfo/US-MMS)
setwd("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US-MMS_BASE-BADM_18-5")
AMF_US.MMS_BASE_HR_18.5 <- read.csv("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US-MMS_BASE-BADM_18-5/AMF_US-MMS_BASE_HR_18-5.csv", header=FALSE, comment.char="#")

MMF <- AMF_US.MMS_BASE_HR_18.5


#Fix Header
#read only the first row from the table
NAMES <- read.table("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US-MMS_BASE-BADM_18-5/AMF_US-MMS_BASE_HR_18-5.csv", nrow = 1, stringsAsFactors = FALSE, sep = ",")
#skip the first row in the table
DATA <- read.table("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US-MMS_BASE-BADM_18-5/AMF_US-MMS_BASE_HR_18-5.csv", skip = 1, stringsAsFactors = FALSE, sep = ",")
#combine them
names(DATA) <- NAMES
#check
head(DATA)
#data shows a double header, remove first row
DATA <- DATA[- 1, ] 
head(DATA)

#Clone data
MMF <- DATA

#Fix Time issues
#Time looks like 199901010000 (YYYYMMDDHHMM) under TIMESTAMP_START/TIMESTAMP_END
#Turn YYYYMMDDHHMM into YYYYMMDD
MMF$TIMESTAMP_START <- as.Date(as.character(MMF$TIMESTAMP_START), format="%Y%m%d")
MMF$TIMESTAMP_END <- as.Date(as.character(MMF$TIMESTAMP_END), format = "%Y%m%d")

head(MMF)

#Turn -9999 into NAs
MMF[MMF == -9999] <- NA

#Data Type; Most variables are characters
str(MMF)

#Which Variables do we need to calculate ET?
#For ET, We need....
#LE          (W m-2): Latent heat flux
#TA             (deg C): Air temperature;                       Qing's is Ta
#PA             (kPa): Atmospheric pressure;                    Qing's is pres
#USTAR    (m s-1): Friction velocity;                           Qing's is ustar
#H           (W m-2): Sensible heat flux
#NETRAD       (W m-2): Net radiation;                           Qing's is Rn
#VPD        (hPa): Vapor Pressure Deficit; 1hpa = 100pa
#P              (mm): Precipitation
#WS            (m s-1): Wind speed (m/s) (meters per second);    Qing's is u
#Qing's data for wind speed (U) is W/m2 (Watt Per Square Metre); Penman Monteith equation needs windspeed in m/s so will leave as is.

#Morgan Monroe Data:



