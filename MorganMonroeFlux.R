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
MMF1 <- MMF

#Which Variables do we need to calculate ET?
#For ET, We need....
#LE             (W m-2): Latent heat flux 
#TA             (deg C): Air temperature
#PA             (kPa): Atmospheric pressure
#USTAR          (m s-1): Friction velocity
#H              (W m-2): Sensible heat flux
#NETRAD         (W m-2): Net radiation
#VPD            (hPa): Vapor Pressure Deficit
#P              (mm): Precipitation
#WS             (m s-1): Wind speed

#Change data from character to numeric for relevant variables
MMF1$LE <- as.numeric(MMF1$LE_1_1_1)

#error, different types of TA data?
MMF1$TA <- as.numeric(MMF1$Ta)


MMF1$PA <- as.numeric(MMF1$PA)
MMF1$USTAR <- as.numeric(MMF1$USTAR)
MMF1$H <- as.numeric(MMF1$H)
MMF1$NETRAD <- as.numeric(MMF1$NETRAD)
MMF1$VPD <- as.numeric(MMF1$VPD)
MMF1$P <- as.numeric(MMF1$P)
MMF1$WS <- as.numeric(MMF1$WS)




