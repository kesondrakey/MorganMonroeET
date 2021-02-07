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

#error, different types of data??? Doing *_1_1_1 for now, not sure which one to use
MMF1$TA <- as.numeric(MMF1$TA_1_1_1)
MMF1$PA <- as.numeric(MMF1$PA_1_1_1)
MMF1$USTAR <- as.numeric(MMF1$USTAR_1_1_1)
MMF1$H <- as.numeric(MMF1$H_1_1_1)
MMF1$NETRAD <- as.numeric(MMF1$NETRAD_1_1_1)
MMF1$VPD <- as.numeric(MMF1$VPD_PI_1_1_1) #Not sure what VPD_PI is... 
MMF1$P <- as.numeric(MMF1$P_1_1_1)
MMF1$WS <- as.numeric(MMF1$WS_1_1_1)

##add column for start_year and start_month; will use this for now
MMF1[, "Start_Year"] <- format(MMF1[,"TIMESTAMP_START"], "%Y")
MMF1[, "Start_Month"] <- format(MMF1[,"TIMESTAMP_START"], "%m")
##add column for end_year and end_month
MMF1[, "End_Year"] <- format(MMF1[,"TIMESTAMP_END"], "%Y")
MMF1[, "End_Month"] <- format(MMF1[,"TIMESTAMP_END"], "%m")
head(MMF1) #works

#add columns for weeks
##add column for start week year and week ID for each year
#Data starts on 01-01-1999 starts on a Friday; not sure how to fix this yet 

MMF1[, "Start_Week_Year"] <- format(MMF1[,"TIMESTAMP_START"], "%G")
MMF1[, "Start_weekID"] <- format(MMF1[,"TIMESTAMP_START"], "%V")

MMF2 <- MMF1

#Begin averaging
library("ggplot2")
library("dplyr")
library("raster")
library ("lubridate")
library("rgdal")
library("tidyverse")

#mean function
mean_na <- function(x) {
  mean(x,na.rm=T)
}


###These functions aren't working ######

#more than 50 warnings? doesnt work
MMFYear <- MMF2 %>% group_by(Start_Year) %>%       
  summarise_at(.vars = names(.)[1:50],.funs = mean,na.rm=TRUE)

head(MMFYear)

#mean by month
MMFMonth <-MMF2 %>% group_by(Start_Month) %>%       
  summarise_at(.vars = names(.)[1:50],.funs = mean,na.rm=TRUE)

head(MMFMonth)

#mean by week - average each numbers week for ALL years
MMFWeek <- MMF2 %>% group_by(Start_weekID) %>%       
  summarise_at(.vars = names(.)[1:50],.funs = mean,na.rm=TRUE)

head(MMFWeek)
