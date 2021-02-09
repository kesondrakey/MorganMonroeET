#this is for AmeriFlux Data
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


#Weekly Average for each week and each year


WeeklyAverage_AllYears <- MMF2 %>% group_by(Start_Week_Year, Start_weekID) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)

View(WeeklyAverage_AllYears)


#if we need to filter year... something like this will work

#Year_2000 <- DATA5 %>% filter(start_week_year == "2000")
#all things are characters
#Year_2000$start_weekID <- as.numeric(Year_2000$start_weekID)


###Begin ET Calculation Here###


###1 Get the data frame from flux tower site used for Gs calculation. 
#read weekly averaged US-MMS flux data. calculated from FLUXNET 2015
#We will use "WeeklyAverage_AllYears"


##1.2 make the data frame
##Tem is degree C (not be used in PM-PET calculation)
df_PM <- data.frame(Lat = df_USMMS$Lat, Year = df_USMMS$Year, Month = df_USMMS$Month, WeekID = df_USMMS$WeekID) 
df_PM$TIMESTAMP_START <- df_USMMS$TIMESTAMP_START
df_PM$Tem <- df_USMMS$TA_F #temperature in degree C
df_PM$Pre <- df_USMMS$P_F * 7 * 0.0394 #Precipitation inch/7day
df_PM$LE <- df_USMMS$LE_F_MDS # w/m2 Latent heat flux, gapfilled using MDS method
df_PM$LE[df_PM$LE <0] <- 0 #PET is zero
df_PM$sw_in <- df_USMMS$SW_IN_F #incoming shortwave radiation
df_PM$sw_out <- df_USMMS$SW_OUT #outgoing shortwave radiation
df_PM$lw_out <- df_USMMS$LW_OUT #outgoing longwave radiation
df_PM$lw_in <- df_USMMS$LW_IN_F #incoming longwave radiation
df_PM$ustar <- df_USMMS$USTAR #Friction velocity
df_PM$Hs <- df_USMMS$H_F_MDS #Sensible heat flux, gapfilled using MDS method
df_PM$U <- df_USMMS$WS_F #Wind speed, consolidated from WS and WS_ERA
#Rn is the difference between the incoming net shortwave (Rns) and the net outgoing longwave (Rnl)
df_PM$Rn <- (df_PM$sw_in - df_PM$sw_out) - (df_PM$lw_out - df_PM$lw_in)  #net short radiation - net long radiation
df_PM$VPD <- df_USMMS$VPD_F * 0.1 #from hPa to Kpa #Vapor Pressure Deficit consolidated from VPD_F_MDS and VPD_ERA
