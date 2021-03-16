library("ggplot2")
library("dplyr")
library("raster")
library ("lubridate")
library("rgdal")
library("tidyverse")

setwd("D:/Research/EFI_Challenge")

EFIData <- read.csv("terrestrial_30min-targets.csv/terrestrial_30min-targets.csv", header = TRUE)

head(EFIData)
#separate data for each site


#data from internet
library(data.table)
mydat <- fread('http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat')
head(mydat)


#Qing's Data Code
###2 For historical dataset
##2.1 Extract inSW and netRad radiation
##step1: put all the .csv in a dataframe

files1 = list.files(pattern = "NEON.D01.BART.DP1.00023.001.000.060.030.SLRNR_30min.*.csv$", full.names = TRUE, recursive = TRUE)

df1 <- read.csv(files1[1],header = TRUE)
df2 <- read.csv(files1[2],header = TRUE)
df <- rbind(df1,df2)
for ( i in 3: length(files1)){
  dfi <- read.csv(files1[i],header = TRUE)
  df <- rbind(df,dfi)
}


