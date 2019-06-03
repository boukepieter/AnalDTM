library(xlsx)
library(spatial)
library(sp)
library(rgdal)
library(raster)
library(plotKML)
library(plyr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(rgeos)
setwd("c:/Users/REACH-IRQ-GIS/Documents/201904 DTM Analysis automatisation")
source("functions.R")

#http://iraqdtm.iom.int/ReturneeML.aspx

##### WATCH OUT CURRENTLY ONLY WORKING (EASILY) FOR COMPARINGWITHDATABASE
##### FOR HELP CONTACT BOUKE PIETER OTTOW ON SKYPE:BOUKEPIETEROTTOW

### Target file
target_file <- "Returnee_MasterList_dataset_DTM_IOMApr 30, 2019.xlsx"
target_sheet <- "Sheet"

### Baseline file
compareWithDatabase <- TRUE
databasefolder <- "database/mid-2018_and_further"
outputfolder <- "output/batch_mid-2018_and_further"
### If False -> NOT WORKING NOW
bline_file <- "IDPs_MasterList_dataset_DTM_IOMFeb 28, 2019.xlsx"
bline_sheet <- "Sheet"


Family_column <- 12 #10 for IDP; 12 for returnee

##### Batch run #####
### Target files
# inputfiles <- list.files("input")
# dates <- as.Date(substr(inputfiles,36,47), format="%b %d, %Y")
# file_order <- order(dates)
# databasefolder <- "database/mid-2018_and_further"
# outputfolder <- "output/batch_mid-2018_and_further"
# compareWithDatabase <- T
# bline_file <- inputfiles[file_order[1]]
# bline_sheet <- "Sheet"

# for (i in 9:length(inputfiles)){
# target_file <- inputfiles[file_order[i]]
# target_sheet <- "Sheet"

##### Prepare environment #####
date <- substr(target_file,nchar(target_file)-16,nchar(target_file)-5)
date <- as.Date(date, format="%b %d, %Y")
if (dir.exists(sprintf('%s/%s',outputfolder,date))==F){dir.create(sprintf('%s/%s',outputfolder,date))}

##### READ IN DATA #####
target <- read.DTM(target_file, target_sheet, columns=Family_column)

if (compareWithDatabase){
  dbase_file <- sort(list.files(databasefolder), decreasing=T)[1]
  bline <- read.csv(paste(databasefolder,dbase_file,sep="/"), stringsAsFactors = F)
} else {
  bline <- read.DTM(bline_file, bline_sheet, columns=Family_column)
}

##### LOCATION LEVEL ANALYSIS #####
DTM.point.analysis(target, bline, outputfolder, date, target_file, databasefolder, 
                   bline.is.dbase=compareWithDatabase)

##### DISTRICT LEVEL ANALYSIS #####
district_shapes <- readOGR("IAU_DIBs_SubDistricts/districts.shp","districts")
projection(district_shapes) <- WGS84

DTM.district.analysis(target, bline, district_shapes, outputfolder, date, target_file, 
                      bline.is.dbase=compareWithDatabase) 

##### SUB-DISTRICT LEVEL ANALYSIS #####
# load in shapes
subdistricts <- readOGR("IAU_DIBs_SubDistricts/irq_polbnda_adm3_500k_UNAMI_PA.shp","irq_polbnda_adm3_500k_UNAMI_PA")
projection(subdistricts) <- WGS84

DTM.subdistrict.analysis(target, bline, subdistricts, outputfolder, date, target_file, 
                         bline.is.dbase=compareWithDatabase) 

