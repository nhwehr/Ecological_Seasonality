##### Ecological Seasonality
### Initial Setup
## R Setup

# Set working directory
setwd('C:\\Users\\natew\\Documents\\All Files\\R Working Directory\\Ecological Seasonality')

# Load Packages 
library(lubridate)
library(dplyr)
library(ctmm)
library(raster)
library(arcgisbinding)
arc.check_product()
library(sf)

## Import Rasters
#Note: All Raster manipulation was done in ArcGIS Pro prior to import

# Aspect
#Aspect was calculated from the USGS elevation map and then recalculated as the cosine of the degree values such that -1 is South and 1 is North and then reprojected to UTMs
Aspect <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Aspect_Cosine_UTM')))
crs(Aspect) <- CRS("+init=epsg:26916")

# Slope 
#Slope was calculated from the USGS elevation map and then reporjected to UTMs
Slope <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Slope_UTM')))
crs(Slope) <- CRS("+init=epsg:26916")

# Distance to Lake Superior
#The shortest distance to Lake Superior was calculated from the NOAA shoreline dataset and reprojected to UTMs
Dist2Superior <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Superior_UTM')))
crs(Dist2Superior) <- CRS("+init=epsg:26916")

# Distance to mainland roads
#Nearest road was calculated from the USCB TIGER roadmap and the OMNRF Ontario Road Network roadmap and reporjected to UTMs
Dist2Roads <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Roads_UTM')))
crs(Dist2Roads) <- CRS("+init=epsg:26916")

# Distance to ISRO trails
#Nearest trail was calculated from the NPS database and reporjected to UTMs
#Not needed for mainland analyses
#Dist2Trails <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Trails_UTM')))
#crs(Dist2Trails) <- CRS("+init=epsg:26916")

# Landcover
#Landcover was pulled from the 2015 CEC landcover database clipped to a reasonable size and reprojected to UTMs
Landcover <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Landcover_UTM')))
crs(Landcover) <- CRS("+init=epsg:26916")
#Generates individual binary rasters for each landcover type of interest and then makes them logical
# Developed/Barren
Other <- raster::calc(Landcover, fun = function(x) ifelse(x %in% c(2, 3, 4, 7, 9, 11, 12, 13, 16, 17, 19), 1, 0))
Other@data@values <- as.logical(Other@data@values)
# Coniferous
Coniferous <- raster::calc(Landcover, fun = function(x) ifelse(x == 1, 1, 0))
Coniferous@data@values <- as.logical(Coniferous@data@values)
# Deciduous
Deciduous <- raster::calc(Landcover, fun = function(x) ifelse(x == 5, 1, 0))
Deciduous@data@values <- as.logical(Deciduous@data@values)
# Mixed
Mixed <- raster::calc(Landcover, fun = function(x) ifelse(x == 6, 1, 0))
Mixed@data@values <- as.logical(Mixed@data@values)
# Shrubland
Shrubland <- raster::calc(Landcover, fun = function(x) ifelse(x %in% c(8, 10, 15), 1, 0))
Shrubland@data@values <- as.logical(Shrubland@data@values)
# Wetland
Wetland <- raster::calc(Landcover, fun = function(x) ifelse(x == 14, 1, 0))
Wetland@data@values <- as.logical(Wetland@data@values)
# Open Water
Water <- raster::calc(Landcover, fun = function(x) ifelse(x == 18, 1, 0))
Water@data@values <- as.logical(Water@data@values)
rm(Landcover)


### GPIR Moose Data
## Prep GPIR Moose Summer Collar Data
# Import Collar Data
GP_Moose <- read.csv('Moose_4hr.csv') #Grand Portage Moose collar locations
GP_Moose <- st_as_sf(GP_Moose, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=NAD83")
GP_Moose <- st_transform(GP_Moose, 26916)
Coords <- as.data.frame(st_coordinates(GP_Moose$geometry))
GP_Moose$geometry <- NULL
GP_Moose$Longitude <- Coords$X
GP_Moose$Latitude <- Coords$Y
GP_Moose$StudyID <- as.factor(GP_Moose$StudyID)
typeof(GP_Moose$CollarID)
GP_Moose$CollarID <- as.character(GP_Moose$CollarID)
GP_Moose <- arrange(GP_Moose, StudyID, DateTime)

# Remove Moose that dispersed outside the study area
#No Moose dispersed outside the study area

# Clean Collar Data for just summer
head(GP_Moose)
GP_Moose$Date <- as.Date(GP_Moose$Date)
GP_Moose$Year <- year(GP_Moose$Date)
table(GP_Moose$Year)
W10 <- GP_Moose[GP_Moose$Date >= "2010-5-21" & GP_Moose$Date <= "2010-10-3",]
W11 <- GP_Moose[GP_Moose$Date >= "2011-5-21" & GP_Moose$Date <= "2011-10-3",]
W12 <- GP_Moose[GP_Moose$Date >= "2012-5-21" & GP_Moose$Date <= "2012-10-3",]
W13 <- GP_Moose[GP_Moose$Date >= "2013-5-21" & GP_Moose$Date <= "2013-10-3",]
W14 <- GP_Moose[GP_Moose$Date >= "2014-5-21" & GP_Moose$Date <= "2014-10-3",]
W15 <- GP_Moose[GP_Moose$Date >= "2015-5-21" & GP_Moose$Date <= "2015-10-3",]
W16 <- GP_Moose[GP_Moose$Date >= "2016-5-21" & GP_Moose$Date <= "2016-10-3",]
W17 <- GP_Moose[GP_Moose$Date >= "2017-5-21" & GP_Moose$Date <= "2017-10-3",]
W18 <- GP_Moose[GP_Moose$Date >= "2018-5-21" & GP_Moose$Date <= "2018-10-3",]
W19 <- GP_Moose[GP_Moose$Date >= "2019-5-21" & GP_Moose$Date <= "2019-10-3",]
W20 <- GP_Moose[GP_Moose$Date >= "2020-5-21" & GP_Moose$Date <= "2020-10-3",]
W21 <- GP_Moose[GP_Moose$Date >= "2021-5-21" & GP_Moose$Date <= "2021-10-3",]
W10$StudyID <- paste(W10$StudyID, W10$Year, sep = '_')
W11$StudyID <- paste(W11$StudyID, W11$Year, sep = '_')
W12$StudyID <- paste(W12$StudyID, W12$Year, sep = '_')
W13$StudyID <- paste(W13$StudyID, W13$Year, sep = '_')
W14$StudyID <- paste(W14$StudyID, W14$Year, sep = '_')
W15$StudyID <- paste(W15$StudyID, W15$Year, sep = '_')
W16$StudyID <- paste(W16$StudyID, W16$Year, sep = '_')
W17$StudyID <- paste(W17$StudyID, W17$Year, sep = '_')
W18$StudyID <- paste(W18$StudyID, W18$Year, sep = '_')
W19$StudyID <- paste(W19$StudyID, W19$Year, sep = '_')
W20$StudyID <- paste(W20$StudyID, W20$Year, sep = '_')
W21$StudyID <- paste(W21$StudyID, W21$Year, sep = '_')
GP_Moose <- rbind(W10, W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21)
rm(W10, W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21)
table(GP_Moose$StudyID) # shows number of locations per individual

# Moose with <50 locations were removed
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_001_2010") #only 30 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_002_2010") #only 5 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_064_2016") #only 13 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_177_2021") #only 12 locations

# Moose with errors were removed
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_014_2012") #causing unknown raster connection error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_072_2020") #causing unknown raster connection error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_185_2021") #causing unknown raster connection error

# Finalize datasets
length(unique(GP_Moose$StudyID))
GP_Moose_Summer <- GP_Moose
rm(GP_Moose, Coords)

## Prep GPIR Moose Winter Collar Data
# Import Collar Data
GP_Moose <- read.csv('Moose_4hr.csv') #Grand Portage Moose collar locations
GP_Moose <- st_as_sf(GP_Moose, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=NAD83")
GP_Moose <- st_transform(GP_Moose, 26916)
Coords <- as.data.frame(st_coordinates(GP_Moose$geometry))
GP_Moose$geometry <- NULL
GP_Moose$Longitude <- Coords$X
GP_Moose$Latitude <- Coords$Y
GP_Moose$StudyID <- as.factor(GP_Moose$StudyID)
GP_Moose <- arrange(GP_Moose, StudyID, DateTime)

# Remove Moose that dispersed outside the study area
#No Moose dispersed outside the study area

# Clean Collar Data for just Winter
head(GP_Moose)
GP_Moose$Date <- as.Date(GP_Moose$Date)
GP_Moose$Year <- year(GP_Moose$Date)
table(GP_Moose$Year)
W09 <- GP_Moose[GP_Moose$Date >= "2010-1-1" & GP_Moose$Date <= "2010-2-24",]
W10 <- GP_Moose[GP_Moose$Date >= "2010-11-1" & GP_Moose$Date <= "2011-2-24",]
W11 <- GP_Moose[GP_Moose$Date >= "2011-11-1" & GP_Moose$Date <= "2012-2-24",]
W12 <- GP_Moose[GP_Moose$Date >= "2012-11-1" & GP_Moose$Date <= "2013-2-24",]
W13 <- GP_Moose[GP_Moose$Date >= "2013-11-1" & GP_Moose$Date <= "2014-2-24",]
W14 <- GP_Moose[GP_Moose$Date >= "2014-11-1" & GP_Moose$Date <= "2015-2-24",]
W15 <- GP_Moose[GP_Moose$Date >= "2015-11-1" & GP_Moose$Date <= "2016-2-24",]
W16 <- GP_Moose[GP_Moose$Date >= "2016-11-1" & GP_Moose$Date <= "2017-2-24",]
W17 <- GP_Moose[GP_Moose$Date >= "2017-11-1" & GP_Moose$Date <= "2018-2-24",]
W18 <- GP_Moose[GP_Moose$Date >= "2018-11-1" & GP_Moose$Date <= "2019-2-24",]
W19 <- GP_Moose[GP_Moose$Date >= "2019-11-1" & GP_Moose$Date <= "2020-2-24",]
W20 <- GP_Moose[GP_Moose$Date >= "2020-11-1" & GP_Moose$Date <= "2021-2-24",]
W21 <- GP_Moose[GP_Moose$Date >= "2021-11-1" & GP_Moose$Date <= "2021-12-31",]
W15$Year <- 2009
W15$Year <- 2010
W15$Year <- 2011
W15$Year <- 2012
W15$Year <- 2013
W15$Year <- 2014
W15$Year <- 2015
W16$Year <- 2016
W17$Year <- 2017
W18$Year <- 2018
W19$Year <- 2019
W20$Year <- 2020
W21$Year <- 2021
W09$StudyID <- paste(W09$StudyID, W09$Year, sep = '_')
W10$StudyID <- paste(W10$StudyID, W10$Year, sep = '_')
W11$StudyID <- paste(W11$StudyID, W11$Year, sep = '_')
W12$StudyID <- paste(W12$StudyID, W12$Year, sep = '_')
W13$StudyID <- paste(W13$StudyID, W13$Year, sep = '_')
W14$StudyID <- paste(W14$StudyID, W14$Year, sep = '_')
W15$StudyID <- paste(W15$StudyID, W15$Year, sep = '_')
W16$StudyID <- paste(W16$StudyID, W16$Year, sep = '_')
W17$StudyID <- paste(W17$StudyID, W17$Year, sep = '_')
W18$StudyID <- paste(W18$StudyID, W18$Year, sep = '_')
W19$StudyID <- paste(W19$StudyID, W19$Year, sep = '_')
W20$StudyID <- paste(W20$StudyID, W20$Year, sep = '_')
W21$StudyID <- paste(W21$StudyID, W21$Year, sep = '_')
GP_Moose <- rbind(W09, W10, W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21)
rm(W09, W10, W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21)
table(GP_Moose$StudyID) # shows number of locations per individual

# Moose with <50 locations were removed
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_018_2015") #only 3 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_032_2013") #only 19 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_034_2013") #only 14 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_065_2020") #only 24 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_106_2019") #only 45 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_134_2018") #only 42 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_135_2018") #only 42 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_136_2018") #only 42 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_137_2018") #only 42 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_138_2018") #only 41 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_139_2018") #only 42 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_140_2018") #only 42 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_141_2018") #only 42 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_142_2018") #only 42 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_143_2018") #only 42 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_146_2019") #only 45 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_146_2021") #only 12 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_148_2019") #only 44 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_149_2019") #only 43 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_150_2019") #only 50 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_151_2019") #only 44 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_152_2019") #only 45 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_153_2019") #only 44 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_154_2019") #only 45 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_155_2019") #only 44 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_157_2019") #only 45 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_160_2019") #only 20 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_162_2019") #only 45 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_163_2019") #only 43 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_164_2019") #only 43 locations
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_165_2019") #only 44 locations

# Moose with errors were removed
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_014_2012") #causing unknown raster connection error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_072_2020") #causing unknown raster connection error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_185_2021") #causing unknown raster connection error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_002_2011") #causing unknown raster connection error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_011_2019") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_011_2020") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_015_2012") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_015_2011") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_021_2016") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_022_2013") #invalid times error 
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_022_2014") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_022_2015") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_029_2013") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_043_2016") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_044_2016") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_046_2015") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_047_2014") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_048_2016") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_101_2016") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_101_2017") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_101_2021") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_102_2016") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_102_2017") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_103_2016") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_110_2020") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_110_2021") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_111_2017") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_113_2017") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_119_2020") #invalid times error
GP_Moose <- filter(GP_Moose, GP_Moose$StudyID != "GP_Moos_120_2019") #invalid times error

# Finalize Datasets
length(unique(GP_Moose$StudyID))
GP_Moose_Winter <- GP_Moose
rm(GP_Moose, Coords)


### AKDE WRSFs
## Summer Home RSFs
Start <- Sys.time()
wRSF_List <- list()
GP_Moose <- GP_Moose_Summer
#the moose analysis is too big for one run, so you have to save at periodic intervals
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Moose_Summer_95.RData")
#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Moose$StudyID))){
  print(Sys.time())
  print(i)
  Moose <- GP_Moose[GP_Moose$StudyID == unique(GP_Moose$StudyID)[i],]
  Moose <- as.telemetry(Moose, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Moose)
  GUESS <- ctmm.guess(Moose, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Moose, GUESS)
  UD <- akde(Moose, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Moose, UD = UD, level.UD = 0.95, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Moose_Summer_95_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Moose_Summer_95_Model)


## Summer Core RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Moose <- GP_Moose_Summer
#the moose analysis is too big for one run, so you have to save at periodic intervals
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Moose_Summer_50.RData")
#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Moose$StudyID))){
  print(Sys.time())
  print(i)
  Moose <- GP_Moose[GP_Moose$StudyID == unique(GP_Moose$StudyID)[i],]
  Moose <- as.telemetry(Moose, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Moose)
  GUESS <- ctmm.guess(Moose, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Moose, GUESS)
  UD <- akde(Moose, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Moose, UD = UD, level.UD = 0.50, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Moose_Summer_50_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Moose_Summer_50_Model)


## Winter Home RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Moose <- GP_Moose_Winter
#the moose analysis is too big for one run, so you have to save at periodic intervals
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Moose_Winter_95.RData")
#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Moose$StudyID))){
  print(Sys.time())
  print(i)
  Moose <- GP_Moose[GP_Moose$StudyID == unique(GP_Moose$StudyID)[i],]
  Moose <- as.telemetry(Moose, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Moose)
  GUESS <- ctmm.guess(Moose, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Moose, GUESS)
  UD <- akde(Moose, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Moose, UD = UD, level.UD = 0.95, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Moose_Winter_95_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Moose_Winter_95_Model)


## Winter Core RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Moose <- GP_Moose_Winter
#the moose analysis is too big for one run, so you have to save at periodic intervals
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Moose_Winter_50.RData")
#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Moose$StudyID))){
  print(Sys.time())
  print(i)
  Moose <- GP_Moose[GP_Moose$StudyID == unique(GP_Moose$StudyID)[i],]
  Moose <- as.telemetry(Moose, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Moose)
  GUESS <- ctmm.guess(Moose, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Moose, GUESS)
  UD <- akde(Moose, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Moose, UD = UD, level.UD = 0.50, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Moose_Winter_50_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Moose_Winter_50_Model)
