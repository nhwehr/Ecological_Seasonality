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


### GPIR Deer Data
## Prep GPIR Deer Summer Collar Data
# Import Collar Data
GP_Deer <- read.csv('GP_Deer_Collars_Edits.csv') #Grand Portage Deer collar locations
GP_Deer <- st_as_sf(GP_Deer, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=NAD83")
GP_Deer <- st_transform(GP_Deer, 26916)
Coords <- as.data.frame(st_coordinates(GP_Deer$geometry))
GP_Deer$geometry <- NULL
GP_Deer$Longitude <- Coords$X
GP_Deer$Latitude <- Coords$Y
GP_Deer$StudyID <- as.factor(GP_Deer$StudyID)
GP_Deer <- arrange(GP_Deer, StudyID, DateTime)

#Remove Deer that dispersed outside the study area
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_002")
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_014")

# Clean Collar Data for just summer
head(GP_Deer)
GP_Deer$Date <- as.Date(GP_Deer$Date)
GP_Deer$Year <- year(GP_Deer$Date)
table(GP_Deer$Year)
W16 <- GP_Deer[GP_Deer$Date >= "2016-5-8" & GP_Deer$Date <= "2016-11-4",]
W17 <- GP_Deer[GP_Deer$Date >= "2017-5-8" & GP_Deer$Date <= "2017-11-4",]
W18 <- GP_Deer[GP_Deer$Date >= "2018-5-8" & GP_Deer$Date <= "2018-11-4",]
W19 <- GP_Deer[GP_Deer$Date >= "2019-5-8" & GP_Deer$Date <= "2019-11-4",]
W20 <- GP_Deer[GP_Deer$Date >= "2020-5-8" & GP_Deer$Date <= "2020-11-4",]
W21 <- GP_Deer[GP_Deer$Date >= "2021-5-8" & GP_Deer$Date <= "2021-11-4",]
W16$StudyID <- paste(W16$StudyID, W16$Year, sep = '_')
W17$StudyID <- paste(W17$StudyID, W17$Year, sep = '_')
W18$StudyID <- paste(W18$StudyID, W18$Year, sep = '_')
W19$StudyID <- paste(W19$StudyID, W19$Year, sep = '_')
W20$StudyID <- paste(W20$StudyID, W20$Year, sep = '_')
W21$StudyID <- paste(W21$StudyID, W21$Year, sep = '_')
GP_Deer <- rbind(W16, W17, W18, W19, W20, W21)
rm(W16, W17, W18, W19, W20, W21)
table(GP_Deer$StudyID) # shows number of locations per individual
#Deer with <50 locations were removed
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_025_2017") #only 43 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_051_2017") #only 10 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_060_2018") #only 36 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_070_2018") #only 42 locations

# Finalize datasets
length(unique(GP_Deer$StudyID))
GP_Deer_Summer <- GP_Deer
rm(GP_Deer, Coords)

## Prep GPIR Deer Winter Collar Data
# Import Collar Data
GP_Deer <- read.csv('GP_Deer_Collars_Edits.csv') #Grand Portage Deer collar locations
GP_Deer <- st_as_sf(GP_Deer, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=NAD83")
GP_Deer <- st_transform(GP_Deer, 26916)
Coords <- as.data.frame(st_coordinates(GP_Deer$geometry))
GP_Deer$geometry <- NULL
GP_Deer$Longitude <- Coords$X
GP_Deer$Latitude <- Coords$Y
GP_Deer$StudyID <- as.factor(GP_Deer$StudyID)
GP_Deer <- arrange(GP_Deer, StudyID, DateTime)

#Remove Deer that dispersed outside the study area
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_002")
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_014")

# Clean Collar Data for just Winter
head(GP_Deer)
GP_Deer$Date <- as.Date(GP_Deer$Date)
GP_Deer$Year <- year(GP_Deer$Date)
table(GP_Deer$Year)
W15 <- GP_Deer[GP_Deer$Date >= "2016-1-1" & GP_Deer$Date <= "2016-4-9",]
W16 <- GP_Deer[GP_Deer$Date >= "2016-12-3" & GP_Deer$Date <= "2017-4-9",]
W17 <- GP_Deer[GP_Deer$Date >= "2017-12-3" & GP_Deer$Date <= "2018-4-9",]
W18 <- GP_Deer[GP_Deer$Date >= "2018-12-3" & GP_Deer$Date <= "2019-4-9",]
W19 <- GP_Deer[GP_Deer$Date >= "2019-12-3" & GP_Deer$Date <= "2020-4-9",]
W20 <- GP_Deer[GP_Deer$Date >= "2020-12-3" & GP_Deer$Date <= "2021-4-9",]
W21 <- GP_Deer[GP_Deer$Date >= "2021-12-3" & GP_Deer$Date <= "2021-12-31",]
W15$Year <- 2015
W16$Year <- 2016
W17$Year <- 2017
W18$Year <- 2018
W19$Year <- 2019
W20$Year <- 2020
W21$Year <- 2021
W15$StudyID <- paste(W15$StudyID, W15$Year, sep = '_')
W16$StudyID <- paste(W16$StudyID, W16$Year, sep = '_')
W17$StudyID <- paste(W17$StudyID, W17$Year, sep = '_')
W18$StudyID <- paste(W18$StudyID, W18$Year, sep = '_')
W19$StudyID <- paste(W19$StudyID, W19$Year, sep = '_')
W20$StudyID <- paste(W20$StudyID, W20$Year, sep = '_')
W21$StudyID <- paste(W21$StudyID, W21$Year, sep = '_')
GP_Deer <- rbind(W15, W16, W17, W18, W19, W20, W21)
rm(W15, W16, W17, W18, W19, W20, W21)
table(GP_Deer$StudyID) # shows number of locations per individual
#Deer with <50 locations were removed
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_001_2016") #only 3 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_006_2015") #only 9 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_008_2015") #only 23 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_009_2015") #only 22 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_012_2015") #only 18 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_013_2015") #only 18 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_018_2017") #only 9 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_043_2016") #only 9 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_060_2018") #only 32 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_070_2017") #only 47 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_071_2017") #only 20 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_074_2019") #only 30 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_079_2018") #only 30 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_087_2018") #only 6 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_088_2018") #only 24 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_100_2019") #only 48 locations
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_101_2020") #only 5 locations
#Deer causing an "invalid times" error were removed
GP_Deer <- filter(GP_Deer, GP_Deer$StudyID != "GP_Deer_027_2018")

# Finalize Datasets
length(unique(GP_Deer$StudyID))
GP_Deer_Winter <- GP_Deer
rm(GP_Deer, Coords)


### AKDE WRSFs
## Summer Home RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Deer <- GP_Deer_Summer
#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Deer$StudyID))){
  print(Sys.time())
  print(i)
  Deer <- GP_Deer[GP_Deer$StudyID == unique(GP_Deer$StudyID)[i],]
  Deer <- as.telemetry(Deer, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Deer)
  GUESS <- ctmm.guess(Deer, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Deer, GUESS)
  UD <- akde(Deer, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Deer, UD = UD, level.UD = 0.95, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

## Population-level Model
# Population-level Model (Recommended by Alston/Fleming, but it doesn't work)
GP_Deer_Summer_95_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Deer_Summer_95_Model)


## Summer Core RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Deer <- GP_Deer_Summer
#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Deer$StudyID))){
  print(Sys.time())
  print(i)
  Deer <- GP_Deer[GP_Deer$StudyID == unique(GP_Deer$StudyID)[i],]
  Deer <- as.telemetry(Deer, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Deer)
  GUESS <- ctmm.guess(Deer, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Deer, GUESS)
  UD <- akde(Deer, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Deer, UD = UD, level.UD = 0.50, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Deer_Summer_50_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Deer_Summer_50_Model)


## Winter Home RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Deer <- GP_Deer_Winter
#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Deer$StudyID))){
  print(Sys.time())
  print(i)
  Deer <- GP_Deer[GP_Deer$StudyID == unique(GP_Deer$StudyID)[i],]
  Deer <- as.telemetry(Deer, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Deer)
  GUESS <- ctmm.guess(Deer, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Deer, GUESS)
  UD <- akde(Deer, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Deer, UD = UD, level.UD = 0.95, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Deer_Winter_95_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Deer_Winter_95_Model)


## Winter Core RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Deer <- GP_Deer_Winter
#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Deer$StudyID))){
  print(Sys.time())
  print(i)
  Deer <- GP_Deer[GP_Deer$StudyID == unique(GP_Deer$StudyID)[i],]
  Deer <- as.telemetry(Deer, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Deer)
  GUESS <- ctmm.guess(Deer, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Deer, GUESS)
  UD <- akde(Deer, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Deer, UD = UD, level.UD = 0.50, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Deer_Winter_50_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Deer_Winter_50_Model)
