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


### GPIR Wolf Data
## Prep GPIR Wolf Summer Collar Data
# Import Collar Data
GP_Wolf <- read.csv('GP_wolf_Collars_edits.csv') #Grand Portage Wolf collar locations
GP_Wolf <- st_as_sf(GP_Wolf, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=NAD83")
GP_Wolf <- st_transform(GP_Wolf, 26916)
Coords <- as.data.frame(st_coordinates(GP_Wolf$geometry))
GP_Wolf$geometry <- NULL
GP_Wolf$Longitude <- Coords$X
GP_Wolf$Latitude <- Coords$Y
GP_Wolf$StudyID <- as.factor(GP_Wolf$StudyID)
GP_Wolf$CollarID <- as.character(GP_Wolf$CollarID)
GP_Wolf <- arrange(GP_Wolf, StudyID, DateTime)

# Remove Wolf that dispersed outside the study area
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2014-001")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2014-003")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2014-008")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2015-008")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2017-004")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2021-001")

# Clean Collar Data for just summer
head(GP_Wolf)
GP_Wolf$Date <- as.Date(GP_Wolf$Date)
GP_Wolf$Year <- year(GP_Wolf$Date)
table(GP_Wolf$Year)
W12 <- GP_Wolf[GP_Wolf$Date >= "2012-4-10" & GP_Wolf$Date <= "2012-11-7",]
W13 <- GP_Wolf[GP_Wolf$Date >= "2013-4-10" & GP_Wolf$Date <= "2013-11-7",]
W14 <- GP_Wolf[GP_Wolf$Date >= "2014-4-10" & GP_Wolf$Date <= "2014-11-7",]
W15 <- GP_Wolf[GP_Wolf$Date >= "2015-4-10" & GP_Wolf$Date <= "2015-11-7",]
W16 <- GP_Wolf[GP_Wolf$Date >= "2016-4-10" & GP_Wolf$Date <= "2016-11-7",]
W17 <- GP_Wolf[GP_Wolf$Date >= "2017-4-10" & GP_Wolf$Date <= "2017-11-7",]
W18 <- GP_Wolf[GP_Wolf$Date >= "2018-4-10" & GP_Wolf$Date <= "2018-11-7",]
W19 <- GP_Wolf[GP_Wolf$Date >= "2019-4-10" & GP_Wolf$Date <= "2019-11-7",]
W20 <- GP_Wolf[GP_Wolf$Date >= "2020-4-10" & GP_Wolf$Date <= "2020-11-7",]
W21 <- GP_Wolf[GP_Wolf$Date >= "2021-4-10" & GP_Wolf$Date <= "2021-11-7",]
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
GP_Wolf <- rbind(W12, W13, W14, W15, W16, W17, W18, W19, W20, W21)
rm(W12, W13, W14, W15, W16, W17, W18, W19, W20, W21)
table(GP_Wolf$StudyID) # shows number of locations per individual

# Wolf with <50 locations were removed
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2013-004_2013") #only 9 locations
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2013-007_2013") #only 32 locations
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2013-008_2013") #only 33 locations
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2015-005_2017") #only 33 locations
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2015-011_2016") #only 18 locations
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2016-004_2016") #only 22 locations
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2017-003_2017") #only 21 locations
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2018-022_2018") #only 7 locations
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2019-005_2021") #only 15 locations
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2020-003_2021") #only 4 locations


# Wolf with errors were removed
#no wolves caused errors

# Finalize datasets
length(unique(GP_Wolf$StudyID))
GP_Wolf_Summer <- GP_Wolf
rm(GP_Wolf, Coords)

## Prep GPIR Wolf Winter Collar Data
# Import Collar Data
GP_Wolf <- read.csv('GP_Wolf_Collars_edits.csv') #Grand Portage Wolf collar locations
GP_Wolf <- st_as_sf(GP_Wolf, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=NAD83")
GP_Wolf <- st_transform(GP_Wolf, 26916)
Coords <- as.data.frame(st_coordinates(GP_Wolf$geometry))
GP_Wolf$geometry <- NULL
GP_Wolf$Longitude <- Coords$X
GP_Wolf$Latitude <- Coords$Y
GP_Wolf$StudyID <- as.factor(GP_Wolf$StudyID)
GP_Wolf$CollarID <- as.character(GP_Wolf$CollarID)
GP_Wolf <- arrange(GP_Wolf, StudyID, DateTime)

# Remove Wolf that dispersed outside the study area
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2014-001")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2014-003")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2014-008")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2015-008")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2017-004")
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2021-001")

# Clean Collar Data for just Winter
head(GP_Wolf)
GP_Wolf$Date <- as.Date(GP_Wolf$Date)
GP_Wolf$Year <- year(GP_Wolf$Date)
table(GP_Wolf$Year)
#W11 <- GP_Wolf[GP_Wolf$Date >= "2012-1-1" & GP_Wolf$Date <= "2012-4-9",] #results in no location data
W12 <- GP_Wolf[GP_Wolf$Date >= "2012-11-8" & GP_Wolf$Date <= "2013-4-9",]
W13 <- GP_Wolf[GP_Wolf$Date >= "2013-11-8" & GP_Wolf$Date <= "2014-4-9",]
W14 <- GP_Wolf[GP_Wolf$Date >= "2014-11-8" & GP_Wolf$Date <= "2015-4-9",]
W15 <- GP_Wolf[GP_Wolf$Date >= "2015-11-8" & GP_Wolf$Date <= "2016-4-9",]
W16 <- GP_Wolf[GP_Wolf$Date >= "2016-11-8" & GP_Wolf$Date <= "2017-4-9",]
W17 <- GP_Wolf[GP_Wolf$Date >= "2017-11-8" & GP_Wolf$Date <= "2018-4-9",]
W18 <- GP_Wolf[GP_Wolf$Date >= "2018-11-8" & GP_Wolf$Date <= "2019-4-9",]
W19 <- GP_Wolf[GP_Wolf$Date >= "2019-11-8" & GP_Wolf$Date <= "2020-4-9",]
W20 <- GP_Wolf[GP_Wolf$Date >= "2020-11-8" & GP_Wolf$Date <= "2021-4-9",]
W21 <- GP_Wolf[GP_Wolf$Date >= "2021-11-8" & GP_Wolf$Date <= "2021-12-31",]
#W11$Year <- 2011
W12$Year <- 2012
W13$Year <- 2013
W14$Year <- 2014
W15$Year <- 2015
W16$Year <- 2016
W17$Year <- 2017
W18$Year <- 2018
W19$Year <- 2019
W20$Year <- 2020
W21$Year <- 2021
#W11$StudyID <- paste(W11$StudyID, W11$Year, sep = '_')
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
GP_Wolf <- rbind(W12, W13, W14, W15, W16, W17, W18, W19, W20, W21) #W11 removed
rm(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21)
table(GP_Wolf$StudyID) # shows number of locations per individual

# Wolf with <50 locations were removed
GP_Wolf <- filter(GP_Wolf, GP_Wolf$StudyID != "GP_Wolf_2021-010_2021") #only 31 locations

# Wolf with errors were removed
#no wolves caused errors

# Finalize Datasets
length(unique(GP_Wolf$StudyID))
GP_Wolf_Winter <- GP_Wolf
rm(GP_Wolf, Coords)


### AKDE WRSFs
## Summer Home RSFs
Start <- Sys.time()
wRSF_List <- list()
GP_Wolf <- GP_Wolf_Summer

#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Wolf$StudyID))){
  print(Sys.time())
  print(i)
  Wolf <- GP_Wolf[GP_Wolf$StudyID == unique(GP_Wolf$StudyID)[i],]
  Wolf <- as.telemetry(Wolf, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Wolf)
  GUESS <- ctmm.guess(Wolf, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Wolf, GUESS)
  UD <- akde(Wolf, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Wolf, UD = UD, level.UD = 0.95, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Wolf_Summer_95_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Wolf_Summer_95_Model)
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Wolf_Summer_95.RData")


## Summer Core RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Wolf <- GP_Wolf_Summer

#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Wolf$StudyID))){
  print(Sys.time())
  print(i)
  Wolf <- GP_Wolf[GP_Wolf$StudyID == unique(GP_Wolf$StudyID)[i],]
  Wolf <- as.telemetry(Wolf, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Wolf)
  GUESS <- ctmm.guess(Wolf, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Wolf, GUESS)
  UD <- akde(Wolf, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Wolf, UD = UD, level.UD = 0.50, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Wolf_Summer_50_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Wolf_Summer_50_Model)
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Wolf_Summer_50.RData")


## Winter Home RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Wolf <- GP_Wolf_Winter

#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Wolf$StudyID))){
  print(Sys.time())
  print(i)
  Wolf <- GP_Wolf[GP_Wolf$StudyID == unique(GP_Wolf$StudyID)[i],]
  Wolf <- as.telemetry(Wolf, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Wolf)
  GUESS <- ctmm.guess(Wolf, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Wolf, GUESS)
  UD <- akde(Wolf, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Wolf, UD = UD, level.UD = 0.95, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
GP_Wolf_Winter_95_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Wolf_Winter_95_Model)
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Wolf_Winter_95.RData")


## Winter Core RSFs 
Start <- Sys.time()
wRSF_List <- list()
GP_Wolf <- GP_Wolf_Winter

#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(GP_Wolf$StudyID))){
  print(Sys.time())
  print(i)
  Wolf <- GP_Wolf[GP_Wolf$StudyID == unique(GP_Wolf$StudyID)[i],]
  Wolf <- as.telemetry(Wolf, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Wolf)
  GUESS <- ctmm.guess(Wolf, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Wolf, GUESS)
  UD <- akde(Wolf, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Wolf, UD = UD, level.UD = 0.50, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Roads=Dist2Roads, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
wRSF_List[[10]] <- NULL #causing unknown error in mean function
GP_Wolf_Winter_50_Model <- mean(wRSF_List, IC = "AICc")
summary(GP_Wolf_Winter_50_Model)
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Wolf_Winter_50.RData")
