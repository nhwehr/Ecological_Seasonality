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
#Not needed for island analyses
#Dist2Roads <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Roads_UTM')))
#crs(Dist2Roads) <- CRS("+init=epsg:26916")

# Distance to ISRO trails
#Nearest trail was calculated from the NPS database and reporjected to UTMs
Dist2Trails <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Trails_UTM')))
crs(Dist2Trails) <- CRS("+init=epsg:26916")

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


### IRNP Wolf Data
## Prep IRNP Wolf Summer Collar Data
# Import Collar Data
IR_Wolf <- read.csv('IR_Wolf_Collars_edits.csv') #Isle Royale Wolf collar locations
IR_Wolf <- st_as_sf(IR_Wolf, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=NAD83")
IR_Wolf <- st_transform(IR_Wolf, 26916)
Coords <- as.data.frame(st_coordinates(IR_Wolf$geometry))
IR_Wolf$geometry <- NULL
IR_Wolf$Longitude <- Coords$X
IR_Wolf$Latitude <- Coords$Y
IR_Wolf$StudyID <- as.factor(IR_Wolf$StudyID)
IR_Wolf$CollarID <- as.character(IR_Wolf$CollarID)
IR_Wolf$DateTime <- mdy_hm(IR_Wolf$DateTime)
IR_Wolf <- arrange(IR_Wolf, StudyID, DateTime)

# Remove Wolf that dispersed outside the study area
#The one wolf that left the island was removed from this dataset prior to analysis (Orning et al. 2020)

# Switch data to an annual cycle
IR_Wolf$Date <- mdy(IR_Wolf$Date)
IR_Wolf$Year <- year(IR_Wolf$Date)
table(IR_Wolf$Year)
W18 <- IR_Wolf[IR_Wolf$Date >= "2018-1-1" & IR_Wolf$Date <= "2018-12-31",]
W19 <- IR_Wolf[IR_Wolf$Date >= "2019-1-1" & IR_Wolf$Date <= "2019-12-31",]
W20 <- IR_Wolf[IR_Wolf$Date >= "2020-1-1" & IR_Wolf$Date <= "2020-12-31",]
W21 <- IR_Wolf[IR_Wolf$Date >= "2021-1-1" & IR_Wolf$Date <= "2021-12-31",]
W18$StudyID <- paste(W18$StudyID, W18$Year, sep = '_')
W19$StudyID <- paste(W19$StudyID, W19$Year, sep = '_')
W20$StudyID <- paste(W20$StudyID, W20$Year, sep = '_')
W21$StudyID <- paste(W21$StudyID, W21$Year, sep = '_')
IR_Wolf <- rbind(W18, W19, W20, W21)
rm(W18, W19, W20, W21)
table(IR_Wolf$StudyID) # shows number of locations per individual

# Wolf with <50 locations were removed
IR_Wolf <- filter(IR_Wolf, IR_Wolf$StudyID != "IR_Wolf_006_2019") #only 29 locations
IR_Wolf <- filter(IR_Wolf, IR_Wolf$StudyID != "IR_Wolf_010_2020") #only 10 locations

# Wolf with errors were removed
#IR_Wolf <- filter(IR_Wolf, IR_Wolf$StudyID != "IR_Wolf_015_2020") #invalid times error

# Finalize dataset
length(unique(IR_Wolf$StudyID))
IR_Wolf_YearRound <- IR_Wolf
rm(IR_Wolf, Coords)

### AKDE WRSFs
## YearRound HomeRange RSFs
Start <- Sys.time()
wRSF_List <- list()
IR_Wolf <- IR_Wolf_YearRound

#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(IR_Wolf$StudyID))){
  print(Sys.time())
  print(i)
  Wolf <- IR_Wolf[IR_Wolf$StudyID == unique(IR_Wolf$StudyID)[i],]
  Wolf <- as.telemetry(Wolf, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Wolf)
  GUESS <- ctmm.guess(Wolf, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Wolf, GUESS)
  UD <- akde(Wolf, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Wolf, UD = UD, level.UD = 0.95, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Trails=Dist2Trails, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
wRSF_List[[24]] <- NULL #invalid times error
wRSF_List[[28]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[23]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[12]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[10]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[6]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[2]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
IR_Wolf_YearRound_95_Model <- mean(wRSF_List, IC = "AICc")
summary(IR_Wolf_YearRound_95_Model)
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Wolf_YearRound_95.RData")


## YearRound CoreRange RSFs
Start <- Sys.time()
wRSF_List <- list()
IR_Wolf <- IR_Wolf_YearRound

#this loop iteratively conducts a WRSF for each individual using an AKDE thereby accounting for pseudoreplication
for(i in 1:length(unique(IR_Wolf$StudyID))){
  print(Sys.time())
  print(i)
  Wolf <- IR_Wolf[IR_Wolf$StudyID == unique(IR_Wolf$StudyID)[i],]
  Wolf <- as.telemetry(Wolf, datum = "+init=epsg:26916", projection = "+init=epsg:26916")
  SVF <- variogram(Wolf)
  GUESS <- ctmm.guess(Wolf, variogram=SVF, interactive = FALSE)
  FIT <- ctmm.select(Wolf, GUESS)
  UD <- akde(Wolf, FIT, weights = TRUE)
  plot(UD)
  wRSF <- rsf.fit(Wolf, UD = UD, level.UD = 0.50, trace = FALSE, reference = 0,
                  R = list(Aspect=Aspect, Slope=Slope, Superior=Dist2Superior, Trails=Dist2Trails, 
                           Coniferous=Coniferous, Deciduous=Deciduous, Mixed=Mixed, Shrubland=Shrubland, Wetland=Wetland, Other=Other))
  wRSF_List[[i]] <- wRSF
}

# Population-level Model
wRSF_List[[24]] <- NULL #invalid times error
wRSF_List[[29]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[23]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[13]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[10]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[6]] <- NULL #Model mu = infinity (ie it didn't actually fit a model)
wRSF_List[[2]] <- NULL #this model's mu wasn't infinity, but its inclusion in the model dramatically inflated the area values; it was also removed in the home range model, so I decided to exclude it here
IR_Wolf_YearRound_50_Model <- mean(wRSF_List, IC = "AICc")
summary(IR_Wolf_YearRound_50_Model)
save.image("C:/Users/natew/Documents/All Files/R Working Directory/Ecological Seasonality/Wolf_YearRound_50.RData")
