#Unfotunately... the "fviz_pca_ind()" function I used to make the graphs is very dependent on the last set of code run through
#so, you have to run everything to get it to work right.
#Libraries
library(lubridate)
library(dplyr)
library(amt)
library(sf)
library(raster)
library(arcgisbinding)
arc.check_product()
library(clusterSim)
library(factoextra)
library(purrr)
library(fpc)
library(ggplot2)
library(ggpubr)
setwd('C:\\Users\\natew\\Documents\\All Files\\R Working Directory\\Ecological Seasonality')

### Moose
### Step 1
#the first step is to extract step lengths and turning angles

## Data Setup
GPIR <- read.csv('GP_Moose_Collars_Edits.csv')
GPIR <- na.omit(GPIR)
head(GPIR)

# Put moose' locations in order by time 
Wolves <- filter(GPIR, GPIR$StudyID == "GP_Moos_001") #this is the first study ID in the file
Wolves <- Wolves[order(Wolves$DateTime),]
#this loop pulls out a single moose, orders its locations by timestamp and then puts it back in
for (i in 2:length(unique(GPIR$StudyID))) {
  W <- GPIR[GPIR$StudyID == unique(GPIR$StudyID)[i],]
  W <- W[order(W$DateTime),]
  Wolves <- rbind(Wolves, W)
}
GPIR <- Wolves
table(GPIR$StudyID)
#these 4 moose had less than 500 locations each causing the SSF to crash
GPIR <- filter(GPIR, GPIR$StudyID != "GP_Moos_171")
GPIR <- filter(GPIR, GPIR$StudyID != "GP_Moos_160")
GPIR <- filter(GPIR, GPIR$StudyID != "GP_Moos_162")
GPIR <- filter(GPIR, GPIR$StudyID != "GP_Moos_186")

## Calculate Step Lengths and Turning Angles for all moose
# Convert to "track" format used by animal movement tools (amt) package
GPIR$TimeStamp <- as.POSIXct(ymd_hms(GPIR$DateTime))
DATA <- make_track(GPIR, Longitude, Latitude, TimeStamp, crs = "+proj=longlat +datum=NAD83")
DATA$Name <- GPIR$StudyID #converting to track format gets rid of all animal descriptors, so they must be added back in if you want them

# Isolate an individual and order by location time
#this step and the next are important for initiating the loop that follows
head(DATA)
table(DATA$Name)
data <- DATA %>% 
  filter(DATA$Name == "GP_Moos_001") %>% 
  arrange(t_)
summarize_sampling_rate(data)

# Format and generate random steps
ssf_data <- data %>% 
  track_resample(rate = hours(12), tolerance = minutes(120)) %>% 
  steps_by_burst() %>% 
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_))%>%
  filter(!is.na(ta_))
ssf_data$Name <- data$Name[1] #add the moose's model ID to the extractions
# check that it worked
print(ssf_data, n = 25, width = Inf)

## Reiterate for remaining population
Moves <- ssf_data # set first moose's output as baseline
DATA$Name <- as.factor(DATA$Name) #factorize name for use in for loop
#note the for loop starts at i = 2 to skip the single moose done above
for(i in 2:length(unique(DATA$Name))){
  data <- DATA %>% 
    filter(DATA$Name == unique(DATA$Name)[i]) %>% 
    arrange(t_)
  ssf_data <- data %>% 
    track_resample(rate = hours(12), tolerance = minutes(120)) %>% 
    steps_by_burst() %>%
    mutate(cos_ta_ = cos(ta_), 
           log_sl_ = log(sl_))%>%
    filter(!is.na(ta_))
  ssf_data$Name <- data$Name[1]
  Moves <- rbind(Moves, ssf_data)
}
table(Moves$Name)
length(unique(Moves$Name))
head(Moves)


### Step 2
#the second step is to extract resource selection
WolfRSF <- Moves
# Convert collar location coordinates to spatial feature
WolfRSF <- st_as_sf(Moves, coords = c("x2_", "y2_"), crs = "+proj=longlat +datum=NAD83") #combines lat/long coordinates into a single column
head(WolfRSF)

#the following steps import a raster generated in ArcGIS, set its CRS, convert locations to the appropriate CRS, extract the raster data from that location, and then remove the raster layer to free up memory
# Aspect Extraction
#Aspect was calculated from the USGS 1 Arc-second Digital Elevation Models (2017)
Aspect <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\USGS_Aspect')))
crs(Aspect) <- CRS("+init=epsg:4269")
WolfRSF <- st_transform(WolfRSF, crs(Aspect))
WolfRSF$Aspect <- extract(Aspect, WolfRSF)
rm(Aspect)

# Slope Extraction
#Slope was calculated from the USGS 1 Arc-second Digital Elevation Models (2017)
Slope <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\USGS_Slope')))
crs(Slope) <- CRS("+init=epsg:4269")
WolfRSF <- st_transform(WolfRSF, crs(Slope))
WolfRSF$Slope <- extract(Slope, WolfRSF)
rm(Slope)

# Distance to Lake Superior Extractions
#Lake Superior's outline was obtained from the NOAA Great Lakes medium resolution digital shoreline, 2006
Dist2Superior <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Dist_Superior')))
crs(Dist2Superior) <- CRS("+init=epsg:26915")
WolfRSF <- st_transform(WolfRSF, crs(Dist2Superior))
WolfRSF$Dist2Superior <- extract(Dist2Superior, WolfRSF)
rm(Dist2Superior)

# Landcover Extractions
#Landcover data was obtained from the Commission for Environmental Cooperation Land Cover 30m, 2015 Map
Landcover <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Landcover')))
crs(Landcover) <- CRS('+proj=laea +lat_0=45 +lon_0=-100')
WolfRSF <- st_transform(WolfRSF, crs(Landcover))
WolfRSF$Landcover <- extract(Landcover, WolfRSF)
rm(Landcover)

# Distance to Road Extractions
#Road layers were obtained using the USCB Tiger roadmaps for Cook, Lake, and St. Louis counties MN (2015) and the Ontario Road Network (2022)
Dist2Roads <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Dist_Roads')))
crs(Dist2Roads) <- CRS("+init=epsg:26915")
WolfRSF <- st_transform(WolfRSF, crs(Dist2Roads))
WolfRSF$Dist2Roads <- extract(Dist2Roads, WolfRSF)
rm(Dist2Roads)

# Distance to ISRO trail extractions
#the ISRO trail map was obtained from NPS
Dist2Trails <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Dist_ISRO_Trails')))
crs(Dist2Trails) <- CRS("+init=epsg:6345")
WolfRSF <- st_transform(WolfRSF, crs(Dist2Trails))
WolfRSF$Dist2Trails <- extract(Dist2Trails, WolfRSF)
rm(Dist2Trails)


### Step 3
#this step corrects the resource variables to make them interpretable
Locs <- WolfRSF

# Aspect
#typeof(Locs$Aspect)
#summary(Locs$Aspect)
#plot(Locs$Aspect)
Locs <- subset(Locs, Locs$Aspect > 0)
Locs$Aspect_cos <- cos(pi * Locs$Aspect/180) #where -1 is a south facing slope and 1 is a north facing slope
#plot(Locs$Aspect_cos)

# Slope
#typeof(Locs$Slope)
#summary(Locs$Slope)
#plot(Locs$Slope)
#ggqqplot(Locs$Slope)
Locs$Slope_sqrt <- sqrt(Locs$Slope)
#plot(Locs$Slope_sqrt)
#ggqqplot(Locs$Slope_sqrt)

# Distance to Lake Superior
#typeof(Locs$Dist2Superior)
#summary(Locs$Dist2Superior)
#plot(Locs$Dist2Superior)
#ggqqplot(Locs$Dist2Superior)
Locs$Lake_scale <- scale(Locs$Dist2Superior)
#plot(Locs$Lake_scale)
#ggqqplot(Locs$Lake_scale)

# Distance to Roads
#typeof(Locs$Dist2Roads)
#summary(Locs$Dist2Roads)
#plot(Locs$Dist2Roads)
#ggqqplot(Locs$Dist2Roads)
Locs$Roads_sqrt <- sqrt(Locs$Dist2Roads)
#plot(Locs$Roads_sqrt)
#ggqqplot(Locs$Roads_sqrt)

# Distance to Trails
#typeof(Locs$Dist2Trails)
#summary(Locs$Dist2Trails)
#plot(Locs$Dist2Trails)
#ggqqplot(Locs$Dist2Trails)
Locs$Trails_sqrt <- sqrt(Locs$Dist2Trails)
#plot(Locs$Trails_sqrt)
#ggqqplot(Locs$Trails_sqrt)

# Landcover
#typeof(Locs$Landcover)
Locs$Landcover <- as.factor(Locs$Landcover)
Locs <- Locs[!is.na(Locs$Landcover),]
#table(Locs$Landcover)
Locs$CoverType <- NA
#Note: Cover types based off 2015 CEC metadata, 
for(i in 1:nrow(Locs)){
  if(Locs$Landcover[i] == '1'){Locs$CoverType[i] <- "Coniferous"}
  if(Locs$Landcover[i] == '5'){Locs$CoverType[i] <- "Deciduous"}
  if(Locs$Landcover[i] == '6'){Locs$CoverType[i] <- "Mixed"}
  if(Locs$Landcover[i] == '8'){Locs$CoverType[i] <- "Shrubland"}
  if(Locs$Landcover[i] == '10'){Locs$CoverType[i] <- "Shrubland"}
  if(Locs$Landcover[i] == '14'){Locs$CoverType[i] <- "Wetland"}
  if(Locs$Landcover[i] == '15'){Locs$CoverType[i] <- "Remove"}
  if(Locs$Landcover[i] == '16'){Locs$CoverType[i] <- "Remove"}
  if(Locs$Landcover[i] == '17'){Locs$CoverType[i] <- "Remove"}
  if(Locs$Landcover[i] == '18'){Locs$CoverType[i] <- "Remove"}
}
Locs$CoverType <- as.factor(Locs$CoverType)
table(Locs$CoverType)


### Step 4
#this step combines all moose into a single value that is the average for each day of the year
Locs$Day <- as.factor(yday(Locs$t2_))
table(Locs$Day)
#because captures generally occurred in September, the greatest number of data points occur on those days with progressively less coverage until captures restart the following summer
# Calculate daily averages
print(Locs, n = 3, width = Inf)
Averages <- data.frame(Day = 1:366)
Averages$Day <- as.factor(Averages$Day)
Averages$Steps <- NA
Averages$Turns <- NA
Averages$Aspect <- NA
Averages$Slope <- NA
Averages$Superior <- NA
Averages$Roads <- NA
Averages$Trails <- NA
for (i in 1:366){
  temp <- filter(Locs, Locs$Day == i)
  Averages$Steps[i] <- mean(temp$sl_)
  Averages$Turns[i] <- mean(temp$cos_ta_)
  Averages$Aspect[i] <- mean(temp$Aspect_cos)
  Averages$Slope[i] <- mean(temp$Slope_sqrt)
  Averages$Superior[i] <- mean(temp$Lake_scale)
  Averages$Roads[i] <- mean(temp$Roads_sqrt)
  Averages$Trails[i] <- mean(temp$Trails_sqrt)
}
#calculates proportion of wolf locations in each of the 5 primary habitat types 
Averages$Coniferous <- NA
Averages$Deciduous <- NA
Averages$Mixed <- NA
Averages$Shrubland <- NA
Averages$Wetland <- NA
for(i in 1:366){
  temp <- filter(Locs, Locs$Day == i)
  Con <- filter(temp, temp$CoverType == "Coniferous")
  Dec <- filter(temp, temp$CoverType == "Deciduous")
  Mix <- filter(temp, temp$CoverType == "Mixed")
  Shr <- filter(temp, temp$CoverType == "Shrubland")
  Wet <- filter(temp, temp$CoverType == "Wetland")
  Averages$Coniferous[i] <- nrow(Con) / nrow(temp)
  Averages$Deciduous[i] <- nrow(Dec) / nrow(temp)
  Averages$Mixed[i] <- nrow(Mix) / nrow(temp)
  Averages$Shrubland[i] <- nrow(Shr) / nrow(temp)
  Averages$Wetland[i] <- nrow(Wet) / nrow(temp)
}


### Step 5
#the cluster analysis (FINALLY)
#this cluster analysis should bin the 366 days of the year into categories which can then be manually combined into a logical set
rm(Con, data, DATA, Dec, GPIR, Locs, Mix, Moves, Shr, ssf_data, temp, Wet, WolfRSF, i, W, Wolves) #to simplify the environment
Cluster <- Averages

# Identify useful variables
#The Heuristic Identification of Noisy Variables (HINoV) calculates "topri", which is a relative score of each variable's usefulness. Low scoring variables can then be removed (Carmone Jr et al 1999)
Cluster <- Cluster[,!(names(Cluster) == "Trails")] #No trail data for the mainland
Cluster <- Cluster[,!(names(Cluster) == "Day")] #Day isn't actually a predictor so much as a descriptor (though it was very useful in terms of clustering when included in the model)
Cluster <- na.omit(Cluster)

# Normalize Variables
Normal <- data.Normalization(Cluster, type="n1", normalization = 'column') #the "n1" procedure standardizes all the variables

# Select Distance Measure
dist.GDM(Normal, method="GDM1", weightsType = "equal") #GDM1 is the only appropriate choice for this entirely continuous dataset

# Select Clustering Method
#kmeans is ultimately the most common choice, what Basille et al. 2013 used, and one of only two choices useful for this dataset

# Determine Number of Clusters (elbow method)
fviz_nbclust(Normal, kmeans, method = "wss")

# Determine Number of Clusters (silhouette method)
#build function to calculate sum of squares
avg_sil <- function(k) {
  km.res <- kmeans(Normal, centers = k, nstart = 1)
  ss <- silhouette(km.res$cluster, dist(Normal))
  mean(ss[, 3])
}
k.values <- 2:10 #range of possible number of clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

# Principle Components Analysis
Normal$Day <- c(1:366)
Normal$Cluster <- NA
for(i in 1:366){
  if(Normal$Day[i] %in% c(1:70)){Normal$Cluster[i] <- "Winter"}
  if(Normal$Day[i] %in% c(71:104)){Normal$Cluster[i] <- "Spring"}
  if(Normal$Day[i] %in% c(105:127)){Normal$Cluster[i] <- "Pre-parturition"}
  if(Normal$Day[i] %in% c(128:292)){Normal$Cluster[i] <- "Summer"}
  if(Normal$Day[i] %in% c(293:366)){Normal$Cluster[i] <- "Winter"}
}
Model_GPIR_Moose <- princomp(Normal[c(1:11)]) #1:11 for the 11 variables
Moose <- Model_GPIR_Moose
MM <- fviz_pca_ind(Moose, geom.ind = "point", pointshape = 21, pointsize = 2, 
                   fill.ind = Normal$Cluster, col.ind = "black", palette = c('grey', 'orange', 'darkgreen', 'blue'), 
                   addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, 
                   legend.title = "Season", title='Moose') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Component 2 (14.9%)", x = "Component 1 (21.5%)")


#### Deer
### Step 1
#the first step is to extract step lengths and turning angles

## Data Setup
GPIR <- read.csv('GP_Deer_Collars_Edits.csv')
GPIR <- na.omit(GPIR)
head(GPIR)

# Put deer' locations in order by time 
Wolves <- filter(GPIR, GPIR$StudyID == "GP_Deer_001") #this is the first study ID in the file
Wolves <- Wolves[order(Wolves$DateTime),]
#this loop pulls out a single deer, orders its locations by timestamp and then puts it back in
for (i in 2:length(unique(GPIR$StudyID))) {
  W <- GPIR[GPIR$StudyID == unique(GPIR$StudyID)[i],]
  W <- W[order(W$DateTime),]
  Wolves <- rbind(Wolves, W)
}
GPIR <- Wolves
table(GPIR$StudyID)
#these deer were causing the SSF to crash because they had too few locations for useful steps (< 10 locations each)
GPIR <- filter(GPIR, GPIR$StudyID != "GP_Deer_087")
GPIR <- filter(GPIR, GPIR$StudyID != "GP_Deer_043")

## Calculate Step Lengths and Turning Angles for all deer
# Convert to "track" format used by animal movement tools (amt) package
GPIR$TimeStamp <- as.POSIXct(ymd_hms(GPIR$DateTime))
DATA <- make_track(GPIR, Longitude, Latitude, TimeStamp, crs = "+proj=longlat +datum=NAD83")
DATA$Name <- GPIR$StudyID #converting to track format gets rid of all animal descriptors, so they must be added back in if you want them

# Isolate an individual and order by location time
#this step and the next are important for initiating the loop that follows
head(DATA)
table(DATA$Name)
data <- DATA %>% 
  filter(DATA$Name == "GP_Deer_001") %>% 
  arrange(t_)
summarize_sampling_rate(data)

# Format and generate random steps
ssf_data <- data %>% 
  track_resample(rate = hours(12), tolerance = minutes(120)) %>% 
  steps_by_burst() %>% 
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_))%>%
  filter(!is.na(ta_))
ssf_data$Name <- data$Name[1] #add the deer's model ID to the extractions
# check that it worked
print(ssf_data, n = 25, width = Inf)

## Reiterate for remaining population
Moves <- ssf_data # set first deer's output as baseline
DATA$Name <- as.factor(DATA$Name) #factorize name for use in for loop
#note the for loop starts at i = 2 to skip the single deer done above
for(i in 2:length(unique(DATA$Name))){
  data <- DATA %>% 
    filter(DATA$Name == unique(DATA$Name)[i]) %>% 
    arrange(t_)
  ssf_data <- data %>% 
    track_resample(rate = hours(12), tolerance = minutes(120)) %>% 
    steps_by_burst() %>%
    mutate(cos_ta_ = cos(ta_), 
           log_sl_ = log(sl_))%>%
    filter(!is.na(ta_))
  ssf_data$Name <- data$Name[1]
  Moves <- rbind(Moves, ssf_data)
}
head(Moves)
table(Moves$Name)
length(unique(Moves$Name))


### Step 2
#the second step is to extract resource selection
WolfRSF <- Moves
# Convert collar location coordinates to spatial feature
WolfRSF <- st_as_sf(Moves, coords = c("x2_", "y2_"), crs = "+proj=longlat +datum=NAD83") #combines lat/long coordinates into a single column
head(WolfRSF)

#the following steps import a raster generated in ArcGIS, set its CRS, convert locations to the appropriate CRS, extract the raster data from that location, and then remove the raster layer to free up memory
# Aspect Extraction
#Aspect was calculated from the USGS 1 Arc-second Digital Elevation Models (2017)
Aspect <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\USGS_Aspect')))
crs(Aspect) <- CRS("+init=epsg:4269")
WolfRSF <- st_transform(WolfRSF, crs(Aspect))
WolfRSF$Aspect <- extract(Aspect, WolfRSF)
rm(Aspect)

# Slope Extraction
#Slope was calculated from the USGS 1 Arc-second Digital Elevation Models (2017)
Slope <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\USGS_Slope')))
crs(Slope) <- CRS("+init=epsg:4269")
WolfRSF <- st_transform(WolfRSF, crs(Slope))
WolfRSF$Slope <- extract(Slope, WolfRSF)
rm(Slope)

# Distance to Lake Superior Extractions
#Lake Superior's outline was obtained from the NOAA Great Lakes medium resolution digital shoreline, 2006
Dist2Superior <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Dist_Superior')))
crs(Dist2Superior) <- CRS("+init=epsg:26915")
WolfRSF <- st_transform(WolfRSF, crs(Dist2Superior))
WolfRSF$Dist2Superior <- extract(Dist2Superior, WolfRSF)
rm(Dist2Superior)

# Landcover Extractions
#Landcover data was obtained from the Commission for Environmental Cooperation Land Cover 30m, 2015 Map
Landcover <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Landcover')))
crs(Landcover) <- CRS('+proj=laea +lat_0=45 +lon_0=-100')
WolfRSF <- st_transform(WolfRSF, crs(Landcover))
WolfRSF$Landcover <- extract(Landcover, WolfRSF)
rm(Landcover)

# Distance to Road Extractions
#Road layers were obtained using the USCB Tiger roadmaps for Cook, Lake, and St. Louis counties MN (2015) and the Ontario Road Network (2022)
Dist2Roads <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Dist_Roads')))
crs(Dist2Roads) <- CRS("+init=epsg:26915")
WolfRSF <- st_transform(WolfRSF, crs(Dist2Roads))
WolfRSF$Dist2Roads <- extract(Dist2Roads, WolfRSF)
rm(Dist2Roads)

# Distance to ISRO trail extractions
#the ISRO trail map was obtained from NPS
Dist2Trails <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Dist_ISRO_Trails')))
crs(Dist2Trails) <- CRS("+init=epsg:6345")
WolfRSF <- st_transform(WolfRSF, crs(Dist2Trails))
WolfRSF$Dist2Trails <- extract(Dist2Trails, WolfRSF)
rm(Dist2Trails)


### Step 3
#this step corrects the resource variables to make them interpretable
Locs <- WolfRSF

# Aspect
#typeof(Locs$Aspect)
#summary(Locs$Aspect)
#plot(Locs$Aspect)
Locs <- subset(Locs, Locs$Aspect > 0)
Locs$Aspect_cos <- cos(pi * Locs$Aspect/180) #where -1 is a south facing slope and 1 is a north facing slope
#plot(Locs$Aspect_cos)

# Slope
#typeof(Locs$Slope)
#summary(Locs$Slope)
#plot(Locs$Slope)
#ggqqplot(Locs$Slope)
Locs$Slope_sqrt <- sqrt(Locs$Slope)
#plot(Locs$Slope_sqrt)
#ggqqplot(Locs$Slope_sqrt)

# Distance to Lake Superior
#typeof(Locs$Dist2Superior)
#summary(Locs$Dist2Superior)
#plot(Locs$Dist2Superior)
#ggqqplot(Locs$Dist2Superior)
Locs$Lake_scale <- scale(Locs$Dist2Superior)
#plot(Locs$Lake_scale)
#ggqqplot(Locs$Lake_scale)

# Distance to Roads
#typeof(Locs$Dist2Roads)
#summary(Locs$Dist2Roads)
#plot(Locs$Dist2Roads)
#ggqqplot(Locs$Dist2Roads)
Locs$Roads_sqrt <- sqrt(Locs$Dist2Roads)
#plot(Locs$Roads_sqrt)
#ggqqplot(Locs$Roads_sqrt)

# Distance to Trails
#typeof(Locs$Dist2Trails)
#summary(Locs$Dist2Trails)
#plot(Locs$Dist2Trails)
#ggqqplot(Locs$Dist2Trails)
Locs$Trails_sqrt <- sqrt(Locs$Dist2Trails)
#plot(Locs$Trails_sqrt)
#ggqqplot(Locs$Trails_sqrt)

# Landcover
#typeof(Locs$Landcover)
Locs$Landcover <- as.factor(Locs$Landcover)
Locs <- Locs[!is.na(Locs$Landcover),]
#table(Locs$Landcover)
Locs$CoverType <- NA
#Note: Cover types based off 2015 CEC metadata, 
for(i in 1:nrow(Locs)){
  if(Locs$Landcover[i] == '1'){Locs$CoverType[i] <- "Coniferous"}
  if(Locs$Landcover[i] == '5'){Locs$CoverType[i] <- "Deciduous"}
  if(Locs$Landcover[i] == '6'){Locs$CoverType[i] <- "Mixed"}
  if(Locs$Landcover[i] == '8'){Locs$CoverType[i] <- "Shrubland"}
  if(Locs$Landcover[i] == '10'){Locs$CoverType[i] <- "Shrubland"}
  if(Locs$Landcover[i] == '14'){Locs$CoverType[i] <- "Wetland"}
  if(Locs$Landcover[i] == '15'){Locs$CoverType[i] <- "Remove"}
  if(Locs$Landcover[i] == '16'){Locs$CoverType[i] <- "Remove"}
  if(Locs$Landcover[i] == '17'){Locs$CoverType[i] <- "Remove"}
  if(Locs$Landcover[i] == '18'){Locs$CoverType[i] <- "Remove"}
}
Locs$CoverType <- as.factor(Locs$CoverType)
table(Locs$CoverType)


### Step 4
#this step combines all wolves into a single value that is the average for each day of the year
Locs$Day <- as.factor(yday(Locs$t2_))
table(Locs$Day)
#because captures generally occurred in September, the greatest number of data points occur on those days with progressively less coverage until captures restart the following summer
# Calculate daily averages
print(Locs, n = 3, width = Inf)
Averages <- data.frame(Day = 1:366)
Averages$Day <- as.factor(Averages$Day)
Averages$Steps <- NA
Averages$Turns <- NA
Averages$Aspect <- NA
Averages$Slope <- NA
Averages$Superior <- NA
Averages$Roads <- NA
Averages$Trails <- NA
for (i in 1:366){
  temp <- filter(Locs, Locs$Day == i)
  Averages$Steps[i] <- mean(temp$sl_)
  Averages$Turns[i] <- mean(temp$cos_ta_)
  Averages$Aspect[i] <- mean(temp$Aspect_cos)
  Averages$Slope[i] <- mean(temp$Slope_sqrt)
  Averages$Superior[i] <- mean(temp$Lake_scale)
  Averages$Roads[i] <- mean(temp$Roads_sqrt)
  Averages$Trails[i] <- mean(temp$Trails_sqrt)
}
#calculates proportion of deer locations in each of the 5 primary habitat types 
Averages$Coniferous <- NA
Averages$Deciduous <- NA
Averages$Mixed <- NA
Averages$Shrubland <- NA
Averages$Wetland <- NA
for(i in 1:366){
  temp <- filter(Locs, Locs$Day == i)
  Con <- filter(temp, temp$CoverType == "Coniferous")
  Dec <- filter(temp, temp$CoverType == "Deciduous")
  Mix <- filter(temp, temp$CoverType == "Mixed")
  Shr <- filter(temp, temp$CoverType == "Shrubland")
  Wet <- filter(temp, temp$CoverType == "Wetland")
  Averages$Coniferous[i] <- nrow(Con) / nrow(temp)
  Averages$Deciduous[i] <- nrow(Dec) / nrow(temp)
  Averages$Mixed[i] <- nrow(Mix) / nrow(temp)
  Averages$Shrubland[i] <- nrow(Shr) / nrow(temp)
  Averages$Wetland[i] <- nrow(Wet) / nrow(temp)
}


### Step 5
#the cluster analysis (FINALLY)
#this cluster analysis should bin the 366 days of the year into categories which can then be manually combined into a logical set
rm(Con, data, DATA, Dec, GPIR, Locs, Mix, Moves, Shr, ssf_data, temp, Wet, WolfRSF, i, W, Wolves) #to simplify the environment
Cluster <- Averages


# Identify useful variables
#The Heuristic Identification of Noisy Variables (HINoV) calculates "topri", which is a relative score of each variable's usefulness. Low scoring variables can then be removed (Carmone Jr et al 1999)
Cluster <- Cluster[,!(names(Cluster) == "Trails")] #No trail data for the mainland
Cluster <- Cluster[,!(names(Cluster) == "Day")] #Day isn't actually a predictor so much as a descriptor (though it was very useful in terms of clustering when included in the model)
Cluster <- na.omit(Cluster)

# Normalize Variables
Normal <- data.Normalization(Cluster, type="n1", normalization = 'column') #the "n1" procedure standardizes all the variables

# Select Distance Measure
dist.GDM(Normal, method="GDM1", weightsType = "equal") #GDM1 is the only appropriate choice for this entirely continuous dataset

# Select Clustering Method
#kmeans is ultimately the most common choice, what Basille et al. 2013 used, and one of only two choices useful for this dataset

# Determine Number of Clusters (elbow method)
fviz_nbclust(Normal, kmeans, method = "wss")

# Determine Number of Clusters (silhouette method)
#build function to calculate sum of squares
avg_sil <- function(k) {
  km.res <- kmeans(Normal, centers = k, nstart = 1)
  ss <- silhouette(km.res$cluster, dist(Normal))
  mean(ss[, 3])
}
k.values <- 2:10 #range of possible number of clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

# Principle Components Analysis
Normal$Day <- c(1:366)
Normal$Cluster <- NA
for(i in 1:366){
  if(Normal$Day[i] %in% c(1:114)){Normal$Cluster[i] <- "Winter"}
  if(Normal$Day[i] %in% c(115:323)){Normal$Cluster[i] <- "Summer"}
  if(Normal$Day[i] %in% c(324:366)){Normal$Cluster[i] <- "Winter"}
}
Model_GPIR_Deer <- princomp(Normal[c(1:11)]) #1:11 for the 11 variables
Deer <- Model_GPIR_Deer
DM <- fviz_pca_ind(Deer, geom.ind = "point", pointshape = 21, pointsize = 2, 
             fill.ind = Normal$Cluster, col.ind = "black", palette = c('darkgreen', 'blue', 'red', 'red'), 
             addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Season", title="White-tailed deer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Component 2 (13.0%)", x = "Component 1 (28.2%)")



#### Wolves
### Step 1
#the first step is to extract step lengths and turning angles

## Data Setup
GPIR <- read.csv('GP_Wolf_Collars_Edits.csv')
GPIR <- na.omit(GPIR)
head(GPIR)

# Put wolves' locations in order by time 
Wolves <- filter(GPIR, GPIR$StudyID == "GP_Wolf_2013-004") #this is the first study ID in the file
Wolves <- Wolves[order(Wolves$DateTime),]
#this loop pulls out a single wolf, orders its locations by timestamp and then puts it back in
for (i in 2:length(unique(GPIR$StudyID))) {
  W <- GPIR[GPIR$StudyID == unique(GPIR$StudyID)[i],]
  W <- W[order(W$DateTime),]
  Wolves <- rbind(Wolves, W)
}
GPIR <- Wolves

## Calculate Step Lengths and Turning Angles for all wolves
# Convert to "track" format used by animal movement tools (amt) package
GPIR$TimeStamp <- as.POSIXct(GPIR$DateTime)
DATA <- make_track(GPIR, Longitude, Latitude, TimeStamp, crs = "+proj=longlat +datum=NAD83")
DATA$Name <- GPIR$StudyID #converting to track format gets rid of all animal descriptors, so they must be added back in if you want them
# Remove individuals w/ insufficient data
table(GPIR$StudyID)
DATA <- filter(DATA, DATA$Name != "GP_Wolf_2013-004")
DATA <- filter(DATA, DATA$Name != "GP_Wolf_2018-022")

# Isolate an individual and order by location time
#this step and the next are important for initiating the loop that follows
head(DATA)
table(DATA$Name)
data <- DATA %>% 
  filter(DATA$Name == "GP_Wolf_2013-007") %>% 
  arrange(t_)
summarize_sampling_rate(data)

# Format and generate random steps
ssf_data <- data %>% 
  track_resample(rate = hours(12), tolerance = minutes(120)) %>% 
  steps_by_burst() %>% 
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_))%>%
  filter(!is.na(ta_))
ssf_data$Name <- data$Name[1] #add the wolf's model ID to the extractions
# check that it worked
print(ssf_data, n = 25, width = Inf)

## Reiterate for remaining population
Moves <- ssf_data # set first wolf's output as baseline
DATA$Name <- as.factor(DATA$Name) #factorize name for use in for loop
head(DATA)
#note the for loop starts at i = 2 to skip the single wolf done above
for(i in 2:length(unique(DATA$Name))){
  data <- DATA %>% 
    filter(DATA$Name == unique(DATA$Name)[i]) %>% 
    arrange(t_)
  ssf_data <- data %>% 
    track_resample(rate = hours(12), tolerance = minutes(120)) %>% 
    steps_by_burst() %>%
    mutate(cos_ta_ = cos(ta_), 
           log_sl_ = log(sl_))%>%
    filter(!is.na(ta_))
  ssf_data$Name <- data$Name[1]
  Moves <- rbind(Moves, ssf_data)
}
head(Moves)
table(Moves$Name)
length(unique(Moves$Name))


### Step 2
#the second step is to extract resource selection
WolfRSF <- Moves
# Convert collar location coordinates to spatial feature
WolfRSF <- st_as_sf(Moves, coords = c("x2_", "y2_"), crs = "+proj=longlat +datum=NAD83") #combines lat/long coordinates into a single column
head(WolfRSF)

#the following steps import a raster generated in ArcGIS, set its CRS, convert locations to the appropriate CRS, extract the raster data from that location, and then remove the raster layer to free up memory
# Aspect Extraction
#Aspect was calculated from the USGS 1 Arc-second Digital Elevation Models (2017)
Aspect <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\USGS_Aspect')))
crs(Aspect) <- CRS("+init=epsg:4269")
WolfRSF <- st_transform(WolfRSF, crs(Aspect))
WolfRSF$Aspect <- extract(Aspect, WolfRSF)
rm(Aspect)

# Slope Extraction
#Slope was calculated from the USGS 1 Arc-second Digital Elevation Models (2017)
Slope <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\USGS_Slope')))
crs(Slope) <- CRS("+init=epsg:4269")
WolfRSF <- st_transform(WolfRSF, crs(Slope))
WolfRSF$Slope <- extract(Slope, WolfRSF)
rm(Slope)

# Distance to Lake Superior Extractions
#Lake Superior's outline was obtained from the NOAA Great Lakes medium resolution digital shoreline, 2006
Dist2Superior <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Dist_Superior')))
crs(Dist2Superior) <- CRS("+init=epsg:26915")
WolfRSF <- st_transform(WolfRSF, crs(Dist2Superior))
WolfRSF$Dist2Superior <- extract(Dist2Superior, WolfRSF)
rm(Dist2Superior)

# Landcover Extractions
#Landcover data was obtained from the Commission for Environmental Cooperation Land Cover 30m, 2015 Map
Landcover <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Landcover')))
crs(Landcover) <- CRS('+proj=laea +lat_0=45 +lon_0=-100')
WolfRSF <- st_transform(WolfRSF, crs(Landcover))
WolfRSF$Landcover <- extract(Landcover, WolfRSF)
rm(Landcover)

# Distance to Road Extractions
#Road layers were obtained using the USCB Tiger roadmaps for Cook, Lake, and St. Louis counties MN (2015) and the Ontario Road Network (2022)
Dist2Roads <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Dist_Roads')))
crs(Dist2Roads) <- CRS("+init=epsg:26915")
WolfRSF <- st_transform(WolfRSF, crs(Dist2Roads))
WolfRSF$Dist2Roads <- extract(Dist2Roads, WolfRSF)
rm(Dist2Roads)

# Distance to ISRO trail extractions
#the ISRO trail map was obtained from NPS
Dist2Trails <- as.raster(arc.raster(arc.open('C:\\Users\\natew\\Documents\\All Files\\ArcGIS Working Directory\\Dissertation.gdb\\Dist_ISRO_Trails')))
crs(Dist2Trails) <- CRS("+init=epsg:6345")
WolfRSF <- st_transform(WolfRSF, crs(Dist2Trails))
WolfRSF$Dist2Trails <- extract(Dist2Trails, WolfRSF)
rm(Dist2Trails)


### Step 3
#this step corrects the resource variables to make them interpretable
Locs <- WolfRSF

# Aspect
#typeof(Locs$Aspect)
#summary(Locs$Aspect)
#plot(Locs$Aspect)
Locs <- subset(Locs, Locs$Aspect > 0)
Locs$Aspect_cos <- cos(pi * Locs$Aspect/180) #where -1 is a south facing slope and 1 is a north facing slope
#plot(Locs$Aspect_cos)

# Slope
#typeof(Locs$Slope)
#summary(Locs$Slope)
#plot(Locs$Slope)
#ggqqplot(Locs$Slope)
Locs$Slope_sqrt <- sqrt(Locs$Slope)
#plot(Locs$Slope_sqrt)
#ggqqplot(Locs$Slope_sqrt)

# Distance to Lake Superior
#typeof(Locs$Dist2Superior)
#summary(Locs$Dist2Superior)
#plot(Locs$Dist2Superior)
#ggqqplot(Locs$Dist2Superior)
Locs$Lake_scale <- scale(Locs$Dist2Superior)
#plot(Locs$Lake_scale)
#ggqqplot(Locs$Lake_scale)

# Distance to Roads
#typeof(Locs$Dist2Roads)
#summary(Locs$Dist2Roads)
#plot(Locs$Dist2Roads)
#ggqqplot(Locs$Dist2Roads)
Locs$Roads_sqrt <- sqrt(Locs$Dist2Roads)
#plot(Locs$Roads_sqrt)
#ggqqplot(Locs$Roads_sqrt)

# Distance to Trails
#typeof(Locs$Dist2Trails)
#summary(Locs$Dist2Trails)
#plot(Locs$Dist2Trails)
#ggqqplot(Locs$Dist2Trails)
Locs$Trails_sqrt <- sqrt(Locs$Dist2Trails)
#plot(Locs$Trails_sqrt)
#ggqqplot(Locs$Trails_sqrt)

# Landcover
#typeof(Locs$Landcover)
Locs$Landcover <- as.factor(Locs$Landcover)
Locs <- Locs[!is.na(Locs$Landcover),]
#table(Locs$Landcover)
Locs$CoverType <- NA
#Note: Cover types based off 2015 CEC metadata, 
for(i in 1:nrow(Locs)){
  if(Locs$Landcover[i] == '1'){Locs$CoverType[i] <- "Coniferous"}
  if(Locs$Landcover[i] == '5'){Locs$CoverType[i] <- "Deciduous"}
  if(Locs$Landcover[i] == '6'){Locs$CoverType[i] <- "Mixed"}
  if(Locs$Landcover[i] == '8'){Locs$CoverType[i] <- "Shrubland"}
  if(Locs$Landcover[i] == '10'){Locs$CoverType[i] <- "Shrubland"}
  if(Locs$Landcover[i] == '14'){Locs$CoverType[i] <- "Wetland"}
  if(Locs$Landcover[i] == '15'){Locs$CoverType[i] <- "Remove"}
  if(Locs$Landcover[i] == '16'){Locs$CoverType[i] <- "Remove"}
  if(Locs$Landcover[i] == '17'){Locs$CoverType[i] <- "Remove"}
  if(Locs$Landcover[i] == '18'){Locs$CoverType[i] <- "Remove"}
}
Locs$CoverType <- as.factor(Locs$CoverType)
table(Locs$CoverType)


### Step 4
#this step combines all wolves into a single value that is the average for each day of the year
Locs$Day <- as.factor(yday(Locs$t2_))
table(Locs$Day)
#because captures generally occurred in September, the greatest number of data points occur on those days with progressively less coverage until captures restart the following summer
# Calculate daily averages
print(Locs, n = 3, width = Inf)
Averages <- data.frame(Day = 1:366)
Averages$Day <- as.factor(Averages$Day)
Averages$Steps <- NA
Averages$Turns <- NA
Averages$Aspect <- NA
Averages$Slope <- NA
Averages$Superior <- NA
Averages$Roads <- NA
Averages$Trails <- NA
for (i in 1:366){
  temp <- filter(Locs, Locs$Day == i)
  Averages$Steps[i] <- mean(temp$sl_)
  Averages$Turns[i] <- mean(temp$cos_ta_)
  Averages$Aspect[i] <- mean(temp$Aspect_cos)
  Averages$Slope[i] <- mean(temp$Slope_sqrt)
  Averages$Superior[i] <- mean(temp$Lake_scale)
  Averages$Roads[i] <- mean(temp$Roads_sqrt)
  Averages$Trails[i] <- mean(temp$Trails_sqrt)
}
#calculates proportion of wolf locations in each of the 5 primary habitat types 
Averages$Coniferous <- NA
Averages$Deciduous <- NA
Averages$Mixed <- NA
Averages$Shrubland <- NA
Averages$Wetland <- NA
for(i in 1:366){
  temp <- filter(Locs, Locs$Day == i)
  Con <- filter(temp, temp$CoverType == "Coniferous")
  Dec <- filter(temp, temp$CoverType == "Deciduous")
  Mix <- filter(temp, temp$CoverType == "Mixed")
  Shr <- filter(temp, temp$CoverType == "Shrubland")
  Wet <- filter(temp, temp$CoverType == "Wetland")
  Averages$Coniferous[i] <- nrow(Con) / nrow(temp)
  Averages$Deciduous[i] <- nrow(Dec) / nrow(temp)
  Averages$Mixed[i] <- nrow(Mix) / nrow(temp)
  Averages$Shrubland[i] <- nrow(Shr) / nrow(temp)
  Averages$Wetland[i] <- nrow(Wet) / nrow(temp)
}


### Step 5
#the cluster analysis (FINALLY)
#this cluster analysis should bin the 366 days of the year into categories which can then be manually combined into a logical set
rm(Con, data, DATA, Dec, GPIR, Locs, Mix, Moves, Shr, ssf_data, temp, Wet, WolfRSF, i, W, Wolves) #to simplify the environment
Cluster <- Averages

# Identify useful variables
#The Heuristic Identification of Noisy Variables (HINoV) calculates "topri", which is a relative score of each variable's usefulness. Low scoring variables can then be removed (Carmone Jr et al 1999)
Cluster <- Cluster[,!(names(Cluster) == "Trails")] #no mainland trail data
Cluster <- Cluster[,!(names(Cluster) == "Day")] #Day isn't actually a predictor so much as a descriptor (though it was very useful in terms of clustering when included in the model)

# Normalize Variables
Normal <- data.Normalization(Cluster, type="n1", normalization = 'column') #the "n1" procedure standardizes all the variables

# Select Distance Measure
dist.GDM(Normal, method="GDM1", weightsType = "equal") #GDM1 is the only appropriate choice for this entirely continuous dataset

# Select Clustering Method
#kmeans is ultimately the most common choice, what Basille et al. 2013 used, and one of only two choices useful for this dataset

# Determine Number of Clusters (elbow method)
fviz_nbclust(Normal, kmeans, method = "wss")

# Determine Number of Clusters (silhouette method) ; ()
#build function to calculate sum of squares
avg_sil <- function(k) {
  km.res <- kmeans(Normal, centers = k, nstart = 1)
  ss <- silhouette(km.res$cluster, dist(Normal))
  mean(ss[, 3])
}
k.values <- 2:10 #range of possible number of clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

# Principle Components Analysis
Normal$Day <- c(1:366)
Normal$Cluster <- NA
for(i in 1:366){
  if(Normal$Day[i] %in% c(1:100)){Normal$Cluster[i] <- "Winter"}
  if(Normal$Day[i] %in% c(101:312)){Normal$Cluster[i] <- "Summer"}
  if(Normal$Day[i] %in% c(313:366)){Normal$Cluster[i] <- "Winter"}
}
Model_GPIR_Wolves <- princomp(Normal[c(1:11)]) #1:11 for the 11 variables
Wolf <- Model_GPIR_Wolves
WM <- fviz_pca_ind(Wolf, geom.ind = "point", pointshape = 21, pointsize = 2, 
             fill.ind = Normal$Cluster, col.ind = "black", palette = c('darkgreen', 'blue', 'red', 'red'), 
             addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Season", title='Gray wolves') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Component 2 (13.8%)", x = "Component 1 (16.7%)")


#Merge and Save Plots  
Combo <- ggarrange(WM, MM, DM, ncol = 1, nrow = 3)
Combo
ggsave(plot = Combo, width = 6.5, height = 10, dpi = 600, filename = "Figure 2 - Ecological Seasonality - V5.jpg")
