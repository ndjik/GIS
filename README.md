# GIS
################################################################
################ READING IN WINTER DATA ########################
################################################################

library(tidyverse)
read_csv("airtube-data-BG-201812.csv.gz") -> dec
dec %>% write_csv("airtube-data-BG-201812.csv")

read_csv("airtube-data-BG-201901.csv.gz") -> jan
jan %>% write_csv("airtube-data-BG-201901.csv")

read_csv("airtube-data-BG-201902.csv.gz") -> feb
feb %>% write_csv("airtube-data-BG-201902.csv")

# Reading in Sofia shapefile as sf 
library(rgdal)
library(sf)

shapes <- st_read("Neighbourhoods/GR_UNITS_20190122.shp")

# Merging data into 'd'
d <- rbind(dec, jan)
d <- rbind(d, feb)

# Converting geohashed coordinated into longlat 
library(geohashTools)
coords <- gh_decode(d$geohash)
d$lat <- coords$latitude
d$long <- coords$longitude

# Focusing data around Sofia 
sofia <- d[ which(d$lat< 42.79
                  & d$lat > 42.61), ]
sofia <- sofia[ which(sofia$long< 23.46
                      & sofia$long > 23.20), ]

# Transforming crs of pollution data to that of Sofia shapefile. 
sofia <- st_as_sf(sofia, coords = c("long", "lat"), crs = 4326) # because not projected
sofia <-st_transform(sofia, crs = 32634)

# Cutting air pollution that is outside of polygon
sofia <-st_crop(sofia, shapes)

################################################################
################# CLEANING WINTER DATA #########################
################################################################

# Removing high and low values of PM2.5 and for observations when high humidity
sofia2 <- sofia[ which(sofia$P2<500 & sofia$P2 > 10 & sofia$humidity < 80), ] 

# Counting the frequency of each sensor (how many obs each sensor has). Data stored in a df called 'counting'
counting <- data.frame(table(sofia2$geohash))
# Creating threshold variable that will help us later keep sensors that have high temporal coverage (over 300 obs) 
counting$keep <- ifelse(counting$Freq > 300, 1, 0) 

names(counting)[names(counting) == "Var1"] <- "geohash" # renaming variable for purpose of merging 

merged1 <- merge(sofia2, counting, all=TRUE, sort=FALSE) # merging pollution and fq df, by the variable 'geohash' (both dataframes have the same variable name for their geohash coordinates as we renamed the geohash variable accordingly in the 'counting' df)

# removing sensors w less than 300 observations using the 'keep' variable
sofia3 <- merged1[ which(merged1$keep==1), ] 

# Grouping data by geohash for: 
# reliable sensors
sofia3 <- aggregate(sofia3, list(sofia3$geohash), mean) # reliable sensor count: 251. 
# all sensors
sofia_all <- aggregate(sofia2, list(sofia2$geohash), mean) # total sensor count: 336. 

# Mapping the sensors that will be used for analysis and interpolation 
plot(shapes_sp)
plot(sofia3, col="red", add=TRUE)

################################################################
############## INTERPOLATION WINTER DATA #######################
################################################################ 

# Converting pollution data to SP
sofia_sp <- as(sofia3, 'Spatial')

# Reading in Sofia shapefile as SP
shapes_sp <-readOGR("Neighbourhoods/GR_UNITS_20190122.shp", layer="GR_UNITS_20190122")

# Calculate range of shapefile
box <- bbox(shapes_sp)
x.range <- as.integer(box[1,])
y.range <- as.integer(box[2,])

# Create a grid from the values in points df. 
# Expand range to a grid with spacing that we would like to use in interpolation
# Here we will use 200m grid cells
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=200), y=seq(from=y.range[1], to=y.range[2], by=200))
# Convert grid to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE

# Assigning the projection of the pollution data to the grid. 
library(gstat)
proj4string(grd) <- proj4string(sofia_sp)
# Inverse-distance interpolation
idw<-idw(formula=P2 ~ 1, locations=sofia_sp, newdata=grd)

# Transforming the interpolated data to raster
library(raster)
ras <- raster(idw)
# Clip the raster to Sofia outline
rasmask <- mask(ras, shapes_sp)

# Calculating mean P2.5 pollution per neighbourhood
av2.5 <- raster::extract(rasmask, sofia_sp, fun=mean, na.rm=TRUE)

# Since this data is ordered with our polygons we can just assign it into a new variable 'av2.5".
# shapes_sp@data$av2.5 <- unlist(raster::extract(rasmask, shapes_sp, fun=mean, na.rm=TRUE))
shapes_sp@data$av2.5 <- as.vector(shapes_sp@data$av2.5)

# mapping interpolated PM2.5 concentrations per neighbourhood
library(tmap)
map_inter <- tm_shape(shapes_sp) + 
  tm_fill("av2.5", 
          style = "fixed", 
          breaks=c(20, 25, 30, 35, 40, 45, 50), 
          palette = c("#a6dba0", "#e7d4e8", "#c2a5cf", "#9970ab", 
                      "#762a83", "#40004b"),
          title = "Concentration (μg/m3)") +
  tm_borders("white", alpha=.5) + 
  tm_compass() 
map_inter

mean(shapes_sp@data$av2.5) # exploratory stat

################################################################
################ READING IN SUMMER DATA ########################
################################################################

read_csv("airtube-data-BG-201906.csv.gz") -> jun
jun %>% write_csv("airtube-data-BG-201904.csv") 

read_csv("airtube-data-BG-201907.csv.gz") -> jul
jul %>% write_csv("airtube-data-BG-201905.csv")

read_csv("airtube-data-BG-201908.csv.gz") -> aug
aug %>% write_csv("airtube-data-BG-201909.csv")

# Merging data into 'd3'
d3 <- rbind(jun, jul)
d3 <- rbind(d3, aug)

# Converting geohashed coordinated into longlat 
coords <- gh_decode(d3$geohash)
d3$lat <- coords$latitude
d3$long <- coords$longitude

# Focusing data around Sofia 
sofiaaa <- d3[ which(d3$lat< 42.79
                     & d3$lat > 42.61), ]
sofiaaa <- sofiaaa[ which(sofiaaa$long< 23.46
                          & sofiaaa$long > 23.20), ]

# Transforming crs of pollution data to crs of Sofia shapefile
sofiaaa <- st_as_sf(sofiaaa, coords = c("long", "lat"), crs = 4326)
sofiaaa <-st_transform(sofiaaa, crs = 32634)

# Cutting air pollution that is outside of polygon
sofiaaa <-st_crop(sofiaaa, shapes)

################################################################
################## CLEANING SUMMER DATA ########################
################################################################

# Removing unreliable data (same process as before) 
sofia222 <- sofiaaa[ which(sofiaaa$P2<500 & sofiaaa$P2 > 10 & sofiaaa$humidity < 80), ]
counting <- data.frame(table(sofia222$geohash))
counting$keep <- ifelse(counting$Freq > 300, 1, 0)

names(counting)[names(counting) == "Var1"] <- "geohash" # for purpose of merging 

merged111 <- merge(sofia222, counting, all=TRUE, sort=FALSE)

sofia333 <- merged111[ which(merged111$keep==1), ] # removing sensors w less than 300 observations

# Grouping data by geohash 
sofia333 <- aggregate(sofia333, list(sofia333$geohash), mean)

# Removing row 29: average concentration of 331 - Weird.  
sofia333 <- sofia333[-c(29), ]

# Mapping the sensors that will be used for analysis and interpolation 
plot(shapes_sp)
plot(sofia333, col="red", add=TRUE)

################################################################
############### INTERPOLATION SUMMER DATA ######################
################################################################

# Converting pollution data to SP
sofia_sp3 <- as(sofia333, 'Spatial')
# Reading in Sofia shapefile as SP
shapes_sp <-readOGR("Neighbourhoods/GR_UNITS_20190122.shp", layer="GR_UNITS_20190122")

# Calculate range of shapefile
box <- bbox(shapes_sp)
x.range <- as.integer(box[1,])
y.range <- as.integer(box[2,])

# Create a grid from the values in points df. 
# Expand range to a grid with spacing that we would like to use in your interpolation
# Here we will use 200m grid cells
grd3 <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=200), y=seq(from=y.range[1], to=y.range[2], by=200))
# Convert grid to SpatialPixel class
coordinates(grd3) <- ~ x+y
gridded(grd3) <- TRUE

# Assigning Sofia pollution CRS to grid 
library(gstat)
proj4string(grd3) <- proj4string(sofia_sp3)

# Inverse-distance interpolation: 
idw3<-idw(formula=P2 ~ 1, locations=sofia_sp3, newdata=grd3)

# Converting interpolated data to raster
library(raster)
ras3 <- raster(idw3)
# Clip the raster to Sofia outline
rasmask3 <- mask(ras3, shapes_sp)

# Calculating mean P2.5 pollution per neighbourhood
av2.5_summer <- raster::extract(rasmask3, sofia_sp3, fun=mean, na.rm=TRUE)

# Since this data is ordered with our polygons we can just assign it into a new column "av2.5_summer".
shapes_sp@data$av2.5_summer <- unlist(raster::extract(rasmask3, shapes_sp, fun=mean, na.rm=TRUE))
shapes_sp@data$av2.5_summer <- as.vector(shapes_sp@data$av2.5_summer)

# mapping interpolated data 
library(tmap)

map_inter_summer <- tm_shape(shapes_sp) + 
  tm_fill("av2.5_summer", 
          style = "fixed", 
          breaks=c(10, 15, 20, 25, 30, 35, 40, 45, 50), 
          #palette = "OrRd",
          palette = c("#1b7837", "#5aae61", "#a6dba0", "#e7d4e8", "#c2a5cf", "#9970ab", "#762a83", "#40004b"),
          title = "Concentration (μg/m3)") +
  tm_borders("white", alpha=.5) + 
  tm_compass() 

map_inter_summer

mean(shapes_sp@data$av2.5_summer) # exploratory stat

################################################################
########################### GWR ################################
################################################################

# Converting some variables into numeric
shapes_sp$Nedu1_perc <- as.numeric(shapes_sp$Nedu1_perc)
shapes_sp$BUI78_TN <- as.numeric(shapes_sp$BUI78_TN)
shapes_sp$BUI2_AreaP  <- as.numeric(shapes_sp$BUI2_AreaP)

# New variables: proportion of buildings per neighbourhood with central or electric heating or both
shapes_sp$NJ17_eq_1_ <- as.numeric(shapes_sp$NJ17_eq_1_)
shapes_sp$NJ17_eq_3_ <- as.numeric(shapes_sp$NJ17_eq_3_)
shapes_sp$NN_HouseH_ <- as.numeric(shapes_sp$NN_HouseH_)

shapes_sp$perc_central <- (shapes_sp$NJ17_eq_1_ / shapes_sp$NN_HouseH_)*100
shapes_sp$perc_elec <- (shapes_sp$NJ17_eq_3_ / shapes_sp$NN_HouseH_)*100
shapes_sp$perc_central_elec <- ((shapes_sp$NJ17_eq_1_ + shapes_sp$NJ17_eq_3_) / shapes_sp$NN_HouseH_)*100

# New variable: propotion of bulidings not connected to heating system 
shapes_sp$NJ16_eq_3_ <- as.numeric(shapes_sp$NJ16_eq_3_)
shapes_sp$unconnected <- ((shapes_sp$NJ16_eq_3_) / shapes_sp$NN_HouseH_)*100

# Removing row 162 because unusual value of proportion of houses burning solid fuels (100%) 
shapes_sp<- shapes_sp[-c(162), ]

# Reading in extra data (income, line connectivity and cars per individual)
# Only income would turn out to be significant in the regression. 

shapes_sp2 <- shapes_sp
extradata <- read.csv('sofia_extradata.csv', strip.white=TRUE)
shapes_sp2@data <- merge(shapes_sp2@data, extradata, by='Rajon', all.x = TRUE)
shapes_sp2@data$Income2017 <- as.numeric(shapes_sp2@data$Income2017)

# Reading in topographic data (altitude) from Data Science Society. 
topo <- read.csv("sofia_topo.txt", header = F, sep = ",")
names(topo)[which(names(topo) == "V1")] <- "lat"
names(topo)[which(names(topo) == "V2")] <- "lon"

topo <- topo[c("lon", "lat", "V3")]
topo <- topo[-c(1), ]
topo$lat <- as.character(topo$lat)
topo$lon <- as.character(topo$lon)
topo$lat <- as.numeric(topo$lat)
topo$lon <- as.numeric(topo$lon)

# Transforming crs of topography data to that of Sofia shapefile
topo <- st_as_sf(topo, coords = c("lon", "lat"), crs = 4326) # Assigning it WGS84 CRS, because 'topo" is not projected
topo <-st_transform(topo, crs = 32634) # Assignign it CRS of Sofia shapefile
topo_sp <- as(topo, 'Spatial') # Converting topography to SPDF 

# Calculating the extent of the SPDF
e <- extent(bbox(topo_sp))

# Converting SPDF to normal DF to then transform the DF to a raster 
DF <- as.data.frame(topo_sp)

library(raster)
r <- raster(e, ncol=10, nrow=2) # Convert 'e' to raster

# Converting the data of the topo DF to numeric. 
# Need to convert to character first, because data was originally in vector form. 
DF$V3 <- as.character(DF$V3)
DF$V3 <- as.numeric(DF$V3) 
DF$coords.x1 <- as.character(DF$coords.x1)
DF$coords.x1 <- as.numeric(DF$coords.x1)
DF$coords.x2 <- as.character(DF$coords.x2)
DF$coords.x2 <- as.numeric(DF$coords.x2)

# Creating a raster of topography data, using the topo DF and the extent e, already in raster form. Cols 2 and 3 are the coordinated, col is the value (the altitude). 
x <- rasterize(DF[, (2:3)], r, DF[,1], fun=mean)

# Cropping the raster to the extent of the Sofia shapefile. 
altmask <- mask(x, shapes_sp)

# Calculating the mean altitude per neighbourhood.
av_alt<- raster::extract(altmask, shapes_sp, fun=mean, na.rm=TRUE)

# Transforming average altitude to DF for purpose of merging with Sofia shapefile. 
av_alt <- as.data.frame(av_alt)
av_alt <- av_alt[-c(162), ]
length(av_alt)
# Merging to Sofia shapefile. The data in the altidude DF is already ordered, so 'unlist' is enough to merge the altitude data to Sofia shapefile. 
shapes_sp2@data$alt <- unlist(av_alt)

# Removing polygons with missing data for GWR 
sum(is.na(shapes_sp2@data))  
library(spatialEco)
shapes_sp3 <- sp.na.omit(shapes_sp2) # 23 rows were omitted

# Fitting the actual model 
model_final <- lm(av2.5 ~ NJ17_4i_pe + GL_DensRZP + BUI2_AreaP + Income2017 + alt + perc_central, data = shapes_sp3)
View(shapes_sp3@data)
summary(model_final)
plot(model_final)
durbinWatsonTest(model_final) # for independent errors 
library(car) 
vif(model_final) # for multicolinearity

# No transformation can really improve the model. 
library(car)
boxCox(model_final, family="yjPower", plotit = TRUE)# Suggesting a 0.2 transformation of the outcome
depvar.transformed <- yjPower(av2.5, 0.2)

#shapes_sp4 <- sp.na.omit(shapes_sp3)
#model_final2 <- lm(depvar.transformed ~ NJ17_4i_pe + GL_DensRZP + BUI2_AreaP + Income2017 + alt + perc_central, data = shapes_sp4)

model_final3 <- lm(log(av2.5) ~ NJ17_4i_pe + GL_DensRZP + BUI2_AreaP + Income2017 + alt + perc_central, data = shapes_sp3)
summary(model_final3)
plot(model_final3)

# Doing the actual GWR 

library(spgwr)
GWRbandwidth <- gwr.sel(model_final, data=shapes_sp3, coords=coordsW,adapt=T)

#run the gwr model
gwr.model = gwr(model_final, data = shapes_sp3, coords=coordsW, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)

#print the results of the model
gwr.model

results<-as.data.frame(gwr.model$SDF)
names(results)

#attach coefficients to original dataframe
shapes_sp3@data$coefNJ17_4i_pe<-results$NJ17_4i_pe
shapes_sp3@data$coefGL_DensRZP<-results$GL_DensRZP
shapes_sp3@data$coefBUI2_AreaP<-results$BUI2_AreaP
shapes_sp3@data$coefIncome2017<-results$Income2017
shapes_sp3@data$coefalt<-results$alt
shapes_sp3@data$coefperc_central<-results$perc_central

################################################################
#################### MAPPING COEFFICIENTS ######################
################################################################

# Prop of hholds burning solid fuels 
tm_shape(shapes_sp3) +
  tm_polygons(col = "coefNJ17_4i_pe", palette = "PuRd", alpha=.5, title="") + 
  tm_shape(shapes_sp3) + 
  tm_borders("white", alpha=.5) +
  tm_compass() + 
  tm_layout(title = 'Solid fuels: coefficients')

# Building density
tm_shape(shapes_sp3) +
  tm_polygons(col = "coefGL_DensRZP", palette = "Reds", alpha=.5, title="") +
  tm_shape(shapes_sp3) + 
  tm_borders("white", alpha=.5) + 
  tm_compass() + 
  tm_layout(title = 'Building density: coefficients')

# Prop of office space
tm_shape(shapes_sp3) +
  tm_polygons(col = "coefBUI2_AreaP", palette = "BuGn", alpha=.5, title="") + 
  tm_shape(shapes_sp3) + 
  tm_borders("white", alpha=.5) + 
  tm_compass() + 
  tm_layout(title = 'Offices: coefficients')

# Monthly income
tm_shape(shapes_sp3) +
  tm_polygons(col = "coefIncome2017", palette = "Purples", alpha=.5, title="") + tm_shape(shapes_sp3) + 
  tm_borders("white", alpha=.5) + 
  tm_compass() + 
  tm_layout(title = 'Monthly income: coefficients')

# Altitude
tm_shape(shapes_sp3) +
  tm_polygons(col = "coefalt", palette = "YlOrBr", alpha=.5, title="") + 
  tm_shape(shapes_sp) + 
  tm_borders("white", alpha=.5) + 
  tm_compass() + 
  tm_layout(title = 'Average altitude: coefficients')

# Prop hholds connected to central heating 
tm_shape(shapes_sp3) +
  tm_polygons(col = "coefperc_central", palette = "Purples", alpha=.5, title="") + tm_shape(shapes_sp3) + 
  tm_borders("white", alpha=.5) + 
  tm_compass() + 
  tm_layout(title = 'Central heating: coefficients')

################################################################
##################A## MAPPING PREDICTORS #######################
################################################################

# prop of hholds burning solid fuels 
library(tmap)
solidfuels <- tm_shape(shapes_sp) + 
  tm_fill("NJ17_4i_pe", 
          style = "fixed", 
          breaks=c(0, 20, 40, 60, 80, 100), 
          palette = "OrRd", 
          title = "Proportion") +
  tm_borders("white", alpha=.8) + 
  tm_compass() 
solidfuels

# prop of hholds burning solid fuels - second distribution map 
library(tmap)
solidfuels2 <- tm_shape(shapes_sp) + 
  tm_fill("NJ17_4i_pe", 
          style = "fixed", 
          breaks=c(0, 20, 40, 60, 80, 100), 
          palette = "OrRd", 
          title = "") +
  tm_borders("white", alpha=.8) + 
  tm_compass() + tm_layout(title = 'Solid fuels: distribution')
solidfuels2

# building density 
buildingdensity <- tm_shape(shapes_sp) + 
  tm_fill("GL_DensRZP", 
          style = "fixed", 
          breaks=c(0, 0.40, 0.80, 1.20, 1.60, 2.00), 
          palette = "OrRd",
          title = "Index of building density") +
  tm_borders("white", alpha=.5) + 
  tm_compass() + 
  tm_layout(title = 'Building density: distribution')
buildingdensity

# prop of office space
offices <- tm_shape(shapes_sp) + 
  tm_fill("BUI2_AreaP", 
          style = "fixed", 
          breaks=c(0, 10, 20, 30, 40, 60, 80), 
          palette = "OrRd",
          title = "") +
  tm_borders("white", alpha=.8) + 
  tm_compass() + 
  tm_layout(title = 'Offices: distribution')
offices
