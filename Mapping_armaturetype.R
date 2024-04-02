##codigo para plotar variaveis em shapefile#
##feito por Eduardo Arle em Github##
rm(list=ls())
# Install and load the sf package if you haven't already done so

#load packages
library(sf);library(raster)

#list wds
wd_shp <- '/Users/rachelsouzaferreira/Dropbox/PhD/Chapter 1/Data/SpatialData'
wd_data <- "/Users/rachelsouzaferreira/Desktop"
wd_map_stuff <- '/Users/rachelsouzaferreira/Desktop'

### get meanBA in grid cells 

table <- read.csv("/Users/rachelsouzaferreira/Dropbox/PhD/Chapter 1/SEMS_chapter1/Data/mapping_variables_2_april.csv")


# Read the shapefile
shp_grid <- st_read("BehrmannMeterGrid_WGS84_land", dsn ="/Users/rachelsouzaferreira/Desktop/SpatialData")
st_geometry_type(shp_grid)
st_write(shp_grid, "/Users/rachelsouzaferreira/Dropbox/PhD/Chapter 1/Data/SpatialData/BehrmannMeterGrid_WGS84_land.shp", driver = "ESRI Shapefile",append=FALSE)

# Check the structure of the shapefile and plot
str(shp_grid)
st_crs(shp_grid)
st_bbox(shp_grid)

plot(st_geometry(shp_grid)) ## st_geometry to select features in sf

#load fire table
setwd(wd_data)


#include fireseason info into shp attribute table
shp_grid$meanBA <- NA
shp_grid$extinct <- NA
shp_grid$extant <- NA
shp_grid$plant_richness <- NA
shp_grid$no_spines <- NA
shp_grid$spines <- NA
shp_grid$dry <- NA
shp_grid$temp <- NA

for(i in 1:nrow(shp_grid))
{
  a <- which(table$WorldID == shp_grid$WorldID[i])
  
  if(length(a) > 0){
    shp_grid$meanBA[i] <- table$meanBA[a]
    # shp_grid$extinct[i] <- table$extinct[a]
    # shp_grid$extant[i] <- table$extant[a]
    # shp_grid$plant_richness[i] <- table$plant_richness[a]
    # shp_grid$spines[i] <- table$spines[a]
    # shp_grid$no_spines[i] <- table$no_spines[a]
    # shp_grid$dry[i] <- table$legthDry[a]
    # shp_grid$temp[i] <- table$MeanTemp[a]
  }
  
  print(i)
}

### plot maps


world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

# # reproject everythign to Eckert
# worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
# shp2 <- spTransform(shp_grid,CRS(proj4string(world)))

#### transform SpatialPolygons into sf object
worldmapframe <- st_as_sf(worldmapframe)

# Transform the sf object and frame
shp2 <- st_transform(shp_grid, crs = st_crs(world))
worldmapframe <- st_transform(worldmapframe,crs = st_crs(world))

###plot

plot(st_geometry(shp2))
plot(st_geometry(worldmapframe),add= T)
# #create colour ramp to represent the values
# colramp <- colorRampPalette(c("#9e0142", "#d53e4f", "#f46d43",
#                               "#fdae61", "#fee08b", "#ffffbf",
#                               "#e6f598", "#abdda4", "#66c2a5",
#                               "#3288bd", "#5e4fa2"))
#                                        
# 
# #populate the table with the colours to be plotted 
# shp2$colours_fire <- colramp(100)[cut(c(0,9,shp_grid$fireseason), 
#                                  breaks = 100)][-c(1,2)]
# 
# shp2$colours_fire[which(is.na(shp2$colours_fire))] <- 'gray80'
# 
# ### PLOT
# par(mar=c(2,2,2,2), bg = 'white')
# plot(shp2, border = NA , col = shp2$colours_fire)
# plot(worldmapframe , add = T)
# 
# #plot icons legend (first load function)
# myGradientLegend(valRange = c(min(shp_grid$fireseason, na.rm = T), 
#                               max(shp_grid$fireseason, na.rm = T)),
#                  pos=c(0.3,0.1,0.8,0.12),
#                  color = colramp(100),
#                  side = 1,
#                  n.seg = 0,
#                  values = c(min(shp_grid$fireseason, na.rm = T), 
#                             max(shp_grid$fireseason, na.rm = T)),
#                  cex = 2)
# 
# myGradientLegend(valRange = c(min(shp_grid$fireseason, na.rm = T), 
#                               max(shp_grid$fireseason, na.rm = T)),
#                  pos=c(0.21,0.1,0.225,0.12),
#                  color = 'gray80',
#                  side = 1,
#                  n.seg = 0,
#                  values = c(NA,'NA'),
#                  cex = 2)


#### alternative

#create colour ramp to represent the values
colramp <- colorRampPalette(c("#3288bd", "#66c2a5", "#abdda4",
                                       "#abdda4", "#e6f598", "#ffffbf",
                                       "#fee08b", "#fdae61", "#f46d43",
                                       "#d53e4f", "#9e0142"))                                       
                                       
#populate the table with the colours to be plotted 
shp2$colours_fire <- colramp(100)[cut(c(0,9,shp_grid$meanBA), 
                                      breaks = 100)][-c(1,2)]

shp2$colours_fire[which(is.na(shp2$colours_fire))] <- 'gray80'
  
### PLOT
par(mar=c(2,2,2,2), bg = 'white')
plot(st_geometry(shp2), border = NA , col = shp2$colours_fire)
plot(worldmapframe , add = T)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$meanBA, na.rm = T), 
                              max(shp_grid$meanBA, na.rm = T)),
                 pos=c(0.27,0.1,0.68,0.12),
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(round(min(shp_grid$meanBA, na.rm = T)), 
                            round(max(shp_grid$meanBA, na.rm = T))),
                 cex = 2)

myGradientLegend(valRange = c(min(shp_grid$meanBA, na.rm = T), 
                              max(shp_grid$meanBA, na.rm = T)),
                 pos=c(0.73,0.1,0.743,0.12),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 2)


#plot phylacine animal presence

#populate the table with the colours to be plotted 
shp2$colours_extinct <- colramp(100)[cut(c(min(shp_grid$extinct, na.rm =T),
                                             max(shp_grid$extinct, na.rm =T),
                                             shp_grid$extinct), 
                                           breaks = 100)][-c(1,2)]


shp2$colours_extinct[which(is.na(shp2$colours_extinct))] <- 'gray80'
  
### PLOT
par(mar=c(2,2,2,2), bg = 'white')
plot(shp2, border = NA , col = shp2$colours_extinct)
plot(worldmapframe , add = T)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$phylacine, na.rm = T), 
                              max(shp_grid$phylacine, na.rm = T)),
                 pos=c(0.27,0.1,0.68,0.12),
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$phylacine, na.rm = T), 
                            max(shp_grid$phylacine, na.rm = T)),
                 cex = 2)

myGradientLegend(valRange = c(min(shp_grid$phylacine, na.rm = T), 
                              max(shp_grid$phylacine, na.rm = T)),
                 pos=c(0.73,0.1,0.743,0.12),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 2)


#plot iucn animal presence

#populate the table with the colours to be plotted 
shp2$colours_extant <- colramp(100)[cut(c(0,9,shp_grid$extant), 
                                      breaks = 100)][-c(1,2)]

shp2$colours_extant[which(is.na(shp2$colours_extant))] <- 'gray80'
  
### PLOT
par(mar=c(2,2,2,2), bg = 'white')
plot(shp2, border = NA , col = shp2$colours_extant)
plot(worldmapframe , add = T)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$iucn, na.rm = T), 
                              max(shp_grid$iucn, na.rm = T)),
                 pos=c(0.27,0.1,0.68,0.12),
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$iucn, na.rm = T), 
                            max(shp_grid$iucn, na.rm = T)),
                 cex = 2)

myGradientLegend(valRange = c(min(shp_grid$iucn, na.rm = T), 
                              max(shp_grid$iucn, na.rm = T)),
                 pos=c(0.73,0.1,0.743,0.12),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 2)




#plot phylacine animal presence

#populate the table with the colours to be plotted 
shp2$colours_spines <- colramp(100)[cut(c(min(shp_grid$spines, na.rm =T),
                                             max(shp_grid$spines, na.rm =T),
                                             shp_grid$spines), 
                                           breaks = 100)][-c(1,2)]


shp2$colours_spines[which(is.na(shp2$colours_spines ))] <- 'gray80'
  
### PLOT
par(mar=c(2,2,2,2), bg = 'white')
plot(shp2, border = NA , col = shp2$colours_spines)
plot(worldmapframe , add = T)




#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$spines, na.rm = T), 
                              max(shp_grid$spines, na.rm = T)),
                 pos=c(0.27,0.1,0.68,0.12),
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$spines, na.rm = T), 
                            max(shp_grid$spines, na.rm = T)),
                 cex = 2)

myGradientLegend(valRange = c(min(shp_grid$spines, na.rm = T), 
                              max(shp_grid$spines, na.rm = T)),
                 pos=c(0.73,0.1,0.743,0.12),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 2)







#populate the table with the colours to be plotted 
shp2$colours_spines <- colramp(100)[cut(c(min(shp2$spines, na.rm =T),
                                             max(shp2$spines, na.rm =T),
                                             shp_grid$spines), 
                                           breaks = 100)][-c(1,2)]


shp2$colours_phylacine[which(is.na(shp2$colours_phylacine ))] <- 'gray80'
  

###plot plant richness

par(mar=c(2,2,2,2), bg = 'white')
plot(shp2, border = NA , col = shp2$plant_richness)
plot(worldmapframe , add = T)




#populate the table with the colours to be plotted 
shp2$ <- colramp(100)[cut(c(min(shp_grid$iucn, na.rm =T),
                                        max(shp_grid$iucn, na.rm =T),
                                        shp_grid$iucn), 
                                      breaks = 100)][-c(1,2)]


shp2$colours_phylacine[which(is.na(shp2$colours_phylacine ))] <- 'gray80'


####### plot all ####

par(mfrow = c(2,3))



