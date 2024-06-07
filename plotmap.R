##codigo para plotar variaveis em shapefile#
##feito por Eduardo Arle em Github##
rm(list=ls())
# Install and load the sf package if you haven't already done so

#load packages
library(sf);library(raster)

#list wds
wd_shp <- '/Users/rachelsouzaferreira/Desktop/SpatialData'
wd_data <- "/Users/rachelsouzaferreira/Data/Chapter 1/Outputs"
wd_map_stuff <- '/Users/rachelsouzaferreira/Dropbox/Data/mapa'


### get meanBA in grid cells 

###################################################################################
### this is very bad practice and makes impossioble to collaborate on GitHub, the WDs 
#must be listed in the top on the code and refered by their name
###################################################################################

setwd(wd_data)
table <- read.csv("Mapping_variables_2_april.csv")


# Read the shapefile
shp_grid <- st_read("BehrmannMeterGrid_WGS84_land",dsn = wd_shp)

st_geometry_type(shp_grid)


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
    shp_grid$extinct[i] <- table$extinct[a]
    shp_grid$extant[i] <- table$extant[a]
    shp_grid$plant_richness[i] <- table$plant_richness[a]
    shp_grid$spines[i] <- table$spines[a]
    shp_grid$no_spines[i] <- table$no_spines[a]
    shp_grid$dry[i] <- table$legthDry[a]
    shp_grid$temp[i] <- table$MeanTemp[a]
  }
  
  print(i)
}

### plot maps
setwd(wd_map_stuff)

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("worldmapframe.rds")

#### transform SpatialPolygons into sf object
worldmapframe <- st_as_sf(worldmapframe)

# Transform the sf object and frame
shp2 <- st_transform(shp_grid, crs = st_crs(world))
worldmapframe <- st_transform(worldmapframe, crs = st_crs(world))

###plot

plot(st_geometry(shp2))
plot(st_geometry(worldmapframe), add= T)
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


#log transform value and make the minimum equal 0 for plotting only
trans_values <-  log(shp_grid$meanBA) + 
  abs(min(log(shp_grid$meanBA), na.rm = T))


# ##### tests #####
# 
# a <- which(!is.na(shp_grid$meanBA))
# b <- which.max(shp_grid$meanBA)
# 
# 
# t <- shp_grid$meanBA[a[1]]
# v <- shp_grid$meanBA[a[991]]
# max <- shp_grid$meanBA[b]
# c <- 0.1875
# d <- 0.375
# e <- 0.5625
# 
# t2 <- log(t) + abs(min(log(shp_grid$meanBA), na.rm = T))
# v2 <- log(v) + abs(min(log(shp_grid$meanBA), na.rm = T))
# max2 <- log(max) + abs(min(log(shp_grid$meanBA), na.rm = T))
# c2 <- log(c) + abs(min(log(shp_grid$meanBA), na.rm = T))
# d2 <- log(d) + abs(min(log(shp_grid$meanBA), na.rm = T))
# e2 <- log(e) + abs(min(log(shp_grid$meanBA), na.rm = T))
# 
# 
# t3 <- exp(t2 - abs(min(log(shp_grid$meanBA), na.rm = T)))
# v3 <- exp(v2 - abs(min(log(shp_grid$meanBA), na.rm = T)))
# max3 <- exp(max2 - abs(min(log(shp_grid$meanBA), na.rm = T)))
# c3 <- exp(c2 - abs(min(log(shp_grid$meanBA), na.rm = T)))
# d3 <- exp(d2 - abs(min(log(shp_grid$meanBA), na.rm = T)))
# e3 <- exp(e2 - abs(min(log(shp_grid$meanBA), na.rm = T)))
# 
# max_1_4 <- exp(max2/4 - abs(min(log(shp_grid$meanBA), na.rm = T)))
# max_1_2 <- exp(max2/2 - abs(min(log(shp_grid$meanBA), na.rm = T)))
# max_3_4 <- exp(max2/4*3 - abs(min(log(shp_grid$meanBA), na.rm = T)))
# 
# max4 <- exp(log(max(shp_grid$meanBA, na.rm = T)) + 
#               abs(min(log(shp_grid$meanBA), na.rm = T)) -
#               abs(min(log(shp_grid$meanBA), na.rm = T)))
# 
# hist(shp_grid$meanBA)
# boxplot(shp_grid$meanBA)
# summary(shp_grid$meanBA)
# 
# round(exp((log(max(shp_grid$meanBA, na.rm = T)) + 
#              abs(min(log(shp_grid$meanBA), na.rm = T)) *3 / 4) -
#             abs(min(log(shp_grid$meanBA), na.rm = T))),3)


#################

#populate the table with the colours to be plotted 
shp2$colours_fire <- colramp(100)[cut(trans_values, breaks = 100)]

#set NAs to be plot in grey
shp2$colours_fire[which(is.na(shp2$colours_fire))] <- 'gray80'

#get max and min values to plot the legend
min_fire <- min(shp_grid$meanBA, na.rm = T)
max_fire <- max(shp_grid$meanBA, na.rm = T)

delta_fire <- max_fire - min_fire

#get log transformed values to plot the legend
trans_min_fire <- min(trans_values, na.rm = T)
trans_max_fire <- max(trans_values, na.rm = T)

trans_min_diff <- abs(min(log(shp_grid$meanBA), na.rm = T))

#list values for the plot
values <-  c(min_fire,             
             exp(trans_max_fire * 2/4 - trans_min_diff),
             max_fire)

#set scientific notation (to deal with tiny numbers)
values_2 <- formatC(values, format = "fg") 



#populate the table with the colours to be plotted 
shp2$colours_spines <- colramp(100)[cut(c(min(shp_grid$spines, na.rm =T),
                                          max(shp_grid$spines, na.rm =T),
                                          shp_grid$spines), 
                                        breaks = 100)][-c(1,2)]


shp2$colours_spines[which(is.na(shp2$colours_spines ))] <- 'gray80'

#populate the table with the colours to be plotted 
shp2$colours_extinct <- colramp(100)[cut(c(min(shp_grid$extinct, na.rm =T),
                                           max(shp_grid$extinct, na.rm =T),
                                           shp_grid$extinct), 
                                         breaks = 100)][-c(1,2)]

shp2$colours_extinct[which(is.na(shp2$colours_extinct))] <- 'gray80'

#populate the table with the colours to be plotted 
shp2$colours_extant <- colramp(100)[cut(c(min(shp_grid$extant, na.rm =T),
                                          max(shp_grid$extant, na.rm =T),
                                          shp_grid$extant), 
                                        breaks = 100)][-c(1,2)]

shp2$colours_extant[which(is.na(shp2$colours_extant))] <- 'gray80'

#populate the table with the colours to be plotted 
shp2$colours_temp <- colramp(100)[cut(c(min(shp_grid$temp, na.rm =T),
                                        max(shp_grid$temp, na.rm =T),
                                        shp_grid$temp), 
                                      breaks = 100)][-c(1,2)]


shp2$colours_temp[which(is.na(shp2$colours_temp))] <- 'gray80'

shp2$colours_dry <- colramp(100)[cut(c(min(shp_grid$dry, na.rm =T),
                                       max(shp_grid$dry, na.rm =T),
                                       shp_grid$dry), 
                                     breaks = 100)][-c(1,2)]

shp2$colours_dry[which(is.na(shp2$colours_dry))] <- 'gray80'

### PLOT 
par(mar=c(1,1,1,1), bg = 'white',mfrow = c(2,3))
plot(st_geometry(shp2), border = NA , col = shp2$colours_spines)
title("Spines", line = -7, cex.main = 3.5, font.main = 2)
plot(worldmapframe , add = T)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$spines, na.rm = T), 
                              max(shp_grid$spines, na.rm = T)),
                 pos=c(0.15,0.19,0.63,0.21),
                 color = colramp(100),
                 side = 1,
                 n.seg = 1,
                 values = c(min(shp_grid$spines, na.rm = T),
                            max(shp_grid$spines, na.rm = T)/2,
                            max(shp_grid$spines, na.rm = T)),
                            cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$spines, na.rm = T), 
                              max(shp_grid$spines, na.rm = T)),
                 pos=c(0.75,0.19,0.775,0.21),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)
#morto
plot(st_geometry(shp2), border = NA , col = shp2$colours_extinct)
plot(worldmapframe , add = T)
title("Extinct", line = -7, cex.main = 3.5, font.main = 2)

myGradientLegend(valRange = c(min(shp_grid$extinct, na.rm = T), 
                              max(shp_grid$extinct, na.rm = T)),
                 pos=c(0.15,0.19,0.63,0.21),
                 color = colramp(100),
                 side = 1,
                 n.seg = 1,
                 values = c(min(shp_grid$extinct, na.rm = T), 
                            max(shp_grid$extinct, na.rm = T)/2,
                            max(shp_grid$extinct, na.rm = T)),
                 cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$extinct, na.rm = T), 
                              max(shp_grid$extinct, na.rm = T)),
                 pos=c(0.75,0.19,0.775,0.21),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)

#vivo
plot(st_geometry(shp2), border = NA , col = shp2$colours_extant)
plot(worldmapframe , add = T)
title("Extant", line = -7, cex.main = 3.5, font.main = 2)


myGradientLegend(valRange = c(min(shp_grid$extant, na.rm = T), 
                              max(shp_grid$extant, na.rm = T)),
                 pos=c(0.15,0.19,0.63,0.21),
                 color = colramp(100),
                 side = 1,
                 n.seg = 1,
                 values = c(min(shp_grid$extant, na.rm = T), 
                            max(shp_grid$extant, na.rm = T)/2,
                            max(shp_grid$extant, na.rm = T)),
                 cex = 1.5)



myGradientLegend(valRange = c(min(shp_grid$extant, na.rm = T), 
                              max(shp_grid$extant, na.rm = T)),
                 pos=c(0.75,0.19,0.775,0.21),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)


### plot temperature ##

plot(st_geometry(shp2), border = NA , col = shp2$colours_temp)
plot(worldmapframe , add = T)
title("Mean Temperature", line = -7, cex.main = 3.5, font.main = 2)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$temp, na.rm = T), 
                              max(shp_grid$temp, na.rm = T)),
                 pos=c(0.15,0.19,0.63,0.21),
                 color = colramp(100),
                 side = 1,
                 n.seg = 1,
                 values = c(round(min(shp_grid$temp, na.rm = T),3), 
                            round(max(shp_grid$temp, na.rm = T)/2,3),
                            round(max(shp_grid$temp, na.rm = T),3)),
                 cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$temp, na.rm = T), 
                              max(shp_grid$temp, na.rm = T)),
                 pos=c(0.75,0.19,0.775,0.21),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)
#dry

plot(st_geometry(shp2), border = NA , col = shp2$colours_dry)
plot(worldmapframe , add = T)
title("Length of Dry Season", line = -7, cex.main = 3.5, font.main = 2)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$dry, na.rm = T), 
                              max(shp_grid$dry, na.rm = T)),
                 pos=c(0.15,0.19,0.63,0.21),
                 color = colramp(100),
                 side = 1,
                 n.seg = 1,
                 values = c(min(shp_grid$dry, na.rm = T),
                            max(shp_grid$dry, na.rm = T)/2,
                            max(shp_grid$dry, na.rm = T)),
                 cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$dry, na.rm = T), 
                              max(shp_grid$dry, na.rm = T)),
                 pos=c(0.75,0.19,0.775,0.21),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)

##fire

plot(st_geometry(shp2), border = NA , col = shp2$colours_fire)
plot(worldmapframe , add = T)
title("Fire Frequency", line = -7, cex.main = 3.5, font.main = 2)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min_fire, max_fire), 
                 pos = c(0.15,0.19,0.63,0.21),
                 color = colramp(100), 
                 side = 1,
                 n.seg = c(min_fire,
                           delta_fire / 2,
                           max_fire),
                 values = values_2,
                 cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$meanBA, na.rm = T), 
                              max(shp_grid$meanBA, na.rm = T)),
                 pos=c(0.75,0.19,0.775,0.21),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)


# burden <- 6638

# t <- c("0",
#        paste(round(exp(log(max(burden))/4))),
#        paste(round(exp(log(max(burden))/2))),
#        paste(round(exp(log(max(burden))*3/4))),
#        paste(max(burden)))
# 
# 
# log(shp_grid$meanBA) + abs(min(log(shp_grid$meanBA), na.rm = T))

#plot Extinct animal presence


### PLOT


#plot Extant animal presence



### PLOT

### plot temperature ##


### PLOT
par(mar=c(2,2,2,2), bg = 'white')
plot(st_geometry(shp2), border = NA , col = shp2$colours_temp)
plot(worldmapframe , add = T)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$temp, na.rm = T), 
                              max(shp_grid$temp, na.rm = T)),
                 pos=c(0.27,0.1,0.68,0.12),
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$temp, na.rm = T), 
                            max(shp_grid$temp, na.rm = T)),
                 cex = 2)

myGradientLegend(valRange = c(min(shp_grid$temp, na.rm = T), 
                              max(shp_grid$temp, na.rm = T)),
                 pos=c(0.73,0.1,0.743,0.12),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 2)

### plot dry ###

### plot temperature ##

#populate the table with the colours to be plotted 


### PLOT





####### plot all ####

par(mfrow=c(2, 3), mar=c(1, 1, 1, 1))