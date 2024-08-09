rm(list=ls())
#load packages
library(sf);library(raster);library(dplyr)

#Load the trait data

#list wds Eduardo
wd_output <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Rachel/Fogo'
wd_shp <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Rachel/Fogo/SpatialData'
wd_map_stuff <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Rachel/Fogo'
wd_data <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Rachel/Fogo'

#list wds Rachel
wd_output <- '"/Users/rachelsouzaferreira/Data/Chapter 1/Outputs'
wd_shp <- '/Users/rachelsouzaferreira/Desktop/SpatialData'
wd_data <- "/Users/rachelsouzaferreira/Data/Chapter 1/Outputs"
wd_map_stuff <- '/Users/rachelsouzaferreira/Dropbox/Data/mapa'

#load table
setwd(wd_output)
armature <- read.csv("WorldID_Mimosoid.csv", sep= ",")

armature$armature_description <- factor(armature$Armature_type,
                                        levels = c(0, 1, 2, 3, 4, 5, 13, 14, 34),
                                        labels = c("No_Armature", "Thorns", "Spinescent_shoots", "Prickles",
                                                   "Stipular_spines", "Axillary_spines", "Thorn_Prickle",
                                                   "Thorn_StipularSpine", "Prickle_StipularSpine"))


aggregate_data <- function(data, description) {
  return(aggregate(Armature ~ WorldID, data = subset(data, armature_description == description), sum))
}

No_Armature_agg <- aggregate_data(armature, "No_Armature")
Thorns_agg <- aggregate_data(armature, "Thorns")
Spinescent_shoots_agg <- aggregate_data(armature, "Spinescent_shoots")
Prickles_agg <- aggregate_data(armature, "Prickles")
Stipular_spines_agg <- aggregate_data(armature, "Stipular_spines")
Axillary_spines_agg <- aggregate_data(armature, "Axillary_spines")
Thorn_Prickle_agg <- aggregate_data(armature, "Thorn_Prickle")
Thorn_StipularSpine_agg <- aggregate_data(armature, "Thorn_StipularSpine")
Prickle_StipularSpine_agg <- aggregate_data(armature, "Prickle_StipularSpine")

# List of aggregated datasets and their corresponding names
agg_datasets <- list(No_Armature_agg, Thorns_agg, Spinescent_shoots_agg, Prickles_agg, 
                     Stipular_spines_agg, Axillary_spines_agg, Thorn_Prickle_agg,
                     Thorn_StipularSpine_agg, Prickle_StipularSpine_agg)

agg_names <- c("No_Armature", "Thorns", "Spinescent_shoots", "Prickles", "Stipular_spines", 
               "Axillary_spines", "Thorn_Prickle", "Thorn_StipularSpine", "Prickle_StipularSpine")

# Merge aggregated datasets with armature
for (i in 1:length(agg_datasets)) {
  armature <- merge(armature, agg_datasets[[i]], by="WorldID", all=TRUE)
  names(armature)[names(armature) == "Armature.y"] <- paste0("Armature_", agg_names[i])
  names(armature)[names(armature) == "Armature.x"] <- "Armature"
}


str(armature)

teste <- armature

teste$Taxon <- NULL
teste$Armature_type <- NULL
teste$Armature <- NULL
teste$armature_description <- NULL

# Assuming 'df' is your dataframe
duplicates <- duplicated(teste) | duplicated(teste, fromLast = TRUE)

# View the rows that are duplicates
duplicate_rows <- teste[duplicates, ]

# View the duplicate rows
print(duplicate_rows)

# Remove duplicates based on all columns
teste_no_duplicates <- distinct(teste)

# View the resulting dataframe with duplicates removed
print(teste_no_duplicates)

remove(agg_datasets,Axillary_spines_agg,duplicate_rows,No_Armature_agg,Prickle_StipularSpine_agg,Spinescent_shoots_agg,Stipular_spines_agg,Thorn_Prickle_agg,Thorn_StipularSpine_agg,Thorns_agg)
remove(Prickles_agg)

table <- teste_no_duplicates

##codigo para plotar variaveis em shapefile#
##feito por Eduardo Arle em Github##

#load fire table
setwd(wd_data)
# Read the shapefile
shp_grid <- st_read("BehrmannMeterGrid_WGS84_land",dsn = wd_shp)

table <- read.csv("/Users/rachelsouzaferreira/Dropbox/PhD/Chapter 1/Outputs/WorldID_Mimosoid.csv", sep= ",")

#include armature info into shp attribute table
shp_grid$Armature_Thorns  <- NA
shp_grid$Armature_Spinescent_shoots  <- NA
shp_grid$Armature_Prickles   <- NA
shp_grid$Armature_Stipular_spines  <- NA
shp_grid$Armature_Axillary_spines  <- NA
shp_grid$Armature_Thorn_Prickle   <- NA
shp_grid$Armature_Thorn_StipularSpine  <- NA
shp_grid$Armature_Prickle_StipularSpine  <- NA


for(i in 1:nrow(shp_grid))
{
  a <- which(table$WorldID == shp_grid$WorldID[i])
  
  if(length(a) > 0){
    shp_grid$Armature_Thorns[i] <- table$Armature_Thorns[a]
    shp_grid$Armature_Spinescent_shoots[i] <- table$Armature_Spinescent_shoots[a]
    shp_grid$Armature_Prickles[i] <- table$Armature_Prickles[a]
    shp_grid$Armature_Stipular_spines[i] <- table$Armature_Stipular_spines[a]
    shp_grid$Armature_Axillary_spines[i] <- table$Armature_Axillary_spines[a]
    shp_grid$Armature_Thorn_Prickle[i] <- table$Armature_Thorn_Prickle[a]
    shp_grid$Armature_Thorn_StipularSpine[i] <- table$Armature_Thorn_StipularSpine[a]
    shp_grid$Armature_Prickle_StipularSpine[i] <- table$Armature_Prickle_StipularSpine[a]
  }
  
  print(i)
}

### plot maps

### plot maps
setwd(wd_map_stuff)

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("worldmapframe.rds")

#### transform SpatialPolygons into sf object
worldmapframe <- st_as_sf(worldmapframe)

# Transform the sf object and frame
shp2 <- st_transform(shp_grid, crs = st_crs(world))
worldmapframe <- st_transform(worldmapframe, crs = st_crs(world))

# #create colour ramp to represent the Armatures
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
#                  Armatures = c(min(shp_grid$fireseason, na.rm = T), 
#                             max(shp_grid$fireseason, na.rm = T)),
#                  cex = 2)
# 
# myGradientLegend(valRange = c(min(shp_grid$fireseason, na.rm = T), 
#                               max(shp_grid$fireseason, na.rm = T)),
#                  pos=c(0.21,0.1,0.225,0.12),
#                  color = 'gray80',
#                  side = 1,
#                  n.seg = 0,
#                  Armatures = c(NA,'NA'),
#                  cex = 2)


#### alternative

#create colour ramp to represent the Armatures
colramp <- colorRampPalette(c("#3288bd", "#66c2a5", "#abdda4",
                              "#abdda4", "#e6f598", "#ffffbf",
                              "#fee08b", "#fdae61", "#f46d43",
                              "#d53e4f", "#9e0142"))                                       

## thorn

#populate the table with the colours to be plotted 
shp2$colours_thorn <- colramp(100)[cut(c(min(shp_grid$Armature_Thorns, na.rm =T),
                                         max(shp_grid$Armature_Thorns, na.rm =T),
                                         shp_grid$Armature_Thorns), 
                                       breaks = 100)][-c(1,2)]

shp2$colours_thorn[which(is.na(shp2$colours_thorn))] <- 'gray80'

#populate the table with the colours to be plotted 
shp2$colours_Armature_Spinescent_shoots <- colramp(100)[cut(c(min(shp_grid$Armature_Spinescent_shoots, na.rm =T),
                                                              max(shp_grid$Armature_Spinescent_shoots, na.rm =T),
                                                              shp_grid$Armature_Spinescent_shoots), 
                                                            breaks = 100)][-c(1,2)]


shp2$colours_Armature_Spinescent_shoots[which(is.na(shp2$colours_Armature_Spinescent_shoots ))] <- 'gray80'


#prickles 

#populate the table with the colours to be plotted 
shp2$colours_Armature_Prickles <- colramp(100)[cut(c(min(shp_grid$Armature_Prickles, na.rm =T),
                                                      max(shp_grid$Armature_Prickles, na.rm =T),
                                                      shp_grid$Armature_Prickles), 
                                                    breaks = 100)][-c(1,2)]

shp2$colours_Armature_Prickles[which(is.na(shp2$colours_Armature_Prickles))] <- 'gray80'

#Stipular spine

shp2$colours_Armature_Stipular_spines<- colramp(100)[cut(c(min(shp_grid$Armature_Stipular_spines, na.rm =T),
                                                           max(shp_grid$Armature_Stipular_spines, na.rm =T),
                                                           shp_grid$Armature_Stipular_spines), 
                                                         breaks = 100)][-c(1,2)]


shp2$colours_Armature_Stipular_spines[which(is.na(shp2$colours_Armature_Stipular_spines))] <- 'gray80'

#Axillary spines
shp2$colours_Armature_Axillary_spines <- colramp(100)[cut(c(min(shp_grid$Armature_Axillary_spines, na.rm =T),
                                                            max(shp_grid$Armature_Axillary_spines, na.rm =T),
                                                            shp_grid$Armature_Axillary_spines), 
                                                          breaks = 100)][-c(1,2)]


shp2$colours_Armature_Axillary_spines[which(is.na(shp2$colours_Armature_Axillary_spines))] <- 'gray80'

### PLOT 
##definir parametro de plot geral
par(mar=c(1,1,1,1), bg = 'white',mfrow = c(2,3))

##definir parametro de cada plot
cexo.main <- 3.5

poso1 <- c(0.15,0.19,0.63,0.21)
poso2 <- c(0.17,0.19,0.65,0.21)
poso3 <- c(0.19,0.19,0.67,0.21)
poso4 <- c(0.21,0.21,0.69,0.23)
poso5 <- c(0.21,0.23,0.69,0.25)
  
pose1 <- c(0.80,0.23,0.825,0.25)
pose2 <- c(0.79,0.23,0.815,0.25)


col_border1 <- '#000000'
col_border2 <- '#303030'

plot(st_geometry(shp2), border = NA , col = shp2$colours_thorn)
title("Thorn", line = -10, cex.main = 3.5, font.main = 2)
plot(worldmapframe , add = T, border = col_border2)

#plot(worldmapframe , add = T)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$Armature_Thorns, na.rm = T), 
                              max(shp_grid$Armature_Thorns, na.rm = T)),
                 pos = poso5,
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$Armature_Thorns, na.rm = T),
                           
                            max(shp_grid$Armature_Thorns, na.rm = T)),
                 cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$Armature_Thorns, na.rm = T), 
                              max(shp_grid$Armature_Thorns, na.rm = T)),
                 pos = pose1,
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)

#Spinescent_shoot
plot(st_geometry(shp2), border = NA , col = shp2$colours_Armature_Spinescent_shoots)
#plot(worldmapframe , add = T)
title("Spinescent Shoot", line = -8.5, cex.main = cexo.main, font.main = 2)
plot(worldmapframe , add = T, border = col_border1)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$Armature_Spinescent_shoots, na.rm = T), 
                              max(shp_grid$Armature_Spinescent_shoots, na.rm = T)),
                 pos = poso2,
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$Armature_Spinescent_shoots, na.rm = T), 
                            max(shp_grid$Armature_Spinescent_shoots, na.rm = T)),
                 cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$Armature_Spinescent_shoots, na.rm = T), 
                              max(shp_grid$Armature_Spinescent_shoots, na.rm = T)),
                 pos = pose2,
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)
### PLOT
plot(st_geometry(shp2), border = NA , col = shp2$colours_Armature_Prickles)
#plot(worldmapframe , add = T)
title("Prickle", line = -9, cex.main = cexo.main, font.main = 2)
plot(worldmapframe , add = T, border = col_border1)
#plot icons legend
myGradientLegend(valRange = c(min(shp_grid$Armature_Prickles, na.rm = T), 
                              max(shp_grid$Armature_Prickles, na.rm = T)),
                 pos = poso3,
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$Armature_Prickles, na.rm = T), 
                            max(shp_grid$Armature_Prickles, na.rm = T)),
                 cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$Armature_Prickles, na.rm = T), 
                              max(shp_grid$Armature_Prickles, na.rm = T)),
                 pos = pose3,
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)

### PLOT
plot(st_geometry(shp2), border = NA , col = shp2$colours_Armature_Stipular_spines)
#plot(worldmapframe , add = T)
title("Stipular spines", line = -9.5, cex.main = cexo.main, font.main = 2)
plot(worldmapframe , add = T, border = col_border2)
#plot icons legend
myGradientLegend(valRange = c(min(shp_grid$Armature_Stipular_spines, na.rm = T), 
                              max(shp_grid$Armature_Stipular_spines, na.rm = T)),
                 pos = poso4,
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$Armature_Stipular_spines, na.rm = T), 
                            max(shp_grid$Armature_Stipular_spines, na.rm = T)),
                 cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$Armature_Stipular_spines, na.rm = T), 
                              max(shp_grid$Armature_Stipular_spines, na.rm = T)),
                 pos = pose4,
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)

### PLOT
plot(st_geometry(shp2), border = NA , col = shp2$colours_Armature_Axillary_spines)
#plot(worldmapframe , add = T)
title("Axillary spines", line = -10, cex.main = cexo.main, font.main = 2)
plot(worldmapframe , add = T, border = col_border2)
#plot icons legend
myGradientLegend(valRange = c(min(shp_grid$Armature_Axillary_spines, na.rm = T), 
                              max(shp_grid$Armature_Axillary_spines, na.rm = T)),
                 pos = poso5,
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$Armature_Axillary_spines, na.rm = T), 
                            max(shp_grid$Armature_Axillary_spines, na.rm = T)),
                 cex = 1.5)

myGradientLegend(valRange = c(min(shp_grid$Armature_Axillary_spines, na.rm = T), 
                              max(shp_grid$Armature_Axillary_spines, na.rm = T)),
                 pos = pose5,
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 1.5)
