################################################
#                                              #
#     PREPARATION OF LAYERS FOR CONDATIS       #
#      *BEFORE AND AFTER INTERVENTION*         #    
#                                              #
################################################
## CLAUDIA GUTIERREZ, AUGUST 2022 ###

#This script is designed to obtain the two layers required by Condatis — 'habitat' and 'source & tagets'— in '*.tif' format 

#Processing is limited to a maximum 60,000 (six thousand) cells in the study area. A section of the code allows to validate the condition of <60000 cells to either proceed with the analysis or reset the extent and resolution of the AOI. 

# DATA REQUIRED
#1. Habitat raster: e.g. CEH Land Cover Map 2020 (used here with a 25m resolution)
#2. B-Line projects shapefile: e.g. 'Shropshire_projects' map
#3. Data quality cvs file ('*.csv') with two columns: ID (land cover class ID) and QUAL(quality value from 0 to 1)

######################################################################
library(rgdal)
library(raster)
library(terra)
library(ggplot2)
library(rgeos)
library(dplyr)

#memory.limit(size=30000)


# Read habitat raster -----------------------------------------------------

#Read CEH Land Cover Map raster of the AOI

hab<-raster('spatialdata/SSLC.tif')
hab[hab>21] <- NA

plot(hab)

# Evaluate Condatis condition ---------------------------------------------

#Evaluate pixel count of AOI, if there are >60000 pixels in the AOI the script will stop

hab1<-setValues(hab, 1)
plot(hab1)

habcount<-cellStats(hab1, 'sum')
{
  if (habcount>600000)
    stop("too many cells")
  
  # Define habitat quality values in raster
  qual<- as.matrix(read.csv("spatialdata/hab_qual.csv", header=TRUE))#habitat quality based on Buglife's Resistance Layer and expert opinion  
  hab_qual<-reclassify(hab, qual) #replace habitat ID with quality value
  
  #save tif for Condatis analysis
  crs (hab_qual)<-"EPSG:27700"
  writeRaster(hab_qual,"spatialdata/habitat.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat.tif',col = topo.colors(20,rev = TRUE),zlim=c(0,1))
}

habcount

# Add B-line projects to habitat layer ------------------------------------

#Read South Shropshire projects shapefile
SS<- readOGR("spatialdata", "SSproj")
plot(SS)

SS$qual<- 1 #add a column with the habitat quality value

# Rasterize the shapefile with the raster "template" (r) used above
SSr <- rasterize(SS, hab,'qual')
crs (SSr)<-"EPSG:27700"
plot(SSr)

# Add SSr to hab_qual (habitat.tif)

hab_bl<- raster:::.ifel(SSr > 0, 1,hab_qual)#if SSr==1 then change value in hab_qual to 1
plot(hab_bl)

hab_blcount<-raster:::.ifel(hab_bl> 0, 1,hab_bl)
plot(hab_blcount)

{hab_blcount<-cellStats(hab_bl, 'sum')
  if (hab_blcount>6000000)
    stop("too many cells")
  
  #save habitat layer with including B-lin projects  
  crs (hab_bl)<-"EPSG:27700"
  writeRaster(hab_bl,"spatialdata/habitatBL.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL.tif',  col = topo.colors(20,rev = TRUE),zlim=c(0,1))
}


# Create Source and targets for AOI ------------------------------------

#movement SOUTH to NORTH
st1 <- raster(hab1) #raster with the extent and resolution of the habitat layer 
st1[]<-NA

st1@nrows
stSN<- as.matrix(st1)
stSN[1,]<-2
stSN[st1@nrows, ]<-1  
st1[]<-stSN

plot(st1,col=c("magenta", "cyan3"), main= 'stSN.tif')
st1

crs (st1)<-"EPSG:27700"
writeRaster(st1,"spatialdata/stSN.tif", overwrite=TRUE)



# movement NORTH TO SOUTH
st2 <- raster(hab1)  
st2[]<-NA

stNS<- as.matrix(st2)
stNS[1,]<-1
stNS[st2@nrows, ]<-2
st2[]<-stNS

plot(st2,col=c("magenta", "cyan3"), main= 'stNS.tif')
st2

crs (st2)<-"EPSG:27700"
writeRaster(st2,"spatialdata/stNS.tif", overwrite=TRUE)


#movement WEST TO EAST

st3 <- raster(hab1) 
st3[]<-NA

stWE<- as.matrix(st3)
stWE[,1]<-1
stWE[,st3@ncols ]<-2
st3[]<-stWE

plot(st3,col=c("magenta", "cyan3"), main= 'stWE.tif')
st3

crs (st3)<-"EPSG:27700"
writeRaster(st3,"spatialdata/stWE.tif", overwrite=TRUE)

#movement EAST TO WEST

st4 <- raster(hab1)  
st4[]<-NA

stEW<- as.matrix(st4)
stEW[,1]<-2
stEW[,st3@ncols ]<-1
st4[]<-stEW

plot(st4,col=c("magenta", "cyan3"), main= 'stEW.tif')
st4

crs (st4)<-"EPSG:27700"
writeRaster(st4,"spatialdata/stEW.tif", overwrite=TRUE)




