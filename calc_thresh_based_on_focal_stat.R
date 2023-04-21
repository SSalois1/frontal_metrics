library(raster)
library(RColorBrewer)
library(ncdf4)
library(wesanderson)
library(terra)
## Functions
fx.sum.greater.than.zero <- function(x) sum(na.omit(x > 0))
fx.sum <- function(x) sum(na.omit(x))
fx.sumsqrs <- function(x) {sum((na.omit(x) - mean(na.omit(x)))^2)}
# create an if-else reclassify statement
rc <- function(x1) {
  ifelse(x1 <= pers_thresh, 1, 
         ifelse((x1 > pers_thresh), 0))
}
## Color Palette
# Continuous palette for displaying smoother images
cols = rev(brewer.pal(11,'RdYlBu'))
pal <- colorRampPalette(cols)(30)
## Getting files
path = here::here('nc_files/ww')
files <- list.files(path = path, pattern = glob2rx('WW_2021*.nc'), 
                    full.names = TRUE)
## Modis
nc <- nc_open(files[1])  # Open files
lat <- ncvar_get(nc, 'latitude')
lon <- ncvar_get(nc, 'longitude')
grad.m <- ncvar_get(nc, attributes(nc$var)$names[2])
x.m <- ncvar_get(nc, attributes(nc$var)$names[3])
y.m <- ncvar_get(nc, attributes(nc$var)$names[4])
nc_close(nc)
rm(nc)

## Mur
nc <- nc_open(files[4])  # Open files
lat <- ncvar_get(nc, 'latitude')
lon <- ncvar_get(nc, 'longitude')
grad.mur <- ncvar_get(nc, attributes(nc$var)$names[1])
x.mur <- ncvar_get(nc, attributes(nc$var)$names[2])
y.mur <- ncvar_get(nc, attributes(nc$var)$names[3])
nc_close(nc)
rm(nc)





