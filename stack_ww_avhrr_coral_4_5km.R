library(raster)
library(RColorBrewer)
library(ncdf4)
library(wesanderson)
### --- SST AVHRR 4 KM --- ### 
#setwd(here::here())
path = here::here('nc_files/ww')
files <- list.files(path = path, pattern = glob2rx('WW_2021*.nc'), 
                    full.names = TRUE)
### --- SST CORAL 5 KM --- ### 
path = here::here('nc_files/ww')
files <- list.files(path = path, pattern = glob2rx('WW_2021*.nc'), 
                    full.names = TRUE)
files <- list.files(path = path, pattern = glob2rx('W_202142*.nc'), 
                    full.names = TRUE)
### --- setting things up --- ## 
threshold = 0.18 # threshold for classifying front at 2km (0.15-0.18) 0.12
threshold = 0.12 # threshold for classifying front at 2km (0.15-0.18) 0.12
threshold = 0.06 # threshold for classifying front (0.06-)
nPeriods = 1:12
weeks <- c(1:52)
pers_thresh <- 0.08  # s
# Discrete palette useful for identifying thresholds
divpal = rev(brewer.pal(10,'RdYlBu'))
# Continuous palette for displaying smoother images
cols = rev(brewer.pal(11,'RdYlBu'))
pal <- colorRampPalette(cols)(30)
# Zissou1 from Wes Anderson palette
wespal <- wes_palette("Zissou1", 30, type = "continuous")
k=1
k=2
k=3
r.grad <- list()
r.xcomp <- list()
r.ycomp <- list()
r.front <- list()
## Pull variables from nc file 
nc <- nc_open(files[k])  # Open files
lat <- ncvar_get(nc, 'latitude')
lon <- ncvar_get(nc, 'longitude')
# for modis
#sst <- ncvar_get(nc, attributes(nc$var)$names[1])
#grad <- ncvar_get(nc, attributes(nc$var)$names[2])
# x <- ncvar_get(nc, attributes(nc$var)$names[3])
# y <- ncvar_get(nc, attributes(nc$var)$names[4])
grad <- ncvar_get(nc, attributes(nc$var)$names[1])
x <- ncvar_get(nc, attributes(nc$var)$names[2])
y <- ncvar_get(nc, attributes(nc$var)$names[3])
nc_close(nc)
rm(nc)
## Quick plots to visualize the data
for(i in 1:52){
  grad.x <- (grad[,,i]) 
  grad.xt <- t(grad.x)
  # create a raster 
  r = raster(grad.xt, xmn = min(lon), xmx = max(lon), 
             ymn = min(lat), ymx = max(lat), 
             crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r = flip(r,2) #
  plot(r, xlim = c(min(lon), max(lon)), ylim = c(min(lat),max(lat)), 
       col = pal, main = paste0('Modis 2021: Week ', i)) 
}
# Coral Reef SST
for(i in 1:52){
  grad.x <- (grad[,,i]) 
  grad.xt <- t(grad.x)
  # create a raster 
  r = raster(grad.xt, xmn = min(lon), xmx = max(lon), 
             ymn = min(lat), ymx = max(lat), 
             crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r = flip(r,2) #
  plot(r, xlim = c(min(lon), max(lon)), ylim = c(min(lat),max(lat)), 
       col = pal, main = paste0('Coral Reef 2021: Week ', i)) 
}