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
files <- list.files(path = path, pattern = glob2rx('W_202120*.nc'), 
                    full.names = TRUE) # choosing these files bc they have x,y
# files <- list.files(path = path, pattern = glob2rx('WW_2020*.nc'), 
#                     full.names = TRUE) # the WW files for modis are the metrics does not have x and y components
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

## Modis
r.grad.m <- list()
r.xcomp.m <- list()
r.ycomp.m <- list()
r.front.m <- list()
for (i in 1:52){
  tmp.g <- (grad.m[,,i]) 
  tmp.gt <- t(tmp.g) 
  r.g = raster(tmp.gt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.g = raster::flip(r.g,2)  
  plot(r.g, col = pal)
  assign(paste('r.g.',i, sep = ''), r.g)
  r.grad.m[[i]] <- get(paste0('r.g.',i))
  tmp.x <- (x.m[,,i]) 
  tmp.xt <- t(tmp.x)
  r.x = raster(tmp.xt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.x = raster::flip(r.x,2)  
  assign(paste('r.x.',i, sep = ''), r.x)
  r.xcomp.m[[i]] <- get(paste0('r.x.',i))
  tmp.y <- (y.m[,,i]) 
  tmp.yt <- t(tmp.y)
  r.y = raster(tmp.yt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.y = raster::flip(r.y,2)  
  assign(paste('r.y.',i, sep = ''), r.y)
  r.ycomp.m[[i]] <- get(paste0('r.y.',i))
  # Change this to reflect new front detection method
  tmp.f <- (grad.m[,,i])
  tmp.f[tmp.f < threshold] <- 0
  tmp.ft <- t(tmp.f)
  r.f = raster(tmp.ft, xmn = min(lon), xmx = max(lon),
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.f = raster::flip(r.f,2)
  assign(paste('r.f.',i, sep = ''), r.f)
  r.front.m[[i]] <- get(paste0('r.f.',i))
}
# substitute a single layer (week) to test metric calcs
sfspr <- r.f.21
sxspr <- r.x.21
syspr <- r.y.21
# Metrics
## Initialize these
r1 = sxspr # stack of x
r3 = sfspr # stack of fronts
r3[r3 <= 0] <- NA # do I need to do this? 
plot(r.g.21, col = pal, main = 'gradient magnitude')
plot(r.f.21, col = pal, main = 'fronts (no NA)') # plot the fronts 
plot(r3, col = pal,  main = 'fronts NA')
r4 <- raster::mask(r1, r3)
plot(r4, col = pal, main = 'x based on grad mag')
r5 = syspr # stack of y
r6 <- raster::mask(r5, r3)
plot(r6, col = pal, main = 'y based on grad mag')
## Base params
fvalid <- calc(sfspr, fx.sum.greater.than.zero)
m_sum <- calc(sfspr, fx.sum)
fclear_m <- calc(sgspr, fx.sum.greater.than.zero)
# x_var
x_based_on_mag <- r4
x_sum <- calc(x_based_on_mag, fx.sum)
x_sumsqr <- calc(x_based_on_mag, fx.sumsqrs)
# y_var
y_based_on_mag <- r6
y_sum <- calc(y_based_on_mag, fx.sum)
y_sumsqr <- calc(y_based_on_mag, fx.sumsqrs)
# Metrics
fmean.90 = (sqrt((x_sum/fvalid)^2 + (y_sum/fvalid)^2))
fvar.90 = (x_sumsqr + y_sumsqr)/fvalid
fsd.90 = sqrt(fvar.90)
fcv.90 = fsd.90/fmean.90
fprob.90 = fvalid/fclear_m
fintst.90 = fmean.90 * fprob.90
fpers.90 <- overlay(fsd.90, fun=rc)
fpersprob.90 = fpers.90 * fprob.90

# just checking ...
plot(fclear_m, main = 'Fclear', col= pal)
plot(fvalid, main = 'fvalid', col= pal)
par(mfrow=c(1,3))
plot(sgspr[[1]], main = 'grad_mag month 1', col= pal)
plot(sgspr[[2]], main = 'grad_mag month 2', col= pal)
plot(sgspr[[3]], main = 'grad_mag month 3', col= pal)

range(na.omit(sgspr[[1]]@data@values))
range(na.omit(sgspr[[2]]@data@values))
range(na.omit(sgspr[[3]]@data@values))

pdf(file = 'metrics_spring_1990_4km_thresh12.pdf', width = 10, height = 6)
par(mfrow = c(2,4), oma = c(1,1,1,1))
plot(fvalid, main = 'fvalid', col= pal)
plot(fclear_m, main = 'Fclear', col= pal)
plot(fmean.90, main = 'Frontal Mean', col= pal)
plot(fprob.90, main = 'Frontal Probability', col= pal)
plot(fvar.90, main = 'Frontal Variance', col= pal)
plot(fsd.90, main = 'Frontal Standard Deviation', col= pal)
plot(fcv.90, main = 'Frontal CV', col= pal)
plot(fintst.90, main = 'Frontal Intensity', col= pal)
plot(fpers.90, main = 'Frontal Persistence', col= pal)
plot(fpersprob.90, main = 'Persistence Probability', col = pal)
dev.off()


## MUR
r.grad.mur <- list()
r.xcomp.mur <- list()
r.ycomp.mur <- list()
r.front.mur <- list()
for (i in 1:52){
  tmp.g <- (grad.mur[,,i]) 
  tmp.gt <- t(tmp.g) 
  r.g = raster(tmp.gt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.g = raster::flip(r.g,2)  
  plot(r.g, col = pal)
  assign(paste('r.g.',i, sep = ''), r.g)
  r.grad.mur[[i]] <- get(paste0('r.g.',i))
  tmp.x <- (x.mur[,,i]) 
  tmp.xt <- t(tmp.x)
  r.x = raster(tmp.xt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.x = raster::flip(r.x,2)  
  assign(paste('r.x.',i, sep = ''), r.x)
  r.xcomp.mur[[i]] <- get(paste0('r.x.',i))
  tmp.y <- (y.mur[,,i]) 
  tmp.yt <- t(tmp.y)
  r.y = raster(tmp.yt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.y = raster::flip(r.y,2)  
  assign(paste('r.y.',i, sep = ''), r.y)
  r.ycomp.mur[[i]] <- get(paste0('r.y.',i))
  ## Change this to reflect new front detection method
  tmp.f <- (grad.mur[,,i])
  tmp.f[tmp.f < threshold] <- 0
  tmp.ft <- t(tmp.f)
  r.f = raster(tmp.ft, xmn = min(lon), xmx = max(lon),
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.f = raster::flip(r.f,2)
  assign(paste('r.f.',i, sep = ''), r.f)
  r.front.mur[[i]] <- get(paste0('r.f.',i))
}

