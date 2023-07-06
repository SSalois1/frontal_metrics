library(raster)
library(RColorBrewer)
library(ncdf4)
### --- SST AVHRR 4 KM --- ### 
#setwd(here::here())
path = ('/Satdata_Primary/nadata/DATASETS/AVHRR/NESGRID/NETCDF/GRAD_SST-BOA')
files <- list.files(path = path, pattern = glob2rx('DD8_1993*.nc'), full.names = TRUE)
files <- list.files(path = path, pattern = glob2rx('DD8_1993*.nc'), full.names = TRUE)

threshold = 0.18 # threshold for classifying front at 2km (0.15-0.18) 0.12
threshold = 0.12 # threshold for classifying front at 2km (0.15-0.18) 0.12
threshold = 0.06 # threshold for classifying front (0.06-)
nPeriods = 1:12
months <- c(1:12)
pers_thresh <- 0.08  # s
divpal = rev(brewer.pal(10,'RdYlBu'))
k=1
r.grad <- list()
r.xcomp <- list()
r.ycomp <- list()
r.front <- list()
## Pull variables from nc file 
nc <- nc_open(files[k])  # Open files
lat <- ncvar_get(nc, 'latitude')
lon <- ncvar_get(nc, 'longitude')
grad <- ncvar_get(nc, attributes(nc$var)$names[1])
x <- ncvar_get(nc, attributes(nc$var)$names[2]) # these files dont have y
y <- ncvar_get(nc, attributes(nc$var)$names[3])
nc_close(nc)
rm(nc)

for(i in 1:365){
  grad.x <- (grad[,,i]) 
  grad.xt <- t(grad.x)
  # create a raster 
  r = raster(grad.xt, xmn = min(lon), xmx = max(lon), 
             ymn = min(lat), ymx = max(lat), 
             crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r = flip(r,2) #
  plot(r, col = divpal, main = paste0('1993:Day',i)) 
}
for (i in days[(60:151)]){
  tmp.g <- (grad[,,i]) 
  tmp.gt <- t(tmp.g) 
  r.g = raster(tmp.gt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.g = raster::flip(r.g,2)  
  plot(r.g, col = divpal, main = paste('grad_mag',i))
  assign(paste('r.g.',i, sep = ''), r.g)
  r.grad[[j]] <- get(paste0('r.g.',j))
  # r.grad = stack(r.grad)}
  tmp.x <- (x[,,i]) 
  tmp.xt <- t(tmp.x)
  r.x = raster(tmp.xt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.x = raster::flip(r.x,2)  
  assign(paste('r.x.',i, sep = ''), r.x)
  r.xcomp[[i]] <- get(paste0('r.x.',i))
  tmp.y <- (y[,,i]) 
  tmp.yt <- t(tmp.y)
  r.y = raster(tmp.yt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.y = raster::flip(r.y,2)  
  assign(paste('r.y.',i, sep = ''), r.y)
  r.ycomp[[i]] <- get(paste0('r.y.',i))
  tmp.f <- (grad[,,i]) 
  tmp.f[tmp.f < threshold] <- 0 
  tmp.ft <- t(tmp.f)
  r.f = raster(tmp.ft, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.f = raster::flip(r.f,2)  
  assign(paste('r.f.',i, sep = ''), r.f)
  r.front[[i]] <- get(paste0('r.f.',i))
}


# Get to stacking! 
grpsize <- 90 # desired layers per stack (here, I am using 3 months per season)
# use names to group - here, names(x) becomes c('1','1','1','2','2','2')
# length.out will help if you have, say an extra 3 months of data in the original stack
days <- c(1:365)
names(x) <- rep(1:(ceiling(length(days)/grpsize)), 
                each = grpsize, length.out = length(days))


grad.stacks <- lapply(unique(names(r.grad)), function(y) {
  b <- r.grad[names(r.grad) == y]
  stack(b)
})

# stack gradient magnitude 
names(r.grad) <- c(1:90)
# make list of rasters into a list of stacks
# this gives me a list of 2 stacks with three rasters each 
grad.stacks <- lapply(unique(names(r.grad)), function(y) {
  b <- r.grad[names(r.grad) == y]
  stack(b)
})

for (i in days[(61:151)]){
  rl <- as.list(get(paste0('r.g.',i)))
}
grad.stacks <-stack(rl)

# attempt 2:
grad.stacks <- stack(r.g.60, r.g.61, r.g.62, 
                     r.g.63, r.g.64, r.g.65,
                     r.g.66, r.g.67, r.g.68, 
                     r.g.69, r.g.70, r.g.71)

for (i in days[(61:151)]){
  grad.stacks[[i+1]] <- stack(get(paste0('r.g.',i)))
}
# attempt 3:
grad.stacks <- stack(r.g.60)
for (i in days[(61:151)]){
  grad.stacks[[i]] <- get(paste0('r.g.',i))
}

# attempt 4:
daynames = days[(61:151)]

for (i in 1:length(daynames)){
  names(r.grad) <- c(1:91)
}
# make list of rasters into a list of stacks
# this gives me a list of 2 stacks with three rasters each 
grad.stacks <- lapply(unique(names(r.grad)), function(y) {
  b <- r.grad[names(r.grad) == y]
  stack(b)
})







