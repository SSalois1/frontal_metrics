library(raster)
library(RColorBrewer)
library(ncdf4)
### --- SST AVHRR 4 KM --- ### 
#setwd(here::here())
path = ('/Satdata_Primary/nadata/DATASETS/AVHRR/NESGRID/NETCDF/GRAD_SST-BOA')
files <- list.files(path = path, pattern = glob2rx('DD_1998*.nc'), full.names = TRUE)
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
grad <- ncvar_get(nc, attributes(nc$var)$names[4])
x <- ncvar_get(nc, attributes(nc$var)$names[5])
y <- ncvar_get(nc, attributes(nc$var)$names[6])
nc_close(nc)
rm(nc)

for (i in months[(1:12)]){
  tmp.g <- (grad[,,i]) 
  tmp.gt <- t(tmp.g) 
  r.g = raster(tmp.gt, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r.g = raster::flip(r.g,2)  
  plot(r.g, col = divpal)
  assign(paste('r.g.',i, sep = ''), r.g)
  r.grad[[i]] <- get(paste0('r.g.',i))
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
grpsize <- 3 # desired layers per stack (here, I am using 3 months per season)
# use names to group - here, names(x) becomes c('1','1','1','2','2','2')
# length.out will help if you have, say an extra 3 months of data in the original stack
names(x) <- rep(1:(ceiling(length(x)/grpsize)), 
                each = grpsize, length.out = length(x))
# stack gradient magnitude 
names(r.grad) <- c('winter', 'winter', 'spring','spring','spring', 'summer',
                   'summer','summer', 'fall', 'fall', 'fall', 'winter')
# make list of rasters into a list of stacks
# this gives me a list of 2 stacks with three rasters each 
grad.stacks <- lapply(unique(names(r.grad)), function(y) {
  b <- r.grad[names(r.grad) == y]
  stack(b)
})

# stack x component
names(r.xcomp) <- c('winter', 'winter', 'spring','spring','spring', 'summer',
                    'summer','summer', 'fall', 'fall', 'fall', 'winter')
# make list of rasters into a list of stacks
# this gives me a list of 2 stacks with three rasters each 
x.stacks <- lapply(unique(names(r.xcomp)), function(y) {
  b <- r.xcomp[names(r.xcomp) == y]
  stack(b)
})

# stack y component
names(r.ycomp) <- c('winter', 'winter', 'spring','spring','spring', 'summer',
                    'summer','summer', 'fall', 'fall', 'fall', 'winter')
# make list of rasters into a list of stacks
# this gives me a list of 2 stacks with three rasters each 
y.stacks <- lapply(unique(names(r.ycomp)), function(y) {
  b <- r.ycomp[names(r.ycomp) == y]
  stack(b)
})

# stack fronts 
names(r.front) <- c('winter', 'winter', 'spring','spring','spring', 'summer',
                    'summer','summer', 'fall', 'fall', 'fall', 'winter')
# make list of rasters into a list of stacks
# this gives me a list of 2 stacks with three rasters each 
front.stacks <- lapply(unique(names(r.front)), function(y) {
  b <- r.front[names(r.front) == y]
  stack(b)
})

# Isolate the season winter (Dec:Feb, 50:9) Months: 12,1,2
sgspr <- grad.stacks[[2]]
sfspr <- front.stacks[[2]]
sxspr <- x.stacks[[2]]
syspr <- y.stacks[[2]]

# Functions
fx.sum.greater.than.zero <- function(x) sum(na.omit(x > 0))
fx.sum <- function(x) sum(na.omit(x))
fx.sumsqrs <- function(x) {sum((na.omit(x) - mean(na.omit(x)))^2)}

# Metrics
## Initialize these
r1 = sxspr # stack of x
r3 = sfspr # stack of fronts
r3[r3 <= 0] <- NA
plot(r3)
r4 <- mask(r1, r3)
plot(r4)
r5 = syspr # stack of y
r6 <- mask(r5, r3)
plot(r6)
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
plot(fclear_m, main = 'Fclear', col= divpal)
plot(fvalid, main = 'fvalid', col= divpal)
par(mfrow=c(1,3))
plot(sgspr[[1]], main = 'grad_mag month 1', col= divpal)
plot(sgspr[[2]], main = 'grad_mag month 2', col= divpal)
plot(sgspr[[3]], main = 'grad_mag month 3', col= divpal)

range(na.omit(sgspr[[1]]@data@values))
range(na.omit(sgspr[[2]]@data@values))
range(na.omit(sgspr[[3]]@data@values))

pdf(file = 'metrics_spring_1990_4km_thresh12.pdf', width = 10, height = 6)
par(mfrow = c(2,4), oma = c(1,1,1,1))
plot(fvalid, main = 'fvalid', col= divpal)
plot(fclear_m, main = 'Fclear', col= divpal)
plot(fmean.90, main = 'Frontal Mean', col= divpal)
plot(fprob.90, main = 'Frontal Probability', col= divpal)
plot(fvar.90, main = 'Frontal Variance', col= divpal)
plot(fsd.90, main = 'Frontal Standard Deviation', col= divpal)
plot(fcv.90, main = 'Frontal CV', col= divpal)
plot(fintst.90, main = 'Frontal Intensity', col= divpal)
plot(fpers.90, main = 'Frontal Persistence', col= divpal)
plot(fpersprob.90, main = 'Persistence Probability', col = divpal)
dev.off()


# saving stacks 
here::here('/Satdata_Primary/nadata/PROJECTS/ILLEX_INDICATORS/R_SCRIPTS/GRADSST_INDICATORS/RASTERS/SEASONAL_AVHRR_4KM/')
setwd('/Satdata_Primary/nadata/PROJECTS/ILLEX_INDICATORS/R_SCRIPTS/GRADSST_INDICATORS/RASTERS/SEASONAL_AVHRR_4KM/')

i=1
writeRaster(fmean.90, filename = paste0('mm_fmean_spring_', substr(files[i],start = 73, stop = 74), '.tif'), format = "GTiff",
            options = "INTERLEAVE=BAND", bylayer = F, 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
            overwrite = TRUE) # bylayer = T if want individual layers to save
writeRaster(fvar.90, filename = paste0('mm_fvar_spring_', substr(files[i],start = 73, stop = 74), '.tif'), format = "GTiff",
            options = "INTERLEAVE=BAND", bylayer = F, 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
            overwrite = TRUE) # bylayer = T if want individual layers to save
writeRaster(fsd.90, filename = paste0('mm_fsd_spring_', substr(files[i],start = 73, stop = 74), '.tif'), format = "GTiff",
            options = "INTERLEAVE=BAND", bylayer = F, 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
            overwrite = TRUE) # bylayer = T if want individual layers to save
vwriteRaster(fcv.90, filename = paste0('mm_fcv_spring_', substr(files[i],start = 73, stop = 74), '.tif'), format = "GTiff",
             options = "INTERLEAVE=BAND", bylayer = F, 
             crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
             overwrite = TRUE) # bylayer = T if want individual layers to save
writeRaster(fprob.90, filename = paste0('mm_fprob_spring_', substr(files[i],start = 73, stop = 74), '.tif'), format = "GTiff",
            options = "INTERLEAVE=BAND", bylayer = F, 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
            overwrite = TRUE) # bylayer = T if want individual layers to save
writeRaster(fintst.90, filename = paste0('mm_fintst_spring_', substr(files[i],start = 73, stop = 74), '.tif'), format = "GTiff",
            options = "INTERLEAVE=BAND", bylayer = F, 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
            overwrite = TRUE) # bylayer = T if want individual layers to save
writeRaster(fpers.90, filename = paste0('mm_fpers_spring_', substr(files[i],start = 73, stop = 74), '.tif'), format = "GTiff",
            options = "INTERLEAVE=BAND", bylayer = F, 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
            overwrite = TRUE) # bylayer = T if want individual layers to save
writeRaster(fpersprob.90, filename = paste0('mm_fpersprob_spring_', substr(files[i],start = 73, stop = 74), '.tif'), format = "GTiff",
            options = "INTERLEAVE=BAND", bylayer = F, 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
            overwrite = TRUE) # bylayer = T if want individual layers to save

