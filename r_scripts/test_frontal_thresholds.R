library(raster)
library(ncdf4)
library(RColorBrewer)
# setwd(here::here('nc_files/dd/'))
# nc2 <- nc_open('DD_20000501_20000531-AT-R2018-NESGRID-GRAD_SST-BOA-STACKED.nc')
setwd(here::here('nc_files/ww/'))
nc3 <- nc_open('W_201036-AT-R2018-NESGRID-GRAD_SST-BOA-STACKED.nc')
#nc3 <- nc_open('W_201011-AT-R2018-NESGRID-GRAD_SST-BOA-STACKED.nc')
#nc3 <- nc_open('W_200118-AT-R2018-NESGRID-GRAD_SST-BOA-STACKED.nc')
# Extract lat, lon, grad, x, y
attributes(nc3$var)
lon <- ncvar_get(nc3, "longitude")
lat <- ncvar_get(nc3, "latitude")
grad <- ncvar_get(nc3, attributes(nc3$var)$names[2])
x <- ncvar_get(nc3, attributes(nc3$var)$names[3])
y <- ncvar_get(nc3, attributes(nc3$var)$names[4])
#nc_close(nc2)
nc_close(nc3)
par(mfrow=c(1,1))
image(grad[,,5]) 
# Create frontal array (via setting setting all values < threshold to 0)
threshold = c(0.05, 0.08, 0.1, 0.15, 0.18, 0.2, 0.3, 0.4) # threshold for classifying front
front.05 <- grad
front.08 <- grad
front.1 <- grad
front.15 <- grad
front.18 <- grad ## trying this one below
front.2 <- grad
front.3 <- grad
front.4 <- grad
# set thresholds to compare
front.05[front.05 < threshold[1]] <- 0 # set all values < thresh == 0 
front.08[front.08 < threshold[2]] <- 0 # set all values < thresh == 0 
front.1[front.1 < threshold[3]] <- 0 # set all values < thresh == 0 
front.15[front.15 < threshold[4]] <- 0 # set all values < thresh == 0 
front.18[front.18 < threshold[5]] <- 0 # set all values < thresh == 0 
front.2[front.2 < threshold[6]] <- 0 # set all values < thresh == 0 
front.3[front.3 < threshold[7]] <- 0 # set all values < thresh == 0 
front.4[front.4 < threshold[8]] <- 0 # set all values < thresh == 0 
# select day to compare
g.d5 <- (grad[,,5]) # day 5 for week 36 2010, 6 for week 11 2010, day 3 for may 2001
f.05d5 <- (front.05[,,5])
f.08d5 <- (front.08[,,5])
f.1d5 <- (front.1[,,5])
f.15d5 <- (front.15[,,5])
f.18d5 <- (front.18[,,5])
f.2d5 <- (front.2[,,5])
f.3d5 <- (front.3[,,5])
f.4d5 <- (front.4[,,5])

# rasterize for plotting difference 
# transpose, rasterize, flip, plot
g.d5.t <- t(g.d5)
g.d5.r = raster(g.d5.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
g.d5.f = raster::flip(g.d5.r,2)  
f.05d5.t <- t(f.05d5)
f.05d5.r = raster(f.05d5.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
f.05d5.f = raster::flip(f.05d5.r,2)  
plot(f.05d5.f)
f.08d5.t <- t(f.08d5)
f.08d5.r = raster(f.08d5.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
f.08d5.f = raster::flip(f.08d5.r,2)  
plot(f.08d5.f)

f.1d5.t <- t(f.1d5)
f.1d5.r = raster(f.1d5.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
f.1d5.f = raster::flip(f.1d5.r,2)  
f.15d5.t <- t(f.15d5)
f.15d5.r = raster(f.15d5.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
f.15d5.f = raster::flip(f.15d5.r,2)  
f.18d5.t <- t(f.18d5)
f.18d5.r = raster(f.18d5.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
f.18d5.f = raster::flip(f.18d5.r,2)  

f.2d5.t <- t(f.2d5)
f.2d5.r = raster(f.2d5.t, xmn = min(lon), xmx = max(lon), 
             ymn = min(lat), ymx = max(lat),
             crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
f.2d5.f = raster::flip(f.2d5.r,2)  
f.3d5.t <- t(f.3d5)
f.3d5.r = raster(f.3d5.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
f.3d5.f = raster::flip(f.3d5.r,2)  
f.4d5.t <- t(f.4d5)
f.4d5.r = raster(f.4d5.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
f.4d5.f = raster::flip(f.4d5.r,2)  
# visually compare difference 
display.brewer.pal(10,"RdYlBu")
display.brewer.pal(10,"Set3")
#pal = RColorBrewer::brewer.pal(10,'RdYlBu')
#pal = c('#FFFFFF',RColorBrewer::brewer.pal(10,'Set3'))
pal = c('#191970','#DAA520','#8DD3C7', '#BEBADA',"#FB8072","#80B1D3", 
        "#FDB462", '#FF00FF', '#964B00','#545454')
par(mfrow=c(2,2))
#plot(g.d5.f, col = rev(pal), main = 'Grad Mag')
plot(g.d5.f, col = pal, main = 'Grad Mag')
plot(f.05d5.f, col = pal, main = 'Front threshold = 0.05')
plot(f.08d5.f, col = pal, main = 'Front threshold = 0.08')
plot(f.1d5.f, col = pal, main = 'Front threshold = 0.1')

plot(f.15d5.f, col = pal, main = 'Front threshold = 0.15')
plot(f.18d5.f, col = pal, main = 'Front threshold = 0.18')


#plot(g.d5.f, col = pal, main = 'Grad Mag')
plot(f.2d5.f, col = pal, main = 'Front threshold = 0.2')
plot(f.3d5.f, col = pal, main = 'Front threshold = 0.3')
plot(f.4d5.f, col = pal, main = 'Front threshold = 0.4')



### Testing sd thresholds 11/23/22
# threshold = 1.5
front = grad
front[front < threshold[6]] <- 0 
sims = grad
# Front gradient density: gdens - smoother to mean gradient mag values 
# fmean = sum of gradient values/total number of frontal pixels 
# fvalid
fvalid <- apply(front, c(1,2), function(x) sum((x > 0)))
#range(!is.na(fvalid))
m_sum <- apply(front, c(1,2), function(x) sum(na.omit(x))) # front[,,5]
range(m_sum)
fclear_m <- apply(sims, c(1,2),  function(x) sum(na.omit(x > 0)))
range(fclear_m)
# x
x_based_on_mag <- ifelse(front[,,5] > 0.0, x[,,5], NA) # selects values of x only if pixel indicated as frontal from grad_mag
x_sum <- apply(x_based_on_mag, c(1,2),  function(x) sum(na.omit(x)))
x_sumsqr <- apply(x_based_on_mag, c(1,2), function(x) {
  sum((na.omit(x) - mean(na.omit(x)))^2) })
# x_var <- apply(x_based_on_mag, c(1,2), function(x) {
#    sum((x - mean(x))^2)/(length(x)-1)})

# y
y_based_on_mag <- ifelse(front[,,5] > 0.0, y[,,5], NA)
y_sum <- apply(y_based_on_mag, c(1,2),  function(x) sum(na.omit(x))) # added na.omit to make it only on frontal pixels
y_sumsqr <- apply(y_based_on_mag, c(1,2), function(x) {
  sum((na.omit(x) - mean(na.omit(x)))^2) })

# -- COMPUTE FRONTAL METRICS -- #
# fmean = sum of gradient values/total number of frontal pixels 
fmean = (sqrt((x_sum/fvalid)^2 + (y_sum/fvalid)^2))
# fvar = variance of gradient values
fvar = (x_sumsqr + y_sumsqr)/fvalid
# fsd =  sqrt of variance of gradient values
fsd = sqrt(fvar)
# get the range of sd
range(na.omit(fsd))


# fcv = fsd/fmean
fcv = fsd/fmean
# fprob = front pixels/clear pixels 
fprob = fvalid/fclear_m
# fintst = frontal intensity, fmean * fprob
fintst = fmean * fprob
# fpers = based on  threshold -> if fsd meets threshold for that week, assign 1, else 0
pers_thresh <- 0.08  # some threshold range (0 - 1) indicating low sd, thus persistence * value will depend on actual data
# generates binary array to calc cumulative persistence across time (weeks, months, etc)
fpers = ifelse(fsd <= pers_thresh, 1, 0)

# Visualize
divpal = c('#FFFFFF',rev(brewer.pal(10,'RdYlBu')))
divpal = rev(brewer.pal(10,'RdYlBu'))

fmean <- t(fmean)
fmean = raster(fmean, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
fmean = flip(fmean,2)  
plot(fmean, col = divpal)

# fprob = front pixels/clear pixels 
fprob <- t(fprob)

fprob = raster(fprob, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

fprob = raster::flip(fprob,2)  
plot(fprob, col = divpal)

# fsd =  
fsd <- t(fsd)

fsd = raster(fsd, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

fsd = raster::flip(fsd,2)  
plot(fsd, col = divpal)

# fpers =  

fpers <- t(fpers)

fpers = raster(fpers, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

fpers = raster::flip(fpers,2)  
plot(fpers, col = divpal)
























# Front gradient density: gdens - smoother to mean gradient mag values 
# fmean = sum of gradient values/total number of frontal pixels 
# fvalid
fvalid <- apply(front[,,5], c(1,2), function(x) sum((x > 0)))
# x
x_based_on_mag <- ifelse(front[,,5] > 0.0, x[,,5], NA) # selects values of x only if pixel indicated as frontal from grad_mag
x_sum <- apply(x_based_on_mag, c(1,2),  function(x) sum(na.omit(x)))
# x_var <- apply(x_based_on_mag, c(1,2), function(x) {
#    sum((x - mean(x))^2)/(length(x)-1)})

# y
y_based_on_mag <- ifelse(front[,,5] > 0.0, y[,,5], NA)
y_sum <- apply(y_based_on_mag, c(1,2),  function(x) sum(na.omit(x))) # added na.omit to make it only on frontal pixels


fmean <- (sqrt((x_sum/fvalid)^2 + (y_sum/fvalid)^2))
fmean <- t(fmean)

fmean = raster(fmean, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
fmean = flip(fmean,2)  
plot(fmean)
# create a weight matrix
W <- raster::focalWeight(fmean,c(1,0.04),type='Gauss')
image(W)
W.0 <- raster::focalWeight(fmean,2,type='Gauss')
image(fmean)
image(W.0)
gdens <- rasterLocalMoments(fmean,W)
gdens.0 <- rasterLocalMoments(fmean,W.0)
plot(fmean)
plot(gdens$mu)
plot(gdens.0$mu)

# Front persistence: pfront <- fraction of cloud-free observations of a pixel 
# for which a front is detected w/a gaussian filter - ie: a local neighbourhood  average of frontal persistence 

fclear_m <- apply(sims[,,5], c(1,2),  function(x) sum(na.omit(x > 0)))

# fprob = front pixels/clear pixels 
fprob = fvalid/fclear_m

fprob <- t(fprob)

fprob = raster(fprob, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

fprob = raster::flip(fprob,2)  
plot(fprob)
# create a weight matrix
W <- raster::focalWeight(fprob,c(1,0.04),type='Gauss')
pfront <- rasterLocalMoments(fprob,W)
plot(fprob)
plot(pfront$mu)

