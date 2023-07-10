# Continuous palette for displaying smoother images
cols = rev(brewer.pal(11,'RdYlBu'))
pal <- colorRampPalette(cols)(30)


thresh.mean <- r.f.og >= tmp.foc.mean
fronts.mean <- mask(r.f.og,thresh.mean/thresh.mean)
thresh.median <- r.f.og >= tmp.foc.median
fronts.median <- mask(r.f.og,thresh.median/thresh.median)
thresh.max <- r.f.og >= tmp.foc.max
thresh.max2 <- r.f.og > tmp.foc.max
fronts.max <- mask(r.f.og,tmp.foc.max/tmp.foc.max)

thresh = tmp.foc.max/tmp.foc.max


par(mfrow = c(2,2))
plot(r.f.og, main = 'grad mad', col = pal)
plot(fronts.mean, main = 'fronts (grad mad >= mean', col = pal)
plot(fronts.median, main = 'fronts (grad mad >= median', col = pal)
plot(fronts.max, main = 'fronts (grad mad >= max', col = pal)
plot(thresh.median, col = pal)



par(mfrow=c(1,3))
#plot(r.f.og,main = 'gm', col = pal )
plot(tmp.foc.mean,main = 'mean filter', col = pal )
#plot((r.f.og -tmp.foc.mean),main = 'grad-filter', col = pal )
plot(tmp.foc.median,main = 'median filter', col = pal )
plot(tmp.foc.max,main = 'max filter', col = pal )


plot(tmp.foc.mean -r.f.og, col = pal)
plot(r.f.og - tmp.foc.median, col = pal)
thresh.median <- r.f.og >= tmp.foc.median
plot(thresh.median, col = pal)




### ----- Testing with simulated matrix ----- ### 
m = c(0.0570000,NA,0.500000,0.418000,
      0.418000,NA,0.430000,0.430000,
      0.0570000,NA,	0.0570000,0.0530000,
      0.0530000,0.500000, 0.706000,	0.530000,
      0.0570000,NA, 0.500000,	0.706000,
      0.530000,0.418000, 0.418000,	NA,
      0.430000,0.430000,0.0570000, NA,
      0.0570000,0.0530000,0.0530000,	0.500000,
      0.0570000,NA, 0.500000,	0.0530000,
      0.500000, 0.706000,	0.530000, 0.418000,
      0.418000, NA, 0.430000, 0.430000,
      0.0570000,NA,	0.0570000,0.0530000,
      0.0570000, NA, 0.500000,	0.0570000,
      0.0530000,0.0530000, 0.500000, 0.706000,
      0.530000, 0.418000, 0.418000,NA,
      0.430000, 0.430000,	0.0570000, NA,
      0.0570000,NA, 0.500000,0.0570000,
      NA,	0.0570000,0.0530000,0.0530000,
      0.500000, 0.706000,	0.530000,0.418000,
      0.418000, NA, 0.430000, 0.430000,
      0.0570000, NA, 0.500000, 0.430000,
      0.430000,	0.0570000,NA,	0.0570000,
      0.0530000,0.0530000, 0.500000,	0.706000,
      0.530000, 0.418000, 0.418000,	NA,
      0.0570000, NA,0.500000, 0.418000,
      NA,	0.430000,	0.430000,	0.0570000,
      NA,	0.0570000,0.0530000,0.0530000,
      0.500000, 0.706000, 0.530000,0.418000)
# getting rid of NAs so that max function plays nicely 
sim.max <- m
sim.max[is.na(sim.max)] <- 0
library(sf)
raster_grid <- raster(xmn = min(lon),
                      xmx = max(lon), 
                      ymn = min(lat),
                      ymx = max(lat),
                      res = 0.33333)
r <- raster(ncols=15, nrows=15)
x <- runif(m) * 360 - 180
y <- runif(m) * 180 - 90
xy <- cbind(x, y)
#coordinates(xy) <- c('lon', 'lat')
# get the (last) indices
r0 <- rasterize(xy, r)
plot(r0)
# presence/absensce (NA) (is there a point or not?)
r1 <- rasterize(xy, r, field=1)
plot(r1)
par(mfrow = c(1,2)); plot(r0); plot(r1)
# how many points?
r2 <- rasterize(xy, r, fun=function(x,...)length(x))
plot(r2)
### This is the old one I did 6/7/23
sim.foc.mean <- raster::focal(r0, fun = mean, 
                              na.rm = TRUE, 
                              w = matrix(1,3,3))

sim.foc.median <- raster::focal(r0, fun = median, 
                                na.rm = TRUE, 
                                w = matrix(1,3,3))
sim.foc.max <- raster::focal(r0, fun = na.omit(max), 
                             na.rm = TRUE, 
                             w = matrix(1,3,3))

par(mfrow=c(2,2))
plot(r0, main = 'gm', col = pal)
plot(sim.foc.mean,main = 'mean filter', col = pal)
plot(sim.foc.median,main = 'median filter', col = pal)
plot(sim.foc.max,main = 'max filter', col = pal)

### make as thresh
thresh.mean <- r0 >= sim.foc.mean
fronts.mean <- mask(r0,thresh.mean/thresh.mean)
thresh.median <- r0 >= sim.foc.median
fronts.median <- mask(r0,thresh.median/thresh.median)
thresh.max <- r0 >= sim.foc.max
fronts.max <- mask(r0,thresh.max/thresh.max)

thresh = tmp.foc.max/tmp.foc.max


par(mfrow = c(2,2))
plot(r0, main = 'grad mad', col = pal)
plot(fronts.mean, main = 'fronts (grad mad >= mean', col = pal)
plot(fronts.median, main = 'fronts (grad mad >= median', col = pal)
plot(fronts.max, main = 'fronts (grad mad >= max', col = pal)



r0@data@values

fronts.mean@data@values
fronts.median@data@values
fronts.max@data@values


## 07/01: This one uses g so there are no NAs
r <- raster(ncols=5, nrows=5)
res(r)
ncell(r)
g = c(0.0570000,1.0,0.500000,0.418000,
      0.418000,2.0,0.430000,0.430000,
      0.0570000,0.5,	0.0570000,0.0530000,
      0.0530000,0.500000, 0.706000,	0.530000,
      0.0570000,0.9, 0.500000,	2.5,
      0.530000,1.918000, 2.99,3.0,
      2.8)

values(r) <- g
hasValues(r)
ncell(r)
res(r)
# res(r) <- 100 #c(72, 36) how to change resolution 
values(r)[1:10]
plot(r, main='Raster with 25 cells', col = pal)


m <- raster::focal(r, fun = na.omit(max), 
                             w = matrix(1,3,3))
ncell(m)
m.med <- raster::focal(r, fun = median, 
                       na.rm = TRUE,
                       w = matrix(1,3,3))
ncell(m.med)
m.mean <- raster::focal(r, fun = mean, 
                              na.rm = TRUE, 
                              w = matrix(1,3,3))

plot(m)
res(m)
values(r)
values(m)
par(mfrow = c(4,1)); 
plot(r, col = pal, main = 'g')
plot(m, col = pal, main = 'max')
plot(m.med, col = pal, main = 'median')
plot(m.mean, col = pal, main = 'mean')

# maksing example
m[m < 0.5] <- NA
mr <- mask(r, m)
# Ok now to mask r(which is representing grad_mag) by the threshold(max focal)
m[m < 0.5] <- NA
mr <- mask(r, m)
plot(mr)
values(mr)
values(r)
par(mfrow = c(2,1)); plot(r, col = pal);plot(mr, col = pal)

## Trying with bigger values and bigger grid
r <- raster(ncols=10, nrows=10)
set.seed(42)
values(r) <- runif(ncell(r)) * 10
plot(r, main='Raster with 100 cells', col = pal)

m <- raster::focal(r, fun = max, 
                   na.rm = TRUE,
                   w = matrix(1,3,3))
values(r)
values(m)

par(mfrow = c(2,1)); 
plot(r, col = pal, main = 'g')
plot(m, col = pal, main = 'max')
# now do the mask 
mr <- mask(r, m)
par(mfrow = c(2,1)); plot(r, col = pal);plot(mr, col = pal)
values(mr)

### Ok try this for local thresholding mean of the min and max values

# T = (max + min/2)
thresh <- raster::focal(r, fun = function (x){(max(x)+min(x))/2}, 
                   w = matrix(1,3,3))
matrix(1/9, nc=3, nr=3)
thresh.mean <- raster::focal(r, fun = mean, 
                   na.rm = TRUE,
                   w = matrix(1,3,3))
values(thresh)
values(thresh.mean)
values(r)
r2 <- r
r3 <- r
r2[r2< thresh] <- NA
r3[r3< thresh.mean] <- NA
values(r2)
values(r3)
values(r)
par(mfrow = c(3,1))
plot(r, col = pal, main = 'grad_mag')
plot(r2, col = pal, main = 'fronts')
plot(r3, col = pal, main = 'mean fronts')
## HOORAY THIS WORKS!! We need a plan for edge effects - maybe do a circle or 
## Other distribution

# save to share with Kim 
df <- r@data@values

write.csv(df, 'simulated_grad_mag.csv')

matrix = nrow
   c(1,2,3)
m = matrix(1/9, nc=3, nr=3)
m[1,] <- c(1,2,1)
m[2,] <- c(2,4,2)
m[3,] <- c(1,2,1)

# need for mask? r3 = r0/r0 not in these examples

gauss.kern <- m

grad.file <- read.csv(here::here('csv_files/grad_sst_test_data.csv'), header = FALSE)
grad.file <- as.matrix(grad.file)
grad <- raster(ncols=100, nrows=100)
values(grad) <- grad.file
grad <- flip(grad,2)
plot(grad, main='Kims grad_sst sim', col = pal)

midrange.fronts <- grad
midrange.fronts.t2 <- grad
gauss.fronts <- grad
## T = (max + min/2)
thresh <- raster::focal(grad, fun = function (x){(max(x)+min(x))/2}, 
                        w = matrix(1,5,5))
plot(thresh, main='Midrange threshold raster', col = pal)
midrange.fronts[midrange.fronts< 0.4] <- NA
midrange.fronts.t2[midrange.fronts.t2< 0.7] <- NA
plot(midrange.fronts, main='Fronts after Midrange threshold applied', 
     col = pal)
plot(midrange.fronts.t2, main='Fronts after Midrange threshold applied', 
     col = pal)
## T = gaussian
thresh.g <- raster::focal(grad, fun = function (x){(max(x)+min(x))/2}, 
                        w = m)
plot(thresh.g, main='Midrange threshold w/gauss', col = pal)
gauss.fronts[gauss.fronts< thresh.g] <- NA
plot(gauss.fronts, main='Fronts after Midrange threshold applied',
     col = pal)

