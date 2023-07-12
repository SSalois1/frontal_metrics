library(ncdf4)
library(raster)
jet.colors <-colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
setwd('~/github/ssalois1/frontal_metrics/nc_files/ww')
nc25 <- nc_open('W_202125-AT-R2018-NESGRID-GRAD_SST-BOA-STACKED.nc')
nc26 <- nc_open('W_202126-AT-R2018-NESGRID-GRAD_SST-BOA-STACKED.nc')
nc0926 <- nc_open('W_201939-AT-R2018-NESGRID-GRAD_SST-BOA-STACKED.nc')
# Extract lat, lon, grad, x, y
lon <- ncvar_get(nc0926, "longitude")
lat <- ncvar_get(nc0926, "latitude", verbose = F)
grad <- ncvar_get(nc0926, attributes(nc0926$var)$names[2])
x <- ncvar_get(nc0926, attributes(nc0926$var)$names[3])
y <- ncvar_get(nc0926, attributes(nc0926$var)$names[4])
# close the netcdf
nc_close(nc0926) 
image(grad[,,6])
# Brick the whole NC for testing 
b <- brick(grad, xmn = min(lon), xmx = max(lon), 
           ymn = min(lat), ymx = max(lat), transpose = T,
           crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
b <- raster::flip(b,2)
bx <- brick(x, xmn = min(lon), xmx = max(lon), 
           ymn = min(lat), ymx = max(lat), transpose = T,
           crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
bx <-  raster::flip(bx,2)
by <- brick(y, xmn = min(lon), xmx = max(lon), 
            ymn = min(lat), ymx = max(lat), transpose = T,
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
by <-  raster::flip(by,2)


divpal = rev(brewer.pal(10,'RdYlBu'))
divpal2 = c('#FFFFFF',rev(brewer.pal(10,'RdYlBu')))
divpal = rev(brewer.pal(10,'RdYlBu'))
# Apply threshold to each layer of brick 
threshold = 0.15
threshold = 0.3
sims = b
front = b
front[front < threshold] <- 0 
# fvalid
f <- function(x) sum(na.omit(x > 0))
fvalid <- calc(front, f)
plot(fvalid, col = divpal2, main = 'FValid')
# range(fvalid@data@values)
# msum
fmsum <- function(x) sum(na.omit(x))
m_sum <- calc(front, fmsum)
range(m_sum@data@values)
# fclear
ffclear <- function(x) sum(na.omit(x > 0))
fclear_m <- calc(sims, ffclear)
range(fclear_m@data@values)
# x
# x_based_on_mag <- ifelse(front > 0.0, bx, NA) # selects values of x only if pixel indicated as frontal from grad_mag
# x_based_on_mag <- bx[[front>0.0]] # selects values of x only if pixel indicated as frontal from grad_mag
# x_sum <- apply(x_based_on_mag, c(1,2),  function(x) sum(na.omit(x)))
# x_sumsqr <- apply(x_based_on_mag, c(1,2), function(x) {
#   sum((na.omit(x) - mean(na.omit(x)))^2) })
# x_var <- apply(x_based_on_mag, c(1,2), function(x) {
#    sum((x - mean(x))^2)/(length(x)-1)})

# step 1: process r2 by replacing any values smaller than or equal to zero as NA 
# step 2: use mask function to extract values in r1 based on r3
r1 = bx
r3 = front
r3[r3 <= 0] <- NA
plot(r3)
r4 <- mask(r1, r3)
# plot(r4)
x_based_on_mag <- r4
fmsum <- function(x) sum(na.omit(x))
x_sum <- calc(x_based_on_mag, fmsum)
range(x_sum@data@values)
sumsqrfunc <- function(x) {sum((na.omit(x) - mean(na.omit(x)))^2)}
x_sumsqr <- calc(x_based_on_mag, sumsqrfunc)
range(x_sumsqr@data@values)

# y
# y_based_on_mag <- ifelse(front[,,5] > 0.0, y[,,5], NA)
# y_sum <- apply(y_based_on_mag, c(1,2),  function(x) sum(na.omit(x))) # added na.omit to make it only on frontal pixels
# y_sumsqr <- apply(y_based_on_mag, c(1,2), function(x) {
#   sum((na.omit(x) - mean(na.omit(x)))^2) })
#### do it w/raster
r5 = by
r6 <- mask(r5, r3)
plot(r6)
y_based_on_mag <- r6
y_sum <- calc(y_based_on_mag, fmsum)
range(y_sum@data@values)
#sumsqrfunc <- function(x) {sum((na.omit(x) - mean(na.omit(x)))^2)}
y_sumsqr <- calc(y_based_on_mag, sumsqrfunc)
range(y_sumsqr@data@values)

# -- COMPUTE FRONTAL METRICS -- #
# fmean = sum of gradient values/total number of frontal pixels 
fmean = (sqrt((x_sum/fvalid)^2 + (y_sum/fvalid)^2))
fmean2 =sqrt(x_sum/fvalid) + sqrt(y_sum/fvalid)
fmean3 = (x_sum + y_sum)/fvalid
fmean = mean(bx) + mean(by)
fmean = mean(x_based_on_mag + y_based_on_mag)
par(mfrow = c(1,2))
plot(fmean1, col = divpal, main = 'Fmean')
plot(fmean2, col = divpal, main = 'Fmean')
plot(fmean3, col = divpal2, main = 'Fmean')
plot(fmean, col = divpal2, main = 'Fmean')
plot(sims, col = divpal, main = 'Grad_mag')
mean(bx)
range(na.omit(fmean@data@values))
range(na.omit(fmean1@data@values))
range(na.omit(fmean2@data@values))
range(na.omit(fmean3@data@values))
range(na.omit(y_based_on_mag@data@values))
range(na.omit(x_based_on_mag@data@values))
range(na.omit(sims@data@values))
# Compute deviation vectors
d_xy = (x_based_on_mag - mean(x_based_on_mag)) + (y_based_on_mag-mean(y_based_on_mag))
d_xy = (x_based_on_mag - fmean) + (y_based_on_mag- fmean)
plot(d_xy, col = divpal)
# Compute lengths of xy deviation vector
l_xy = sqrt(sum(d_xy * d_xy))
plot(l_xy, col = divpal)
range(na.omit(l_xy@data@values))

# Compute sd of xy
s_xy = l_xy / sqrt(fvalid)
range(na.omit(s_xy@data@values))
plot(s_xy, col = divpal)



# fvar = variance of gradient values
# x_var <- apply(x_based_on_mag, c(1,2), function(x) {
#    sum((x - mean(x))^2)/(length(x)-1)})
fvar = (x_sumsqr + y_sumsqr)/fvalid
plot(fvar, col = divpal2, main = 'Fvar')
range(na.omit(fvar@data@values))
# fsd =  sqrt of variance of gradient values
fsd = sqrt(fvar)
plot(fsd, col = divpal, main = 'Fsd')
# Compute the lengths of xy deviation matrix
l_xy = sqrt(sum(fvar * fvar))

# get the range of sd
range(na.omit(fsd@data@values))
# fcv = fsd/fmean
fcv = fsd/fmean
plot(fcv, col = divpal, main = 'Fcv')
range(na.omit(fcv@data@values))
# fprob = front pixels/clear pixels 
fprob = fvalid/fclear_m
plot(fprob, col = divpal, main = 'Fprob')
range(na.omit(fprob@data@values))
# fintst = frontal intensity, fmean * fprob
fintst = fmean * fprob
plot(fintst, col = divpal, main = 'Fintst')
range(na.omit(fintst@data@values))
# fpers = based on  threshold -> if fsd meets threshold for that week, assign 1, else 0
pers_thresh <- 0.08  # some threshold range (0 - 1) indicating low sd, thus persistence * value will depend on actual data
# generates binary array to calc cumulative persistence across time (weeks, months, etc)
#fpers = ifelse(fsd <= pers_thresh, 1, 0)

# create an if-else reclassify statement
rc <- function(x1) {
    ifelse(x1 <= pers_thresh, 1, 
         ifelse((x1 > pers_thresh), 0))
}

fpers <- overlay(fsd, fun=rc)
range(na.omit(r.class@data@values))
unique(na.omit(r.class@data@values))
plot(r.class, main = 'Fpers (persistence)')
plot(fpers, col = divpal, main = 'Fpers')
# persistence probability 
fpersprob = fpers * fprob
plot(fpersprob, col = divpal2)
# --- Testing SST Frontal standard deviation thresholds ---- #
# Create a series of thresholds 
threshold = seq(from = 0.08, to = 0.2, by = .01) 

for (i in 1:threshold[1]){
  tmp <- fsd
  tmp[tmp < threshold[i]] <- 0 
  s = raster::stack(tmp)
  for (j in 2:length(threshold)){
    tmp2 <- fsd
    tmp2[tmp2 < threshold[j]] <- 0 
    s[[j]]= tmp2
  }
}

setwd('~/github/ssalois1/frontal_metrics')

# Save the images
pdf(file = 'testing_fsd_thresholds_wk39_2019_y.pdf', width = 5, height = 10)
par(mfrow=c(7,2), mar = c(2, 2, 2, 2))
plot(fsd, main = 'Fsd (frontal temporal variablity)')
for (i in 1:length(threshold)){
  plot(s[[i]], 
       main = paste('Sd threshold =', threshold[i]))
}
dev.off()



# Save the images
pdf(file = 'Frontal_metrics_wk39_2019.pdf', width = 5, height = 10)
par(mfrow=c(7,2), mar = c(2, 2, 2, 2))
plot(fmean, col = divpal2, main = 'Fmean (frontal mean)')
plot(fvar, col = divpal2, main = 'Fvar (frontal variance)')
plot(fsd, col = divpal2, main = 'Fsd (frontal temporal variablity)')
plot(fcv, col = divpal2, main = 'Fcv (frontal coefficient of variation)')
plot(fprob, col = divpal2, main = 'Fprob (frontal probability)')
plot(fintst, col = divpal2, main = 'Fintst (frontal intensity)')
plot(fpers, col = divpal2, main = 'Fpers (frontal persistence)')
plot(fpersprob, col = divpal2, main = 'Fpersprob (persistence probability)')
dev.off()






