# Note: sst resolution plotting is at bottom
# Create frontal array (via setting setting all values < threshold to 0)
threshold = seq(from = 0.1, to = 0.2, by = .01) # threshold for classifying front
g.d5 <- (grad[,,5]) # day 5 for week 36 2010, 6 for week 11 2010, day 3 for may 2001
grad <- grad0926
grad <- grad0320
grad <- grad0709
for (i in 1:threshold[1]){
  tmp <- grad
  tmp[tmp < threshold[i]] <- 0 
  #tmpd5 <- (tmp[,,5])
  tmpd5.t <- t(tmp) # tmpd5
  r = raster(tmpd5.t, xmn = min(lon), xmx = max(lon), 
             ymn = min(lat), ymx = max(lat),
             crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r = raster::flip(r,2)  
  s = raster::stack(r)
  for (j in 2:length(threshold)){
    tmp2 <- grad
    tmp2[tmp2 < threshold[j]] <- 0 
    #tmp2d5 <- (tmp2[,,5])
    tmp2d5.t <- t(tmp2) # needs to be transposed , was tmp2d5
    # create a raster 
    r = raster(tmp2d5.t, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    r = flip(r,2) # 
    s[[j]]= r
  }
}

setwd('~/github/ssalois1/frontal_metrics/')

## Color palettes 
pal = c('#191970','#DAA520','#8DD3C7', '#BEBADA',"#FB8072","#80B1D3", 
        "#FDB462", '#FF00FF', '#964B00','#545454', 'red')
divpal = c('#FFFFFF',rev(brewer.pal(10,'RdYlBu')))
divpal = rev(brewer.pal(10,'RdYlBu'))
palchl = c('#191970','#DAA520','#8DD3C7', '#BEBADA',"#FB8072","#80B1D3", 
           "#FDB462", '#FF00FF', 'red')


# Rasterize the grad_mag for plotting images 
grad.t <- t(grad)
grad.r = raster(grad.t, xmn = min(lon), xmx = max(lon), 
                ymn = min(lat), ymx = max(lat),
                crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
grad.r = raster::flip(grad.r,2)  
plot(grad.r)
# Save the images
pdf(file = 'testing_sst_thresholds_007092017_2km.pdf', width = 5, height = 10)
par(mfrow=c(6,2), mar = c(2, 2, 2, 2))
#ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
plot(grad.r, col = divpal, main = 'Grad Mag', 
     ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
for (i in 1:length(threshold)){
  plot(s[[i]], col = divpal, 
       main = paste('Front threshold =', threshold[i]),
       ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
}
dev.off()


### --- CHL --- ### 
setwd(here::here('nc_files/dd/'))
path = here::here('nc_files/dd/')
path = here::here('nc_files/dd/')
files <- list.files(path = path, pattern = glob2rx('D_2006*.nc'), full.names = TRUE)
files <- list.files(path = path, pattern = glob2rx('D_2010*.nc'), full.names = TRUE)
files <- list.files(path = path, pattern = glob2rx('D_2017*.nc'), full.names = TRUE)
files <- list.files(path = path, pattern = glob2rx('W_*.nc'), full.names = TRUE)
threshold = seq(from = 1.01, to = 1.40, by = .01) # threshold for classifying front
for (k in 1:length(files)) {
  nc <- nc_open(files[k])  # Open files
  lat <- ncvar_get(nc, 'latitude')
  lon <- ncvar_get(nc, 'longitude')
  grad <- ncvar_get(nc, attributes(nc$var)$names[1])
  nc_close(nc)
  rm(nc)
  g09262k <- grad
  for (i in 1:threshold[1]){
    tmp <- grad
    tmp[tmp < threshold[i]] <- 0 
    #tmp <- (tmp[,,5]) this file is one day
    tmp.t <- t(tmp)
    r = raster(tmp.t, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    r = raster::flip(r,2)  
    s = raster::stack(r)
    for (j in 2:length(threshold)){
      tmp2 <- grad
      tmp2[tmp2 < threshold[j]] <- 0 
      #tmp2d5 <- (tmp2[,,5])
      tmp2.t <- t(tmp2) # needs to be transposed 
      # create a raster 
      r = raster(tmp2.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      r = flip(r,2) # 
      s[[j]]= r
    }
  }
}   

# setting up grad_mag for comparison
g0926.t <- t(g09262k)
g0926.r = raster(g0926.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
g0926.f = raster::flip(g0926.r,2)  
plot(log(g0926.f), col = pal, main = 'Grad Mag', 
     ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))

g0926.f1km <- g0926.f
g0926.f2km <- g0926.f
g0926.f4km <- g0926.f

g0320.f1km <- g0926.f
g0320.f2km <- g0926.f
g0320.f4km <- g0926.f

g0309.f1km <- g0926.f
g0309.f2km <- g0926.f
g0309.f4km <- g0926.f


pdf(file = 'chl_vs_logchl_1km.pdf', width = 7, height = 4)
par(mfrow = c(1,2))
plot(g0926.f)
plot(log(g0926.f))
range(na.omit(g0926.f@data@values))
range(na.omit(log(g0926.f@data@values)))
dev.off()

pdf(file = 'chl_across_km_0309.pdf', width = 8, height = 10)
par(mfrow = c(3,2))
plot(g0309.f1km, main = 'grad_mag_1km', col= divpal) # zlim = c(0,2.0)
plot(log(g0309.f1km), main = 'log grad_mag_1km', col= divpal)
plot(g0309.f4km, main = 'grad_mag_2km', col= divpal)
plot(log(g0309.f4km), main = 'log grad_mag_2km', col= divpal)
plot(g0309.f2km, main = 'grad_mag_4km', col= divpal)
plot(log(g0309.f2km), main = 'log grad_mag_4km', col= divpal)
dev.off()


# create/save multipanel figure to compare thresholds
pdf(file = 'testing_logchl_thresholds_4km.pdf', width = 5, height = 10)
par(mfrow=c(5,2), mar = c(2, 2, 2, 2))
#ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
plot(log(g0926.f), col = palchl, main = 'Grad Mag', 
     ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
for (i in 1:length(threshold)){
  plot(log(s[[i]]), col = palchl, 
       main = paste('Front threshold =', threshold[i]),
       ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)),
       zlim = c(0.0,0.8))
}
dev.off()

range(na.omit(s[[2]]@data@values))
range(na.omit(log(s[[2]]@data@values)))

plot(log(s[[2]]), col = palchl, 
     main = paste('Front threshold =', threshold[2]),
     ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)), 
     zlim = c(0.0,0.8))
palchl2 = c('#191970','#DAA520','#8DD3C7', "#FB8072",'#FF00FF', 'green', 'red',
            'yellow','blue')

plot(s[[2]], col = palchl2, 
     main = paste('Front threshold =', threshold[2]),
     ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)), 
     zlim = c(1.0,1.08))

setwd('~/github/ssalois1/frontal_metrics/')

pdf(file = 'testing_chl_thresholds0926_2km.pdf', width = 5, height = 10)
par(mfrow=c(5,2), mar = c(2, 2, 2, 2))
#ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
plot(g0926.f, col = palchl2, main = 'Grad Mag', 
     ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
for (i in 1:length(threshold)){
  plot(s[[i]], col = divpal, 
       main = paste('Front threshold =', threshold[i]),
       ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
}
dev.off()

pdf(file = 'testing_chl_thresholds0320_2km.pdf', width = 5, height = 10)
par(mfrow=c(5,2), mar = c(2, 2, 2, 2))
#ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
plot(g0926.f, col = palchl2, main = 'Grad Mag', 
     ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
for (i in 1:length(threshold)){
  plot(s[[i]], col = divpal, 
       main = paste('Front threshold =', threshold[i]),
       ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
}
dev.off()
# ,zlim = c(1.0,1.08)
plot(s[[2]], col = divpal)
plot(s[[2]], col = divpal, 
     main = paste('Front threshold =', threshold[i]),
     ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)),
     zlim = c(1.0,1.5))


> log(1.05)
[1] 0.04879016
> log(1.06)
[1] 0.05826891
> log(1.07)
[1] 0.06765865
> log(1.1)
[1] 0.09531018
> log(1.2)
[1] 0.1823216
log(1.4)
[1] 0.3364722

## --- Notes --- ## 
# Range for 1km data
> range(na.omit(g0926.f@data@values))
1.000000 2.198771
> range(na.omit(log(g0926.f@data@values)))
0.0000000 0.7878984
# Range for 2km data
> range(na.omit(g0926.f@data@values))
1.000000 1.574782
> range(na.omit(log(g0926.f@data@values)))
0.0000000 0.4541166
# Range for 4km data
> range(na.omit(g0926.f@data@values))
1.000000 1.320347
> range(na.omit(log(g0926.f@data@values)))
0.0000000 0.2778949



############# SST #####################
library(ncdf4)
library(raster)
library(RColorBrewer)
### --- SST --- ### 
#setwd(here::here('nc_files/dd/'))
path = here::here('nc_files/dd/')
# these file lists include the same day across different resolutions 
# therefore, files[1] == 1km, files[2] == 2km, files[3] == 4km
# 4/23/2018 good day - can detect fronts even in GOM
files <- list.files(path = path, pattern = glob2rx('D_20180423*.nc'), full.names = TRUE)
# 6/20/2018 patchy but good to examine at 4km 
files <- list.files(path = path, pattern = glob2rx('D_201806*.nc'), full.names = TRUE)
files <- list.files(path = path, pattern = glob2rx('D_2019*.nc'), full.names = TRUE)
threshold = seq(from = 0.1, to = 0.2, by = .01) # threshold for classifying front
# Just to look at 1km
k = 1
threshold = seq(from = 0.3, to = 0.4, by = .01) # threshold for classifying front
## Just to look at 4km
k = 3
threshold = seq(from = 0.01, to = 0.10, by = .01) # threshold for classifying front
#
for (k in 1:length(files)) {
  nc <- nc_open(files[k])  # Open files
  lat <- ncvar_get(nc, 'latitude')
  lon <- ncvar_get(nc, 'longitude')
  grad <- ncvar_get(nc, attributes(nc$var)$names[1])
  nc_close(nc)
  rm(nc)
  g09261km <- grad # manually choose which depending on km 
  g09262km <- grad
  g09264km <- grad
  for (i in 1:threshold[1]){
    tmp <- g09264km
    tmp[tmp < threshold[i]] <- 0 
    #tmp <- (tmp[,,5]) this file is one day
    tmp.t <- t(tmp)
    r = raster(tmp.t, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    r = raster::flip(r,2)  
    s = raster::stack(r)
    for (j in 2:length(threshold)){
      tmp2 <- g09264km
      tmp2[tmp2 < threshold[j]] <- 0 
      #tmp2d5 <- (tmp2[,,5])
      tmp2.t <- t(tmp2) # needs to be transposed 
      # create a raster 
      r = raster(tmp2.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      r = flip(r,2) # 
      s[[j]]= r
    }
  }
}   
# setting up grad_mag for comparison
divpal = rev(brewer.pal(10,'RdYlBu'))
image(g04231km) 
g04232km 
g04234km 
g06201km 
g06202km 
g06204km 
g09261km 
g09262km 
g09264km 
g0926.t <- t(g09264km)
g0926.r = raster(g0926.t, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
g0926.f = raster::flip(g0926.r,2)  
plot(g0926.f, col = divpal)

g0423.f1km <- g0926.f
g0423.f2km <- g0926.f
g0423.f4km <- g0926.f

g0620.f1km <- g0926.f
g0620.f2km <- g0926.f
g0620.f4km <- g0926.f

g0926.f1km <- g0926.f
g0926.f2km <- g0926.f
g0926.f4km <- g0926.f


pdf(file = 'chl_vs_logchl_1km.pdf', width = 7, height = 4)
par(mfrow = c(1,2))
plot(g0926.f)
plot(log(g0926.f))
range(na.omit(g0926.f@data@values))
range(na.omit(log(g0926.f@data@values)))
dev.off()

pdf(file = 'sst_across_km_09262019.pdf', width = 8, height = 5)
par(mfrow = c(1,3))
plot(g0926.f1km, main = 'grad_mag_1km', col= divpal)
plot(g0926.f2km, main = 'grad_mag_2km', col= divpal)
plot(g0926.f4km, main = 'grad_mag_4km', col= divpal)
dev.off()

# create/save multipanel figure to compare thresholds - use log for chl
pdf(file = 'testing_sst_thresholds_4km092619_v1.pdf', width = 5, height = 10)
par(mfrow=c(5,2), mar = c(2, 2, 2, 2))
#ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
plot(g0926.f4km, col = divpal, main = 'Grad Mag', 
     ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
for (i in 1:length(threshold)){
  plot((s[[i]]), col = divpal, 
       main = paste('Front threshold =', threshold[i]),
       ylim = c(min(lat), max(lat)), xlim = c(min(lon), max(lon)))
}
dev.off()


## Take-away: 
## The 0.5 multiplier works well from 2-4km - may be too extreme going down 
## just one km in resolution



