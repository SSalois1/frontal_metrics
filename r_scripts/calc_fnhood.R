# Pull in netcdf
nc <- nc_open('DD_20180402_20180408-AT-R2018-NESGRID-NWA_SUBSET-PXY_1_1413572-GRAD_SST-BOA-D3_DAT.nc')
nc <- nc_open('DD_20000501_20000531-AT-R2018-NESGRID-GRAD_SST-BOA-STACKED.nc')

# Extract lat, lon, grad, x, y
lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude", verbose = F)
grad <- ncvar_get(nc, attributes(nc$var)$names[2])
x <- ncvar_get(nc, attributes(nc$var)$names[3])
y <- ncvar_get(nc, attributes(nc$var)$names[4])
# these are old files so need to replace fillvalue with NA - Kim has updated
# the newer files with _Fillvalue so that R can recognize it and does this automatically 
fillvalue <- ncatt_get(nc, 'GRAD_SST', "_fillvalue")
grad[grad == fillvalue$value] <- NA
x[x == fillvalue$value] <- NA
y[y == fillvalue$value] <- NA
# close the netcdf
nc_close(nc) 
# save two versions of grad_mag with and w/out frontal threshold (sims here, front below)
sims <- grad
# Just looking at day 4 of this week for now
gradsst5 <- (sims[,,5])
gradsst5 <- t(gradsst5) # needs to be transposed 
# create a raster 
r = raster(gradsst5, xmn = min(lon), xmx = max(lon), 
           ymn = min(lat), ymx = max(lat),
           crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r = flip(r,2)  # flip it so it is correct orientation
plot(r) # see it! 


# weekly sst frontal metrics #
# On server: 
# path = '/nadata/PROJECTS/IDL_PROJECTS/ILLEX_INDICATORS/R_SCRIPTS/SST_ANOM/NETCDFS/'
# Locally: 
path = here::here('nc_files/')
files <- list.files(path = path, pattern = glob2rx('W_*.nc'), full.names = TRUE)
setwd('C:/Users/sarah.salois/Documents/github/ssalois1/frontal_metrics/rasters')
threshold = 0.4 # threshold for classifying front
## local neighbourhood average of frontal persistence ##
for (i in 1:length(files)) {
  nc <- nc_open(files[i])  # Open files
  lat <- ncvar_get(nc, 'latitude')
  lon <- ncvar_get(nc, 'longitude')
  grad <- ncvar_get(nc, attributes(nc$var)$names[2])
  nc_close(nc)
  rm(nc)
  grad[grad < threshold] <- 0 # set all values < thresh == 0 
  for (j in 1:1){
  tmp <- (grad[,,j])
  tmp <- t(tmp) # needs to be transposed 
  # create a raster 
  r = raster(tmp, xmn = min(lon), xmx = max(lon), 
             ymn = min(lat), ymx = max(lat),
             crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r = flip(r,2) # flip it to correct orientation
  s = raster::stack(r)
  for (k in 2:7){
    tmp <- (grad[,,k])
    tmp <- t(tmp) # needs to be transposed 
    # create a raster 
    r = raster(tmp, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    r = flip(r,2) # 
    s[[k]]= r
  }
 }  
  f_stack = stackApply(s, c(1,1,1,1,1,1,1), fun = mean)
  assign(paste0('ww_fvalid',i), f_stack)  
  # also add smoother to mean_fronts for comparison
  assign(paste0('ww_fnhood_2021',i), raster.gaussian.smooth(f_stack, 
                                                       sigma = 2,
                                                       n = 5,
                                                       type = mean))
  
  }


rlist <- mget(ls(pattern='ww_fnhood_2021*'))
fnhood_sst_2021 = raster::stack(rlist)
b = brick(fnhood_sst_2021)
raster::writeRaster(b, filename = 'b_fnhood_sst_2021.tif', format = "GTiff",
                    bylayer = F, 
                    crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
                    overwrite = TRUE) # bylayer = T if want individual layers to save

image(s[[8]])
image(ww_gradsst)
image(kern.weekly.fvalid)
image(ww_fnhood3)
plot(ww_fvalid1, main = 'week 1 raw')
plot(ww_fnhood_20211, main = 'week 1 filter')
plot(ww_fvalid2, main = 'week 2 raw')
plot(ww_fnhood_20212, main = 'week 2 filter')
plot(ww_fvalid3, main = 'week 3')
plot(ww_fnhood_20213, main = 'week 3 filter')
plot(ww_fvalid4, main = 'week 4')
plot(ww_fnhood_20214, main = 'week 4 filter')


library(rgdal)
# get EPU shape files 
wd = here::here('shape_files/')
epu <- readOGR(wd,'EPU_NOESTUARIES')
plot(epu)
epu@data
mab <- epu[na.omit(epu@data$EPU == 'MAB'),]
gom <-  epu[na.omit(epu@data$EPU == 'GOM'),]
gb <-  epu[na.omit(epu@data$EPU == 'GB'),]
plot(NULL, xlim = c(doy[1],doy[(length(doy))]), ylim = c(4,25), las = 1, 
     ylab = "", yaxt = "n", xaxt = "n", xlab = "")
plot(epu, xaxt = 'n', yaxt = 'n')
plot(gom, col = 'slateblue', add = TRUE)
plot(gb, col = 'goldenrod3', add = TRUE)
plot(mab, col = 'orangered', add = TRUE)
fnhood_mab_wk12 <-  raster::extract(ww_fnhood_202112, mab, weights = FALSE, 
                                    fun = NULL, na.rm = TRUE)

fnhood_mab_wk12_crop <- raster::crop(x = ww_fnhood_202112, y = mab)
plot(fnhood_mab_wk12_crop)
fnhood_mab_wk14_crop <- raster::crop(x = ww_fnhood_202114, y = mab)
plot(fnhood_mab_wk14_crop)
fnhood_gom_wk12_crop <- raster::crop(x = ww_fnhood_202112, y = gom)
plot(fnhood_gom_wk12_crop)
fnhood_gb_wk12_crop <- raster::crop(x = ww_fnhood_202112, y = gb)
plot(fnhood_gb_wk12_crop)

head(fnhood_mab_wk12)
extent(ww_fnhood_202112)
# Convert to dataframe
# 2021
gb_wk12 = as.data.frame(fnhood_gb_wk12_crop, xy = TRUE)
colnames(gb_wk12) <- c('lon', 'lat','fhood_sst')
mab_wk12 = as.data.frame(fnhood_mab_wk12_crop, xy = TRUE)
colnames(mab_wk12) <- c('lon', 'lat','fhood_sst')
mab_wk14 = as.data.frame(fnhood_mab_wk14_crop, xy = TRUE)
colnames(mab_wk14) <- c('lon', 'lat','fhood_sst')
gom_wk12 = as.data.frame(fnhood_gom_wk12_crop, xy = TRUE)
colnames(gom_wk12) <- c('lon', 'lat','fhood_sst')


ggplot(mab_wk12, aes(x=lat, y=fhood_sst)) +
  geom_line(color = 'slateblue', alpha = 0.5) +
  geom_line(data = mab_wk14, aes(x=lat, y=fhood_sst), 
            color = 'magenta', alpha = 0.5) +
  labs(title = 'MAB', subtitle = 'Week 12 vs 14') +
  xlab('Longitude') +
  ylab('Frontal Neighborhood') +
  theme_classic()

ggplot(mab_wk12, aes(x=lon, y=fhood_sst)) +
  geom_line(color = 'slateblue', alpha = 0.5) +
  geom_line(data = mab_wk14, aes(x=lon, y=fhood_sst), 
            color = 'magenta', alpha = 0.5) +
  labs(title = 'MAB', subtitle = 'Week 12 vs 14') +
  xlab('Longitude') +
  ylab('Frontal Neighborhood') +
  theme_classic()


### Interannual variability in seasonal front frequency ###



## Plan : 1. calculate frontal neighborhood for week 12
## across all years, 2. calculate climatology for week 12 
## 3. compare climatology vs 2021, 4. calc interannual variability for wk 12

# weekly sst frontal metrics #
# On server: 
# path = '/nadata/PROJECTS/IDL_PROJECTS/ILLEX_INDICATORS/R_SCRIPTS/SST_ANOM/NETCDFS/'
# Locally:
path = here::here('nc_files/wk_12')
files <- list.files(path = path, pattern = glob2rx('W_*.nc'), full.names = TRUE)
setwd('C:/Users/sarah.salois/Documents/github/ssalois1/frontal_metrics/rasters')
threshold = 0.4 # threshold for classifying front
for (i in 1:length(files)) {
  nc <- nc_open(files[i])  # Open files
  lat <- ncvar_get(nc, 'latitude')
  lon <- ncvar_get(nc, 'longitude')
  grad <- ncvar_get(nc, attributes(nc$var)$names[2])
  nc_close(nc)
  rm(nc)
  grad[grad < threshold] <- 0 # set all values < thresh == 0 
  for (j in 1:1){
    tmp <- (grad[,,j])
    tmp <- t(tmp) # needs to be transposed 
    # create a raster 
    r = raster(tmp, xmn = min(lon), xmx = max(lon), 
               ymn = min(lat), ymx = max(lat),
               crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    r = raster::flip(r,2) # flip it to correct orientation
    s = raster::stack(r)
    for (k in 2:7){
      tmp <- (grad[,,k])
      tmp <- t(tmp) # needs to be transposed 
      # create a raster 
      r = raster(tmp, xmn = min(lon), xmx = max(lon), 
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      r = raster::flip(r,2) # 
      s[[k]]= r
    }
  }  
  f_stack = stackApply(s, c(1,1,1,1,1,1,1), fun = mean)
  assign(paste0('ww_fvalid_wk12',i), f_stack)  
  # also add smoother to mean_fronts for comparison
  assign(paste0('ww_fnhood_wk12',i), raster.gaussian.smooth(f_stack, 
                                                            sigma = 2,
                                                            n = 5,
                                                            type = mean))
  
}

rlist <- mget(ls(pattern='ww_fnhood_wk12*'))
fnhood_sst_wk12 = raster::stack(rlist)
clmtgy_wk12 = stackApply(fnhood_sst_wk12, 
                         c(1,1,1,1,1,1,1, 
                          1,1,1,1,1,1,1,
                          1,1,1,1,1,1,1,1),
                         fun = mean)
raster::writeRaster(clmtgy_wk12, filename = 'clmtgy_wk12.tif', format = "GTiff",
            bylayer = F, 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"),
            overwrite = TRUE) # bylayer = T if want individual layers to save

library(RColorBrewer)
pal <- RColorBrewer::brewer.pal(9,'RdBu')
par(mfrow=c(1,2))
plot(log(clmtgy_wk12), main = 'Climatology of Week 12')
plot(log(ww_fnhood_wk1222), main = 'Week 12 2021')
fnhood_diff <- clmtgy_wk12 - ww_fnhood_wk1222 
fnhood_diff2 <- ww_fnhood_wk1222 - clmtgy_wk12
plot(fnhood_diff, main = 'Diff Week 12 2021')
plot(fnhood_diff2, main = 'Diff Week 12 2021')
# diff color palette
plot(fnhood_diff, main = 'Diff Week 12 2021', col = pal)
