rm(list=ls())
cat("\014")  

library("hydroGOF")
library("terra")
library("readxl")

sample <- rast(matrix(rnorm(400),20,20), crs = "+init=epsg:4326")
ext(sample) <- c(-72.79,-72.69,-37.55, -37.45)

#Observations
period <- seq(from = as.POSIXct('2006-04-01', tz="UTC"), to = as.POSIXct('2019-03-31', tz="UTC"), by = "day")
t2m_shape<-vect("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/PP_stations.shp")[4,]

t2m_data<-data.frame(read_xlsx("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Climate/T2M_data.xlsx", sheet = "data", guess_max = 1000))
t2m_data$Date<-as.POSIXct(t2m_data$Date)
t2m_data<-subset(t2m_data, Date >= min(period) & Date <= max(period))

#Gridded products
t2m_max_cr2met<-rast("E:/Datasets/CR2MET/CR2MET_tmax_v2.0_day_1979_2020_005deg.nc")
t2m_min_cr2met<-rast("E:/Datasets/CR2MET/CR2MET_tmin_v2.0_day_1979_2020_005deg.nc")
t2m_max_cr2met<-resample(t2m_max_cr2met, sample, method='bilinear')
t2m_min_cr2met<-resample(t2m_min_cr2met, sample, method='bilinear')

#Downscaling
dem_hr<-project(rast("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/DEMs/dem_AL_125f3.tif"), sample, method="bilinear")
dem_lr<-project(rast("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/DEMs/dem_AL_125f3.tif"), rast("E:/Datasets/CR2MET/CR2MET_tmax_v2.0_day_1979_2020_005deg.nc"), method="bilinear")
dem_lr<-resample(dem_lr, dem_hr, method="bilinear")

t2m_max_cr2met<-t2m_max_cr2met+(dem_lr-dem_hr)*0.0065
t2m_min_cr2met<-t2m_min_cr2met+(dem_lr-dem_hr)*0.0065
t2m_max_cr2met<-round(t2m_max_cr2met, 2)
t2m_min_cr2met<-round(t2m_min_cr2met, 2)

t2m_max_cr2met<-setNames(t2m_max_cr2met, seq(as.Date('1979-01-01'), as.Date('2020-04-30'), by = 'day'))
t2m_min_cr2met<-setNames(t2m_min_cr2met, seq(as.Date('1979-01-01'), as.Date('2020-04-30'), by = 'day'))
t2m_max_cr2met<-subset(t2m_max_cr2met, which(time(t2m_max_cr2met) >= min(period) & time(t2m_max_cr2met) <= max(period)))
t2m_min_cr2met<-subset(t2m_min_cr2met, which(time(t2m_min_cr2met) >= min(period) & time(t2m_min_cr2met) <= max(period)))

#Extract, measure performance 
t2m_max_cr2met_i<-as.numeric(t(terra::extract(t2m_max_cr2met,t2m_shape)))
t2m_min_cr2met_i<-as.numeric(t(terra::extract(t2m_min_cr2met,t2m_shape)))
t2m_max_cr2met_i<-t2m_max_cr2met_i[2:length(t2m_max_cr2met_i)]
t2m_min_cr2met_i<-t2m_min_cr2met_i[2:length(t2m_min_cr2met_i)]

KGE_cr2met_tmax     <-KGE(sim=t2m_max_cr2met_i, obs=t2m_data$T2M_max, method="2012", out.type="full",na.rm=TRUE)
KGE_cr2met_tmax     <- as.data.frame(cbind(t(KGE_cr2met_tmax$KGE.elements),KGE_cr2met_tmax$KGE.value))
KGE_cr2met_tmin     <-KGE(sim=t2m_min_cr2met_i, obs=t2m_data$T2M_min, method="2012", out.type="full",na.rm=TRUE)
KGE_cr2met_tmin     <- as.data.frame(cbind(t(KGE_cr2met_tmin$KGE.elements),KGE_cr2met_tmin$KGE.value))
KGE_cr2met_tmax$ME  <-me(sim=t2m_max_cr2met_i, obs=t2m_data$T2M_max, na.rm=TRUE)
KGE_cr2met_tmin$ME  <-me(sim=t2m_min_cr2met_i, obs=t2m_data$T2M_min, na.rm=TRUE)

#Downscaling: Mean and variance scaling 
me_tmax<-KGE_cr2met_tmax$ME
me_tmin<-KGE_cr2met_tmin$ME

rSD_tmax<-1/rSD(sim=t2m_max_cr2met_i-me_tmax, obs=t2m_data$T2M_max, na.rm=TRUE)
rSD_tmin<-1/rSD(sim=t2m_min_cr2met_i-me_tmin, obs=t2m_data$T2M_min, na.rm=TRUE)

t2m_max_c1<-t2m_max_cr2met - me_tmax
t2m_max_c2<-t2m_max_c1-mean(t2m_max_c1)
t2m_max_c2<-t2m_max_c2*rSD_tmax
t2m_max_c2<-t2m_max_c2 + mean(t2m_max_c1)

t2m_min_c1<-t2m_min_cr2met - me_tmin
t2m_min_c2<-t2m_min_c1-mean(t2m_min_c1)
t2m_min_c2<-t2m_min_c2*rSD_tmin
t2m_min_c2<-t2m_min_c2 + mean(t2m_min_c1)
t2m_max_c2<-setNames(t2m_max_c2, period)
t2m_min_c2<-setNames(t2m_min_c2, period)

t2m_max_c2_m <- tapp(t2m_max_c2, strftime(time(t2m_max_c2),format="%Y"), fun = mean)
t2m_max_c2_m <- mean(t2m_max_c2_m[[-c(1,nlyr(t2m_max_c2_m))]])
t2m_min_c2_m <- tapp(t2m_min_c2, strftime(time(t2m_min_c2),format="%Y"), fun = mean)
t2m_min_c2_m <- mean(t2m_min_c2_m[[-c(1,nlyr(t2m_min_c2_m))]])
t2m_mean<-mean(c(t2m_max_c2_m, t2m_min_c2_m))

#New validation: It works!
t2m_max_cr2met_i<-as.numeric(t(terra::extract(t2m_max_c2,t2m_shape)))
t2m_min_cr2met_i<-as.numeric(t(terra::extract(t2m_min_c2,t2m_shape)))
t2m_max_cr2met_i<-t2m_max_cr2met_i[2:length(t2m_max_cr2met_i)]
t2m_min_cr2met_i<-t2m_min_cr2met_i[2:length(t2m_min_cr2met_i)]

KGE_cr2met_tmax    <-KGE(sim=t2m_max_cr2met_i, obs=t2m_data$T2M_max, method="2012", out.type="full",na.rm=TRUE)
KGE_cr2met_tmax    <- as.data.frame(cbind(t(KGE_cr2met_tmax$KGE.elements),KGE_cr2met_tmax$KGE.value))
KGE_cr2met_tmin    <-KGE(sim=t2m_min_cr2met_i, obs=t2m_data$T2M_min, method="2012", out.type="full",na.rm=TRUE)
KGE_cr2met_tmin    <- as.data.frame(cbind(t(KGE_cr2met_tmin$KGE.elements),KGE_cr2met_tmin$KGE.value))
KGE_cr2met_tmax$ME <-me(sim=t2m_max_cr2met_i, obs=t2m_data$T2M_max, na.rm=TRUE)
KGE_cr2met_tmin$ME <-me(sim=t2m_min_cr2met_i, obs=t2m_data$T2M_min, na.rm=TRUE)

setwd("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Climate/")

writeRaster(t2m_mean, "T2M_mean.tif", overwrite = TRUE)
writeCDF(t2m_max_c2, "TMAX_Corrected.nc", overwrite=TRUE, varname="t2m_max", unit="degC", longname="Maximun temperature", zname="time", compression = 9)
writeCDF(t2m_min_c2, "TMIN_Corrected.nc", overwrite=TRUE, varname="t2m_min", unit="degC", longname="Minimum temperature", zname="time", compression = 9)
