rm(list=ls())
cat("\014")  

library("hydroGOF")
library("raster")
library("readxl")

sample <- raster(matrix(rnorm(400),20,20))
extent(sample) <- c(-72.79,-72.69,-37.55, -37.45)
projection(sample) <- CRS("+init=epsg:4326")

#Observations
period <- seq(as.Date('2006-04-01'), as.Date('2019-03-31'), by = 'day')
t2m_shape<-shapefile("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/PP_stations.shp")[4,]

t2m_data<-data.frame(read_xlsx("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/T2M_data.xlsx", sheet = "data", guess_max = 30000))
t2m_data$Date<-as.Date(t2m_data$Date)
t2m_data<-subset(t2m_data, Date >= min(period) & Date <= max(period))

#Gridded products
t2m_max_cr2met<-stack("E:/Datasets/CR2MET/CR2MET_tmax_v2.0_day_1979_2020_005deg.nc", varname="tmax")
t2m_min_cr2met<-stack("E:/Datasets/CR2MET/CR2MET_tmin_v2.0_day_1979_2020_005deg.nc", varname="tmin")
t2m_max_cr2met<-crop(t2m_max_cr2met, sample, snap='out')
t2m_min_cr2met<-crop(t2m_min_cr2met, sample, snap='out')

#Downscaling
dem_hr<-projectRaster(raster("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/DEMs/dem_AL_125f3.tif"), to = sample, method="bilinear")
dem_lr<-projectRaster(raster("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/DEMs/dem_AL_125f3.tif"), to = t2m_max_cr2met, method="bilinear")
dem_lr<-resample(dem_lr, dem_hr, method="bilinear")

t2m_max_cr2met<-resample(t2m_max_cr2met, sample, method="ngb")
t2m_min_cr2met<-resample(t2m_min_cr2met, sample, method="ngb")
t2m_max_cr2met<-t2m_max_cr2met+(dem_lr-dem_hr)*0.0065
t2m_min_cr2met<-t2m_min_cr2met+(dem_lr-dem_hr)*0.0065
t2m_max_cr2met<-round(t2m_max_cr2met, 2)
t2m_min_cr2met<-round(t2m_min_cr2met, 2)

t2m_max_cr2met<-setZ(t2m_max_cr2met, seq(as.Date('1979-01-01'), as.Date('2020-04-30'), by = 'day'))
t2m_min_cr2met<-setZ(t2m_min_cr2met, seq(as.Date('1979-01-01'), as.Date('2020-04-30'), by = 'day'))
t2m_max_cr2met<-setNames(t2m_max_cr2met, seq(as.Date('1979-01-01'), as.Date('2020-04-30'), by = 'day'))
t2m_min_cr2met<-setNames(t2m_min_cr2met, seq(as.Date('1979-01-01'), as.Date('2020-04-30'), by = 'day'))
t2m_max_cr2met<-subset(t2m_max_cr2met, which(getZ(t2m_max_cr2met) >= min(period) & getZ(t2m_max_cr2met) <= max(period)))
t2m_min_cr2met<-subset(t2m_min_cr2met, which(getZ(t2m_min_cr2met) >= min(period) & getZ(t2m_min_cr2met) <= max(period)))

#Extract, measure performance 
t2m_max_cr2met_i<-t(raster::extract(t2m_max_cr2met,t2m_shape, method='simple'))
t2m_min_cr2met_i<-t(raster::extract(t2m_min_cr2met,t2m_shape, method='simple'))

KGE_cr2met_tmax <-KGE(sim=as.numeric(t2m_max_cr2met_i), obs=t2m_data$T2M_max, method="2012", out.type="full",na.rm=TRUE)
KGE_cr2met_tmax <- as.data.frame(cbind(t(KGE_cr2met_tmax$KGE.elements),KGE_cr2met_tmax$KGE.value))
KGE_cr2met_tmin <-KGE(sim=as.numeric(t2m_min_cr2met_i), obs=t2m_data$T2M_min, method="2012", out.type="full",na.rm=TRUE)
KGE_cr2met_tmin <- as.data.frame(cbind(t(KGE_cr2met_tmin$KGE.elements),KGE_cr2met_tmin$KGE.value))
KGE_cr2met_tmax$ME<-me(sim=as.numeric(t2m_max_cr2met_i), obs=t2m_data$T2M_max, na.rm=TRUE)
KGE_cr2met_tmin$ME<-me(sim=as.numeric(t2m_min_cr2met_i), obs=t2m_data$T2M_min, na.rm=TRUE)

#Downscaling: Mean and variance scaling 
me_tmax<-KGE_cr2met_tmax$ME
me_tmin<-KGE_cr2met_tmin$ME

rSD_tmax<-1/rSD(sim=as.numeric(t2m_max_cr2met_i)-me_tmax, obs=t2m_data$T2M_max, na.rm=TRUE)
rSD_tmin<-1/rSD(sim=as.numeric(t2m_min_cr2met_i)-me_tmin, obs=t2m_data$T2M_min, na.rm=TRUE)

t2m_max_c1<-t2m_max_cr2met - me_tmax
t2m_max_c2<-t2m_max_c1-mean(t2m_max_c1)
t2m_max_c2<-t2m_max_c2*rSD_tmax
t2m_max_c2<-t2m_max_c2 + mean(t2m_max_c1)

t2m_min_c1<-t2m_min_cr2met - me_tmin
t2m_min_c2<-t2m_min_c1-mean(t2m_min_c1)
t2m_min_c2<-t2m_min_c2*rSD_tmin
t2m_min_c2<-t2m_min_c2 + mean(t2m_min_c1)

t2m_max_c2<-setZ(t2m_max_c2, period)
t2m_min_c2<-setZ(t2m_min_c2, period)
t2m_max_c2<-setNames(t2m_max_c2, period)
t2m_min_c2<-setNames(t2m_min_c2, period)
t2m_max_c2<-stack(t2m_max_c2)
t2m_min_c2<-stack(t2m_min_c2)

#New validation: It works!
t2m_max_cr2met_i<-t(raster::extract(t2m_max_c2,t2m_shape, method='simple'))
t2m_min_cr2met_i<-t(raster::extract(t2m_min_c2,t2m_shape, method='simple'))

KGE_cr2met_tmax <-KGE(sim=as.numeric(t2m_max_cr2met_i), obs=t2m_data$T2M_max, method="2012", out.type="full",na.rm=TRUE)
KGE_cr2met_tmax <- as.data.frame(cbind(t(KGE_cr2met_tmax$KGE.elements),KGE_cr2met_tmax$KGE.value))
KGE_cr2met_tmin <-KGE(sim=as.numeric(t2m_min_cr2met_i), obs=t2m_data$T2M_min, method="2012", out.type="full",na.rm=TRUE)
KGE_cr2met_tmin <- as.data.frame(cbind(t(KGE_cr2met_tmin$KGE.elements),KGE_cr2met_tmin$KGE.value))
KGE_cr2met_tmax$ME<-me(sim=as.numeric(t2m_max_cr2met_i), obs=t2m_data$T2M_max, na.rm=TRUE)
KGE_cr2met_tmin$ME<-me(sim=as.numeric(t2m_min_cr2met_i), obs=t2m_data$T2M_min, na.rm=TRUE)

setwd("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/")

writeRaster(t2m_max_c2, "TMAX_Corrected.nc", format = "CDF", overwrite=TRUE, varname="tmax", varunit="degC", 
            longname="tmac", xname="X", yname="Y", zname="time", zunit="day")

writeRaster(t2m_min_c2, "TMIN_Corrected.nc", format = "CDF", overwrite=TRUE, varname="tmin", varunit="degC", 
            longname="tmin", xname="X", yname="Y", zname="time", zunit="day")



