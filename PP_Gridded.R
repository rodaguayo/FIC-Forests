rm(list=ls())
cat("\014")  

library("hydroGOF")
library("raster")
library("readxl")
library("RFmerge")
library("zoo")
library("sf")

#Precipitation validation
sample <- raster(matrix(rnorm(400),20,20))
extent(sample) <- c(-72.79,-72.69,-37.55, -37.45)
projection(sample) <- CRS("+init=epsg:4326")

#Observations
period <- seq(as.Date('2006-04-01'), as.Date('2019-03-31'), by = 'day')
pp_shape<-shapefile("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/PP_stations.shp")

pp_data<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/PP_data_qc.csv")
pp_data$Date<-as.Date(pp_data$Date)
pp_data<-subset(pp_data, Date >= min(period) & Date <= max(period))

pp_mswep<-list.files("E:/Datasets/MSWEP/Daily/", full.names = TRUE)
pp_mswep<-stack(pp_mswep[9952:length(pp_mswep)], varname="precipitation")
pp_mswep<-crop(pp_mswep, sample, snap='out')
pp_mswep<-resample(pp_mswep, sample)
pp_mswep<- setZ(pp_mswep, seq(min(period), as.Date('2020-12-30'), by = 'day'))
pp_mswep<-setNames(pp_mswep, seq(min(period), as.Date('2020-12-30'), by = 'day'))
pp_mswep<-subset(pp_mswep, which(getZ(pp_mswep) >= min(period) & getZ(pp_mswep) <= max(period)))

pp_cr2met<-stack("E:/Datasets/CR2MET/CR2MET_pr_v2.0_day_1979_2020_005deg.nc", varname="pr")
pp_cr2met<-crop(pp_cr2met, sample, snap='out')
pp_cr2met<-resample(pp_cr2met, sample)
pp_cr2met<- setZ(pp_cr2met, seq(as.Date('1979-01-01'), as.Date('2020-04-30'), by = 'day'))
pp_cr2met<-setNames(pp_cr2met, seq(as.Date('1979-01-01'), as.Date('2020-04-30'), by = 'day'))
pp_cr2met<-subset(pp_cr2met, which(getZ(pp_cr2met) >= min(period) & getZ(pp_cr2met) <= max(period)))

#Extract, measure performance and compare
pp_cr2met_i<-t(raster::extract(pp_cr2met,pp_shape, method='simple'))
pp_mswep_i<-t(raster::extract(pp_mswep,pp_shape, method='simple'))
colnames(pp_cr2met_i) <- pp_shape$NOMBRE
colnames(pp_mswep_i)  <- pp_shape$NOMBRE

KGE_cr2met<-KGE(sim=pp_cr2met_i, obs=pp_data[,-1], method="2012", out.type="full",na.rm=TRUE)
KGE_cr2met <- as.data.frame(cbind(t(KGE_cr2met$KGE.elements),KGE_cr2met$KGE.value))
KGE_mswep<-KGE(sim=pp_mswep_i, obs=pp_data[,-1], method="2012", out.type="full",na.rm=TRUE)
KGE_mswep <- as.data.frame(cbind(t(KGE_mswep$KGE.elements),KGE_mswep$KGE.value))

#Fill gaps using linear regression with MSWEP (the best option!)
for(i in 1:4){
tf<-lm(as.numeric(pp_data[,i+1]) ~ 0 + as.numeric(pp_mswep_i[,i]))
pp_data[,i+1] <- ifelse(is.na(pp_data[,i+1]), round(pp_mswep_i[,i]*tf$coefficients,0), pp_data[,i+1])
}

#Random forest bias correction
pp_data_zoo<-zoo(pp_data[,-1], order.by = pp_data$Date)
pp_info<-pp_shape@data

area <- st_read("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/area.shp")
dem<-raster("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/DEMs/dem_AL_125f3.tif")
dem<-projectRaster(dem, to = sample)
covariates <- list(dem=dem, cr2met=pp_cr2met, mswep = pp_mswep)

pp_corrected <- RFmerge(x=pp_data_zoo, metadata=pp_info, cov=covariates, ED = TRUE, ntree = 1000,
                        id="ID", lat="lat", lon="lon",  mask = area, training=1, write2disk=FALSE, parallel="parallelWin", par.nnodes = 6)
pp_corrected<-setZ(pp_corrected, period)
pp_corrected<-setNames(pp_corrected, period)

setwd("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/")
writeRaster(pp_corrected, "PP_Corrected.nc", format = "CDF", datatype='INT2S', 
            overwrite=TRUE, varname="pp", varunit="mm", xname="Longitude",   
            yname="Latitude", zname="day")
