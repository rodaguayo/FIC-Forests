rm(list=ls())
cat("\014")  

library("hydroTSM")
library("raster")
library("zoo")

#Open TIF LAI MODIS 
basins<-shapefile("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/Cuencas_N1_11.shp")
basins<-spTransform(basins, CRS("+init=epsg:32719"))
basins<-basins[order(as.numeric(substr(basins$Name, 2,3))),]

LAI<-stack(list.files("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/LAND_COVER", full.names = TRUE, recursive = FALSE))

#Project into basin's CRS
beginCluster(8)
LAI<-projectRaster(LAI, crs = crs(basins), method = "ngb")
LAI[LAI >= 250] <- NA
endCluster()

#Extract mean values for each basin (basin-wide)
LAI_basins<-t(raster::extract(LAI, basins, fun = mean, na.rm = TRUE))
threshold  <-quantile(as.numeric(LAI_basins), 0.01, na.rm = T)
LAI_basins[LAI_basins <= threshold] <- NA #Possible errors
colnames(LAI_basins)<-basins$Name

#Extract date from row-names
year<-as.numeric(substr(row.names(LAI_basins), 14, 17))
day<-as.numeric(substr(row.names(LAI_basins), 19, 21))
date<-as.Date(day, origin = paste0(year, "-01-01"))

#create spline version for LAI time-series
LAI_basins_s<-LAI_basins
for(i in 1:length(basins)){
  LAI_serie<-zoo(LAI_basins[,i], order.by = date)
  LAI_serie<-na.spline(LAI_serie)                             # Fill gaps
  LAI_basins_s[,i] <- smooth.spline(LAI_serie, spar =0.1)$y   # Smooth spline
}

LAI_basins<-cbind(LAI_basins,LAI_basins_s)
rownames(LAI_basins)<-as.character(date)

LAI_mean<-t(monthlyfunction(xts(LAI_basins, order.by = date), FUN = mean, na.rm = TRUE))
LAI_mean[col.order,c("colname4","colname3","colname2","colname5","colname1")]


write.csv(LAI_basins, "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/Basins_LAI.csv")