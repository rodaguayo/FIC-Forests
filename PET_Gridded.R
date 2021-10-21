rm(list=ls())
cat("\014")  

library("raster")
library("readxl")
library("Evapotranspiration")

t2m_max_c2<-stack("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/TMAX_Corrected.nc")
t2m_min_c2<-stack("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/TMIN_Corrected.nc")

data("constants")
constants$lat_rad <- -37*3.14/180
constants$Elev <- mean(getValues(dem_hr))

PET_HS<-brick(t2m_max_c2, values=FALSE)
for (i in 1:ncell(t2m_max_c2)) {
  
  climatedata<-data.frame("Tmax" = as.numeric(t2m_max_c2[i]), 
                          "Tmin" = as.numeric(t2m_min_c2[i]), 
                          "Month" = as.numeric(format(period, "%m")),
                          "Day" = as.numeric(format(period, "%d")),
                          "Year" = as.numeric(format(period, "%Y")))
  
  climatedata<-ReadInputs(varnames= c("Tmax", "Tmin"), climatedata, constants, stopmissing =c(10,10,3), timestep = "daily", message = "no")
  PET_HS[i] <- as.numeric(ET.HargreavesSamani(climatedata, constants, ts="daily", message="no", AdditionalStats="no", save.csv="no")$ET.Daily)
  print(i)
}

PET_HS<-setZ(PET_HS, period)
PET_HS<-setNames(PET_HS, period)

setwd("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/")

writeRaster(PET_HS, "PET_Corrected.nc", format = "CDF", overwrite=TRUE, varname="pet", varunit="mm", 
            longname="pet", xname="X", yname="Y", zname="time", zunit="day")