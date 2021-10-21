rm(list=ls())
cat("\014")  

library("raster")
library("readxl")

#Precipitation validation
period <- seq(as.Date('2006-04-01'), as.Date('2019-03-31'), by = 'day')

pp_data<-data.frame(read_xlsx("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/PP_data.xlsx", sheet = "data", guess_max = 30000))
pp_data$Date<-as.Date(pp_data$Date)
pp_data<-subset(pp_data, Date >= min(period) & Date <= max(period))
rownames(pp_data)<-pp_data$Date
pp_data<-subset(pp_data, select = -c(Date))

errors<-data.frame(matrix(NA, nrow = 0, ncol = 2))

for (i in 1:nrow(pp_data)) {
  
  if(sum(is.na(pp_data[i,])) == 0){
    
    for (j in 1:ncol(pp_data)) {
    
    #All PPi>0, PP = 0
    if(pp_data[i,j] == 0 & mean(as.numeric(pp_data[i,-j]), na.rm = TRUE) > 5){
      pp_data[i,j] <- NA
      print(paste0("Code 1  ", rownames(pp_data)[i]))
      errors<-rbind(errors, c(1,rownames(pp_data)[i]))}
    
    #All PPi=0, PP > 0
    if(pp_data[i,j] > 5 & mean(as.numeric(pp_data[i,-j]), na.rm = TRUE) == 0){
      pp_data[i,j] <- 0
      print(paste0("Code 2  ", rownames(pp_data)[i]))
      errors<-rbind(errors, c(2,rownames(pp_data)[i]))}
    
    #Too wet or dry if there is precipitacion
    if(mean(as.numeric(pp_data[i,]), na.rm = TRUE) > 5 & sum(is.na(pp_data[i,])) == 0){
      
      #All PPi=0, PP > 0
      if(pp_data[i,j]/mean(as.numeric(pp_data[i,]), na.rm = TRUE) > 2){
        pp_data[i,j] <- NA
        print(paste0("Code 3  ", rownames(pp_data)[i]))
        errors<-rbind(errors, c(3,rownames(pp_data)[i]))} 
      
      #All PPi=0, PP > 0
      else if (pp_data[i,j]/mean(as.numeric(pp_data[i,]), na.rm = TRUE) < 0.5) {
          pp_data[i,j] <- NA
          print(paste0("Code 4  ", rownames(pp_data)[i]))
          errors<-rbind(errors, c(4,rownames(pp_data)[i]))}
    }
    }
  }
  }

pp_data$Date<-rownames(pp_data)
pp_data <- pp_data[c("Date", "P1", "P2", "P3", "P4")]
setwd("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/")
write.csv(pp_data, "PP_data_qc.csv", row.names = FALSE)

