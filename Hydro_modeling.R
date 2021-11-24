rm(list=ls())
cat("\014")  

library("terra")
library("readxl")
library("airGRteaching")
library("exactextractr")
library("hydroGOF")
library("hydroPSO")

## Basins
basins<-sf::st_read("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/Cuencas_N1_N11sel.shp")
basins <-basins[order(as.numeric(substr(basins$Name,2,3))),]
period <- seq(as.Date('2006-04-01'), as.Date('2019-03-31'), by = 'day')

## Streamflow data
q_data<-data.frame(read_xlsx("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Climate/Q_data.xlsx", sheet = "data", guess_max = 30000))
q_data$Date<-as.Date(q_data$Date)
q_data$nicodahue<-NULL
q_data<-subset(q_data, Date >= min(period) & Date <= max(period))

## PP, PET and T2M
pet_corrected<-rast("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Climate/PET_Corrected.nc")
pp_corrected<-rast("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Climate/PP_Corrected.nc")
pp_corrected <-  as.data.frame(t(exact_extract(pp_corrected,  basins , 'mean')))
pet_corrected <- as.data.frame(t(exact_extract(pet_corrected, basins , 'mean')))

#Warm up, calibration and validation period 
warm_period <- seq(1, length(period))[period >= as.Date("2006-04-01") & period <= as.Date("2008-03-31")]
ind_period  <- seq(1, length(period))[period >= as.Date("2008-04-01") & period <= as.Date("2019-03-31")]
ind_periodc  <- subset(period, period >= as.Date("2008-04-01") & period <= as.Date("2019-03-31"))
model       <-list("RunModel_GR4J","RunModel_GR5J","RunModel_GR6J")
transfo     <-list("TransfoParam_GR4J","TransfoParam_GR5J","TransfoParam_GR6J")

KGE_params            <-data.frame(matrix(ncol = 7+6, nrow = 1))
colnames(KGE_params)  <-c("Name", "Model","Stage","KGE", "r", "Beta", "Gamma")
q_results             <-data.frame(matrix(ncol = 0, nrow = 4017))
q_results$date        <-seq(from = as.POSIXct("2008-04-01", tz = "UTC"), to = as.POSIXct('2019-03-31', tz = "UTC"), by = "day")

upperGR <- list(c(100,   -10,    0, 0.2), c(100,  -10,    0,   0.2, -2), c(100, -10,    0, 0.2,  -4,  0))
lowerGR <- list(c(6000,   10, 1000,   5), c(6000,  10,  500,     5,  2), c(6000, 10, 1000,  5,   4,  20))

for (j in 1:3) {
  for (i in 1:4) {
  
  #Data for each basin  
  BasinObs<-data.frame(matrix(NA, nrow = length(period), ncol = 0))
  BasinObs$DatesR <-seq(from = as.POSIXct('2006-04-01', tz = "UTC"), to = as.POSIXct('2019-03-31', tz = "UTC"), by = "day")
  BasinObs$P <- as.numeric(pp_corrected[,i])
  BasinObs$E <- as.numeric(pet_corrected[,i])
  BasinObs$Qmm <- as.numeric(q_data[,i+1]*24*3600)/expanse(vect(basins)[i,])

  InputsModel<- CreateInputsModel(FUN_MOD = get(model[[j]]), DatesR = BasinObs$DatesR, Precip = BasinObs$P, PotEvap = BasinObs$E) 
  RunOptions<- CreateRunOptions(FUN_MOD = get(model[[j]]), InputsModel = InputsModel, IndPeriod_Run = ind_period, IndPeriod_WarmUp = warm_period)      

  ShinyGR(ObsDF = BasinObs, SimPer = c("2008-04-01", "2019-03-31"))
  
  OptimGR <- function(ParamOptim) {
  
    OutputsModel <- get(model[[j]])(InputsModel = InputsModel, RunOptions = RunOptions, Param = ParamOptim) # Simulation given a parameter set
    
    q_obs <- BasinObs$Qmm     [period >= as.Date("2008-04-01") & as.numeric(format(period, "%Y")) %% 2 == 1]
    q_sim <- OutputsModel$Qsim[as.numeric(format(ind_periodc, "%Y")) %% 2 == 1]
    q_sim[is.na(q_obs)] <-NA
    
    mu_obs <- (0.01 * mean(q_obs, na.rm = TRUE))^0.25
    q_sim<-(q_sim^0.25 - mu_obs) / 0.25
    q_obs<-(q_obs^0.25 - mu_obs) / 0.25
    OutputsCrit<-KGE(q_sim, q_obs, method=c("2012"))
    
    return(OutputsCrit)
  }

   optPSO <- hydroPSO(fn = OptimGR, lower = lowerGR[[j]], upper = upperGR[[j]], method="spso2011", 
                      control = list(write2disk=FALSE, MinMax="max", npart=60, maxit=100, normalise=TRUE, REPORT=10, reltol=1E-6))
  
   OutputsModel <- get(model[[j]])(InputsModel = InputsModel, RunOptions = RunOptions, Param = optPSO$par)
   plot(OutputsModel, Qobs = BasinObs$Qmm[ind_period])
   length(optPSO$par) <- 6
   
   #Extract performance values (calibration)
   q_obs<-BasinObs$Qmm     [period >= as.Date("2008-04-01") & as.numeric(format(period, "%Y")) %% 2 == 1]
   q_sim<-OutputsModel$Qsim[as.numeric(format(ind_periodc, "%Y")) %% 2 == 1]
   q_sim[is.na(q_obs)] <-NA
   
   mu_obs <- (0.01 * mean(q_obs, na.rm = TRUE))^0.25
   q_sim<-(q_sim^0.25 - mu_obs) / 0.25
   q_obs<-(q_obs^0.25 - mu_obs) / 0.25
   KGE<-KGE(q_sim, q_obs, method=c("2012"), out.type="full")
   KGE_params<-rbind(KGE_params, c(basins$Name[i], model[j], "Calibration",KGE$KGE.value, as.numeric(KGE$KGE.elements),  as.numeric(optPSO$par)))
   
   #Extract performance values (Validation)
   q_obs<-BasinObs$Qmm     [period >= as.Date("2008-04-01") & as.numeric(format(period, "%Y")) %% 2 == 0]
   q_sim<-OutputsModel$Qsim[as.numeric(format(ind_periodc, "%Y")) %% 2 == 0]
   q_sim[is.na(q_obs)] <-NA
   
   mu_obs <- (0.01 * mean(q_obs, na.rm = TRUE))^0.25
   q_sim<-(q_sim^0.25 - mu_obs) / 0.25
   q_obs<-(q_obs^0.25 - mu_obs) / 0.25
   KGE<-KGE(q_sim, q_obs, method=c("2012"), out.type="full")
   KGE_params<-rbind(KGE_params, c(basins$Name[i], model[j], "Validation",KGE$KGE.value, as.numeric(KGE$KGE.elements),  as.numeric(optPSO$par)))
   
   q_results<-cbind(q_results, OutputsModel$Qsim)
   colnames(q_results)[length(q_results)]<-paste0(basins$Name[i],"_", model[j])
  
  print(paste0(j,"_",i))
}
  
}

write.csv(q_results, "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Results/q_sim.csv")
write.csv(KGE_params, "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Results/KGE_values.csv")
