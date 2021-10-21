rm(list=ls())
cat("\014")  

library("rvest")
library("zoo")
pp_data<-data.frame()
  
for (j in 1959:2020) {
  for (i in 1:12) {
     
       url<-paste0("https://climatologia.meteochile.gob.cl/application/informacion/datosMensualesDelElemento/370017/",j,"/",i,"/60")
       pp <- read_html(url)
       pp <- html_nodes(pp, "table")
       pp <- html_table(pp[[2]])
       pp <- as.data.frame(pp)
       pp_data <- rbind(pp_data, pp)
       
       }
    print(j)
    }

pp_data$Fecha<-as.Date(pp_data$Fecha, format = "%d-%m-%Y")
full <- seq(from = as.Date("1959-01-01"), to= as.Date("2020-12-31"), by='1 day')
pp_regular<-data.frame(date=full, value=with(pp_data, RRR24[match(full, Fecha)]))
pp_regular$value<-as.numeric(pp_regular$value)

write.csv(pp_regular, "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Data/pp_data_dmc.csv", row.names = FALSE)
