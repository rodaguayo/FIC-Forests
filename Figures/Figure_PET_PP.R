Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("terra")
library("plotly")
library("RColorBrewer")
library("exactextractr")

basins<-sf::st_read("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/Cuencas_N1_N11sel.shp")
pet_corrected<-rast("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Climate/PET_Corrected.nc")
pp_corrected<-rast("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Climate/PP_Corrected.nc")
time(pp_corrected)<-time(pet_corrected)

pet_corrected <- tapp(pet_corrected, strftime(time(pet_corrected),format="%Y"), fun = sum)
pp_corrected  <- tapp(pp_corrected, strftime(time(pp_corrected),format="%Y"), fun = sum)
pp_corrected <- pp_corrected[[2:(nlyr(pp_corrected)-1)]]
pet_corrected <- pet_corrected[[2:(nlyr(pet_corrected)-1)]]

pp_corrected <- as.data.frame(t(exact_extract(pp_corrected, basins , 'mean')))
pp_corrected <- round(pp_corrected,0)
colnames(pp_corrected) <- basins$Name

pet_corrected <- as.data.frame(t(exact_extract(pet_corrected, basins , 'mean')))
pet_corrected <- round(pet_corrected,0)
colnames(pet_corrected) <- basins$Name

data<-as.data.frame(cbind(rowMeans(pet_corrected), rowMeans(pp_corrected)))
data$date<-seq(from=2007, to = 2018, by = 1)
colnames(data)<-c("PET", "PP", "DATE")

f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside", type = 'date')
y <- list(title = "PP - PET (mm)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE)
l <- list(orientation = 'h', xanchor ="center", y = 1.05, x = 0.5, font = f2)

fig1 <- plot_ly(data, y = ~PP, x = ~DATE, type = 'scatter', mode = 'lines+markers', name = "PP", marker = list(size = 10))
fig1 <- fig1 %>% add_trace(y = ~PET, mode = 'lines+markers', name = "PET", marker = list(size = 10))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, legend = l)
fig1

server <- orca_serve()
server$export(fig1, file = "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Figures/Figure4_PET_PP.png", width = 900, height = 400, scale = 3)
server$close()




