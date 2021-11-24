Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")


data<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Results/Change_points_metrics_des.csv")
data$Metric<-as.factor(data$Metric)
data$ID<-as.factor(data$ID)
data$P_value<-as.factor(data$P_value)
data$Trend[is.na(data$Trend)]<-" "

f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 16) 

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
y <- list(titlefont = f, tickfont = f2, ticks = "outside")
tiple <- list(text = "P-value", titlefont = f, tickfont = f2, ticks = "outside")

fig <- plot_ly(data, y = ~Metric, x = ~ID, z = ~P_value, type = "heatmap", colorscale  = "Viridis", colorbar = list(title = tiple))
fig <- fig %>% layout(xaxis = x, yaxis = y)
fig <- fig %>% add_annotations(x = data$ID, y = data$Metric, text =  data$Trend, showarrow = FALSE, font=list(color='white', size = 19))
fig


server <- orca_serve()
server$export(fig, file = "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Figures/Figure7_PET_PP.png", width = 800, height = 900, scale = 3)
server$close()
