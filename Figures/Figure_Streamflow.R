Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  


library("readxl")
library("plotly")
library("RColorBrewer")

## Streamflow data
basins<-sf::st_read("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/GIS/Cuencas_N1_N11sel.shp")
basins <-basins[order(as.numeric(substr(basins$Name,2,3))),]
basins$area<-terra::expanse(terra::vect(basins))

period <- seq(as.Date('2008-04-01'), as.Date('2019-03-31'), by = 'day')

data<-data.frame(read_xlsx("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Climate/Q_data.xlsx", sheet = "data", guess_max = 30000))
data$Date<-as.Date(data$Date)
data$nicodahue<-NULL
data<-subset(data, Date >= min(period) & Date <= max(period))
data[,2:5]<-t(t(data[,2:5])*24*3600/basins$area)
data<-cbind(data,read.csv("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Results/q_sim.csv"))


f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside", type = 'date')
y1 <- list(title = "N04 (mm/day)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, range = c(0,30))
y2 <- list(title = "N08 (mm/day)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, range = c(0,30))
y3 <- list(title = "N09 (mm/day)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, range = c(0,30))
y4 <- list(title = "N11 (mm/day)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, range = c(0,30))
l <- list(orientation = 'h', xanchor ="center", y = 1, x = 0.8, font = f2)
colors<-brewer.pal(3, 'Dark2')

fig1 <- plot_ly(data, showlegend = FALSE, y = ~N04_RunModel_GR4J, x = ~Date, type = 'scatter', mode = 'lines', name = "GR4J",  opacity  = 1, line = list(color = colors[1], width = 1))
fig1 <- fig1 %>% add_trace(y = ~N04_RunModel_GR5J, mode = 'lines', name = "GR5J",  opacity  = 1, line = list(color = colors[2], width = 1))
fig1 <- fig1 %>% add_trace(y = ~N04_RunModel_GR6J, mode = 'lines', name = "GR6J",  opacity  = 1, line = list(color = colors[3], width = 1))
fig1 <- fig1 %>% add_trace(y = ~N04, mode = 'lines', name = "OBS", opacity  = 0.4, line = list(color = "black", width = 3))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y1)

fig2 <- plot_ly(data, showlegend = FALSE, y = ~N08_RunModel_GR4J, x = ~Date, type = 'scatter', mode = 'lines', name = "GR4J",  opacity  = 1, line = list(color = colors[1], width = 1))
fig2 <- fig2 %>% add_trace(y = ~N08_RunModel_GR5J, mode = 'lines', name = "GR5J",  opacity  = 1, line = list(color = colors[2], width = 1))
fig2 <- fig2 %>% add_trace(y = ~N08_RunModel_GR6J, mode = 'lines', name = "GR6J",  opacity  = 1, line = list(color = colors[3], width = 1))
fig2 <- fig2 %>% add_trace(y = ~N08, mode = 'lines', name = "OBS", opacity  = 0.4, line = list(color = "black", width = 3))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2)

fig3 <- plot_ly(data, showlegend = FALSE, y = ~N09_RunModel_GR4J, x = ~Date, type = 'scatter', mode = 'lines', name = "GR4J",  opacity  = 1, line = list(color = colors[1], width = 1))
fig3 <- fig3 %>% add_trace(y = ~N09_RunModel_GR5J, mode = 'lines', name = "GR5J",  opacity  = 1, line = list(color = colors[2], width = 1))
fig3 <- fig3 %>% add_trace(y = ~N09_RunModel_GR6J, mode = 'lines', name = "GR6J",  opacity  = 1, line = list(color = colors[3], width = 1))
fig3 <- fig3 %>% add_trace(y = ~N09, mode = 'lines', name = "OBS", opacity  = 0.4, line = list(color = "black", width = 3))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3)

fig4 <- plot_ly(data, y = ~N11_RunModel_GR4J, x = ~Date, type = 'scatter', mode = 'lines', name = "GR4J",  opacity  = 1, line = list(color = colors[1], width = 1))
fig4 <- fig4 %>% add_trace(y = ~N11_RunModel_GR5J, mode = 'lines', name = "GR5J",  opacity  = 1, line = list(color = colors[2], width = 1))
fig4 <- fig4 %>% add_trace(y = ~N11_RunModel_GR6J, mode = 'lines', name = "GR6J",  opacity  = 1, line = list(color = colors[3], width = 1))
fig4 <- fig4 %>% add_trace(y = ~N11, mode = 'lines', name = "OBS", opacity  = 0.4, line = list(color = "black", width = 3))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, legend = l)

fig <- subplot(fig1, fig2, fig3, fig4, nrows = 4, shareY = T, shareX = T, margin = c(0.01, 0.01, 0.01, 0.01))
fig

server <- orca_serve()
server$export(fig, file = "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Figures/Figure8_Streamflow.png", width = 1100, height = 1200, scale = 3)
server$close()

