Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("readxl")
library("plotly")
library("RColorBrewer")

data<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Results/Basins_LAI.csv")

for(j in 1:12) {
    data[which(as.numeric(format(data$date, "%m"))==j),2:23]<-data[which(as.numeric(format(data$date, "%m"))==j),2:23]-unlist(mean_values[j,2:23])
}

f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside", type = 'date')
y <- list(title = "LAI", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 15, range = c(10,69))
l <- list(orientation = 'h', xanchor ="center", y = 1.05, x = 0.5, font = f2)

colors1<-brewer.pal(4, 'Greens')
fig1 <- plot_ly(data, y = ~N01s, x = ~date, type = 'scatter', mode = 'lines', name = "N01", line = list(color = colors1[1]))
fig1 <- fig1 %>% add_trace(y = ~N02s, mode = 'lines', name = "N02", line = list(color = colors1[2]))
fig1 <- fig1 %>% add_trace(y = ~N03s, mode = 'lines', name = "N03", line = list(color = colors1[3]))
fig1 <- fig1 %>% add_trace(y = ~N04s, mode = 'lines', name = "N04", line = list(color = colors1[4]))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, legend = l)

colors2<-brewer.pal(3, 'Blues')
fig2 <- plot_ly(data, y = ~N06s, x = ~date, type = 'scatter', mode = 'lines', name = "N06", line = list(color = colors2[1]))
fig2 <- fig2 %>% add_trace(y = ~N07s, mode = 'lines', name = "N07", line = list(color = colors2[2]))
fig2 <- fig2 %>% add_trace(y = ~N08s, mode = 'lines', name = "N08", line = list(color = colors2[3]))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y)

colors3<-brewer.pal(3, 'Reds')
fig3 <- plot_ly(data, y = ~N09s, x = ~date, type = 'scatter', mode = 'lines', name = "N09", line = list(color = colors3[2]))
fig3 <- fig3 %>% add_trace(y = ~N10s, mode = 'lines', name = "N10", line = list(color = colors3[3]))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y)

colors4<-brewer.pal(3, 'Purples')
fig4 <- plot_ly(data, y = ~N05s, x = ~date, type = 'scatter', mode = 'lines', name = "N05", line = list(color = colors4[2]))
fig4 <- fig4 %>% add_trace(y = ~N11s, mode = 'lines', name = "N11", line = list(color = colors4[3]))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y)

fig <- subplot(fig1, fig2, fig3, fig4, nrows = 4, shareY = T, shareX = T, margin = c(-0.04, -0.04, 0, -0.01))
fig

server <- orca_serve()
server$export(fig, file = "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Figures/Figure_LAI.png", width = 1200, height = 1000, scale = 3)
server$close()




