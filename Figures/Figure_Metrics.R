Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")


data<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Results/Change_points_metrics.csv")
data$Basin<-as.factor(data$Basin)

f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside", dtick=1)
y1 <- list(title = "High flows", titlefont = f, tickfont = f2, ticks = "outside", zeroline = TRUE,  dtick=2, range = c(0, 4))
y2 <- list(title = "Low flows", titlefont = f, tickfont = f2, ticks = "outside", zeroline = TRUE,  dtick=2, range = c(0, 4))
y3 <- list(title = "Base flows", titlefont = f, tickfont = f2, ticks = "outside", zeroline = TRUE,  dtick=2, range = c(0, 4))
l <- list(orientation = 'v', xanchor ="center", y = 0.98, x = 0.92, font = f2)

fig1 <- data%>%  group_by(Basin)%>%  plot_ly(x=~Year, y= ~High, type = 'bar', color= ~Basin)
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y1, legend = l)

fig2 <- data%>%  group_by(Basin)%>%  plot_ly(x=~Year, y= ~Low,  type = 'bar', color= ~Basin, showlegend = F)
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, legend = l)

fig3 <- data%>%  group_by(Basin)%>%  plot_ly(x=~Year, y= ~Base, type = 'bar', color= ~Basin, showlegend = F)
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, legend = l)

fig <- subplot(fig1, fig2, fig3, nrows = 3, shareY = T, shareX = T, margin = c(0.04, 0.04, 0.04, 0.01))
fig

server <- orca_serve()
server$export(fig, file = "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Figures/Figure6_Metrics.png", width = 1000, height = 700, scale = 3)
server$close()
