Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("readxl")
library("plotly")
library("RColorBrewer")

data_q<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Results/KGE_values.csv")
data_q<-data_q[-1, ]
data_q$Stage <- substr(data_q$Stage, 1, 1) 
data_q$Model <- as.factor(data_q$Model)
levels(data_q$Model)<- c("GR4J", "GR5J", "GR6J")
data_q$Model <- factor(paste(data_q$Model,"-",data_q$Stage), levels = c("GR4J - C", "GR4J - V", "GR5J - C", "GR5J - V","GR6J - C","GR6J - V"))

f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
title <-list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
y <- list(title = "Correlation (r)", titlefont = f, 
          tickfont = f2, dtick = 0.1, ticks = "outside", zeroline = FALSE, range = c(0.6, 1))

fig1 <- plot_ly(data_q, y = ~r, x = ~Model, type = "box", color = ~Model, colors = brewer.pal(6, 'Dark2'), boxmean = T)
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title)

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.99)
y2 <- list(title = "Bias (β)", titlefont = f, range = c(0.8, 1.2),
           tickfont = f2, dtick = 0.1, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_q, y = ~Beta, x = ~Model, type = "box", color = ~Model, colors = brewer.pal(6, 'Dark2'), boxmean = T)
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2)

title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y3 <- list(title = "Variability (γ)", titlefont = f, range = c(0.7, 1.1),
           tickfont = f2, dtick = 0.1, ticks = "outside", zeroline = FALSE)

fig3 <- plot_ly(data_q, y = ~Gamma, x = ~Model, type = "box", color = ~Model, colors =  brewer.pal(6, 'Dark2'), boxmean = T)
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig3 <- fig3 %>% layout(annotations = title3)

title4 <-list(text = "d)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.95)
y4 <- list(title = "KGE", titlefont = f, 
           tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE, range = c(0.5, 1))

fig4 <- plot_ly(data_q, y = ~KGE, x = ~Model, type = "box", color = ~Model, colors =  brewer.pal(6, 'Dark2'), boxmean = T)
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig4 <- fig4 %>% layout(annotations = title4)

fig <- subplot(fig1, fig2,  fig3, fig4, nrows = 2, shareX = T, titleY = T, margin = c(0.04, 0.04, 0.02, 0.02))
fig

server <- orca_serve()
server$export(fig, file = "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Figures/Figure9_KGE.png", width = 1200, height = 800, scale = 4)
server$close()