rm(list=ls())
cat("\014")  

library("readxl")
library("FlowScreen")

q_data<-data.frame(read_xlsx("C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Climate/Q_data.xlsx", sheet = "data", guess_max = 30000))
q_data<-subset(q_data, Date >= "2008-04-15")
q_data<-subset(q_data, select =  c("Date","N04","N08", "N09", "N11"))

options<-c("N04","N08", "N09", "N11")
metrics_deg<-data.frame(matrix(0,1,3))
colnames(metrics_deg)<-c("Metric","ID","P_value")
metrics_agg<-data.frame(matrix(12,1,0))

for(i in 1:4){
q_data_i<-subset(q_data, select =  c("Date", options[i]))
names(q_data_i)[names(q_data_i) == options[i]] <- "Flows"

# Create data.frame for create.ts
q_data_i$ID<-options[i]
q_data_i$Date<-as.Date(q_data_i$Date)
q_data_i$PARAM<-1
q_data_i$SYM<-1
q_data_i$Agency<-"MININCO"
q_data_i<-create.ts(Flows = q_data_i, hyrstart = 4)

#Calculate all metrics and save them
metrics_i<-metrics.all(q_data_i, Season = c(12,1:5))

for(j in 1:30){
  metrics_deg<-rbind(metrics_deg,c(metrics_i$tcpRes[[j]]$MetricName,options[i],metrics_i$tcpRes[[j]]$pval))
  }

summary_i<-screen.cpts(metrics_i, type = "a", text = NULL)
metrics_agg<-cbind(metrics_agg,summary_i)

#Do you want to generate plots?
#screen.cpts(metrics_i, type = "a", text = NULL)
#screen.summary(metrics, type = "h", StnInfo = NULL)
print(i)

}

metrics_deg$P_value[metrics_deg$P_value<=0.01]<-0.01
metrics_deg$P_value[metrics_deg$P_value>0.01 & metrics_deg$P_value<=0.05]<-0.05
metrics_deg$P_value[metrics_deg$P_value>0.05 & metrics_deg$P_value<=0.1]<-0.1
metrics_deg$P_value[metrics_deg$P_value>0.1]<-NA

write.csv(metrics_deg, "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Results/Change_points_metrics_des.csv")
write.csv(metrics_agg, "C:/Users/rooda/Dropbox/Proyectos/Nacimiento FIC/Results/Change_points_metrics.csv")
