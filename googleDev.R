setwd(paste0(getwd(), "/RDatelligence/SATURNO"))


library(ggplot2)
library(dplyr)


SATURNO <- read.csv("SATURNO.csv")


filter1 <- group_by(SATURNO, dia_semana, fecha)
initData <- summarise(filter1, count = n())
filter2 <- group_by(initData, dia_semana)
initData2 <- summarise(filter2, 
                       min = min(count), 
                       max = max(count), 
                       Q1 = quantile(count, probs=0.25),
                       Q2 = quantile(count, probs=0.5),
                       Q3 = quantile(count, probs=0.75),
                       media = mean(count), 
                       sd = sd(count),
                       cv = paste0(round((sd*100/media), 1), "%"))


grafica <- ggplot(initData, aes(x = dia_semana, y = count, fill = dia_semana)) + 
           geom_boxplot() + 
           labs(x = "Dia de la semana", y = "Conteo") + 
           theme(legend.position="none"); grafica