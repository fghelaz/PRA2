
packages <- c("ggplot2","ggpubr","readr","plotly","tidyverse","lubridate",
              "magrittr","funModeling","skimr","dplyr","nortest","caret",
              "rpart","rpart.plot","pROC","ROCR","performance","see",
              "plotrix", "e1071","interplot","bayestestR","rstanarm","fBasics",
              "glmnet", "rminer","sqldf")

new <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)

stressEcho <- read_csv("dataset/stressEcho.csv")

plot(stressEcho)

stressEcho$chestpain <- ifelse(stressEcho$chestpain == 0, 'No', 'Sí')
stressEcho$chestpain <- as.factor(stressEcho$chestpain)

stressEcho$hxofCig <- ifelse(stressEcho$chestpain == 0, 'No', 'Sí')
stressEcho$hxofCig <- as.factor(stressEcho$chestpain)

sqldf(x = "SELECT gender AS Genero, hxofCig AS Condición_de_fumador, COUNT(*) AS Total FROM stressEcho GROUP BY gender, hxofCig ORDER BY gender, - hxofCig")


percentData <- stressEcho %>%
  group_by(ecg, hxofCig) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(percentData, aes(x = ecg, y = round(100*freq, 2), fill = hxofCig)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(100*freq, 2), "%"), y = round(100*freq, 2)), 
            position = position_stack(vjust = 0.5)) +
  xlab("Diagnóstico ecocardiograma") +
  ylab("Porcentaje de la condición de fumador por grupos de diagnóstico") +
  labs(fill = "Condición de fumador") +
  ggtitle("Relación entre diagnóstico ecocardiograma y condición de fumador")