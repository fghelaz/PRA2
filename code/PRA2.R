
packages <- c("ggplot2","ggpubr","readr","plotly","tidyverse","lubridate",
              "magrittr","funModeling","skimr","dplyr","nortest","caret",
              "rpart","rpart.plot","pROC","ROCR","performance","see",
              "plotrix", "e1071","interplot","bayestestR","rstanarm","fBasics",
              "glmnet", "rminer","sqldf","reshape")

new <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)

cardio <- read_csv("dataset/stressEcho.csv")

# Clases de las variables y valores de las primeras observaciones
str(cardio)

# Resumen general
skimr::skim(cardio)

# Ver zeros, NA, dtype y unique
funModeling::status(cardio)

#cardio = rename(cardio, c(Parcela="Subparcela")

#cambiamos a factor
cardio$gender <- factor(cardio$gender)


cardio$chestpain <- ifelse(cardio$chestpain == 0, 'Yes', 'Not')
cardio$chestpain <- as.factor(cardio$chestpain)

cardio$restwma <- ifelse(cardio$restwma == 0, 'Yes', 'Not')
cardio$restwma <- as.factor(cardio$restwma)


cardio$posSE <- ifelse(cardio$posSE == 0, 'Yes', 'Not')
cardio$posSE <- as.factor(cardio$posSE)

cardio$newMI <- ifelse(cardio$newMI == 0, 'Yes', 'Not')
cardio$newMI <- as.factor(cardio$newMI)

cardio$newPTCA <- ifelse(cardio$newPTCA == 0, 'Yes', 'Not')
cardio$newPTCA <- as.factor(cardio$newPTCA)

cardio$newCABG <- ifelse(cardio$newCABG == 0, 'Yes', 'Not')
cardio$newCABG <- as.factor(cardio$newCABG)


cardio$death <- ifelse(cardio$death == 0, 'Yes', 'Not')
cardio$death <- as.factor(cardio$death)

cardio$hxofHT <- ifelse(cardio$hxofHT == 0, 'Yes', 'Not')
cardio$hxofHT <- as.factor(cardio$hxofHT)

cardio$hxofDM <- ifelse(cardio$hxofDM == 0, 'Yes', 'Not')
cardio$hxofDM <- as.factor(cardio$hxofDM)

cardio$hxofCig <- factor(cardio$hxofCig)

cardio$hxofMI <- ifelse(cardio$hxofMI == 0, 'Yes', 'Not')
cardio$hxofMI <- as.factor(cardio$hxofMI)

cardio$hxofPTCA <- ifelse(cardio$hxofPTCA == 0, 'Yes', 'Not')
cardio$hxofPTCA <- as.factor(cardio$hxofPTCA)

cardio$hxofCABG <- ifelse(cardio$hxofCABG == 0, 'Yes', 'Not')
cardio$hxofCABG <- as.factor(cardio$hxofCABG)

cardio$any.event <- ifelse(cardio$any.event == 0, 'Yes', 'Not')
cardio$any.event <- as.factor(cardio$any.event)

cardio$ecg <- factor(cardio$ecg)



# Para variables numéricas y no numéricas. Será necesario transformar
# todas las variables en tipo numérico antes
cardio_numeric <- data.frame(lapply(cardio, function(x) as.numeric(x)))
cor(cardio_numeric)


# Establecemos el valor por defecto de alpha

alpha = 0.05
col.names = colnames(cardio)
for (i in 1:ncol(cardio)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n")
  if (is.integer(cardio[,i]) | is.numeric(cardio[,i])) {
    # Como nuestro conjunto de datos es grande, utilizaremos la prueba de
    # Kolmogorov-Smirnov
    p_val = lillie.test(cardio[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      
      # Establecemos cómo queremos ver la salida que muestra el bucle
      if (i < ncol(cardio) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}

#library(plotrix)
shecol<-list( c("red","green","gold"),c("pink","lightblue"),c("blue","gold","green"),
              c("#dddd00","#886600","lightblue"))

data<- cardio %>% dplyr::select(ecg,death,gender,hxofCig)

sizetree(data,main="ECG VS Evento salud VS Sexo, VS Muerte", col=shecol, 
         toplab=c("ECG" ,"Evento", "Sexo" ,"Muerte"))


