library(tidyverse)
library(caret)
library(kernlab)
install.packages("ROSE")
library(ROSE)

data <- read.csv(file="datos_limpiosFINALE.csv")
summary(data)
str(data)

data <- data[,-c(1,7,6)]

data$Departamento.o.Distrito=fct_collapse(data$Departamento.o.Distrito, Atlántico=c("Atlántico","Barranquilla D.E."))

fct_count(data$Departamento.o.Distrito)
data$Sexo[data$Sexo=="f"]="F"
data$Sexo[data$Sexo=="m"]="M"
#datos<-filter(data,!is.na(data$dias.analizar.prueba==FALSE))
#datos<-filter(data,!is.na(data$dias.enfermo==FALSE))
datos<-filter(data,!is.na(data$dias.antes.notificar==FALSE))
#datos<-filter(data,!is.na(data$dias.desde.primer.caso==FALSE))
datos<-filter(data, data$Departamento.o.Distrito=="Atlántico")

data=datos[,-1]



#Dividir los datos en Train y test set
indexes <- createDataPartition(data$atencion,
                              times = 1,
                              p = 0.5,
                              list = FALSE)

data.train <- data[indexes,]
data.test <- data[-indexes,]
# 
train_control <- trainControl(method="repeatedcv", number=10, repeats=3,classProbs = T)

# Entramos el primer modelo 
svm1 <- train(atencion ~., data = data.train, method = "svmLinear",
              trControl = train_control,  preProcess = c("center","scale"))
#Visualizamos el modelo
svm1

#Colocamos a itere para mirar cual es el mejor C
svm2 <- train(atencion ~.,data=data.train,method="svmLinear",trControl=train_control,
              preProcess = c("center","scale"), tuneGrid = expand.grid(C = seq(0, 2, 
              length = 20)))
#View the model
svm2

#Graficamos el modelo para visualizar accuracy vs Costo
plot(svm2)
svm2$bestTune
svm2$bestTune <- 1.894737


#Corremos el mejor modelo encontrado
svm3 <- train(atencion ~.,data=data.train,method="svmLinear",trControl=train_control,
              preProcess = c("center","scale"), tuneGrid = expand.grid(C=svm2$bestTune))
#View the model
svm3

p <- predict(svm3,data.test,type="raw")
str(respuesta)
confusionMatrix(p,data.test[["atencion"]])



### BALANCEANDO DATOS ########
data1 <- ROSE(atencion~.,data,seed=3)$data

indexes1 <- createDataPartition(data1$atencion,
                               times = 1,
                               p = 0.7,
                               list = FALSE)

data.train1 <- data1[indexes,]
data.test1 <- data1[-indexes,]


svm4 <- train(atencion ~.,data=data.train1,method="svmLinear",trControl=train_control,
              preProcess = c("center","scale"), tuneGrid = expand.grid(C=svm2$bestTune))
#View the model
svm4

p1 <- predict(svm4,data.test1,type="raw")
str(p1)
confusionMatrix(p1,data.test1[["atencion"]])


data2 <- read.csv(file="datos_limpiosFINALE.csv")
remove(data2)

summary(data2)

str(data2)

data2 <- data2[,-c(2,7,8)]

datos2<-filter(data2,!is.na(data2$dias.analizar.prueba==FALSE))

datos2<-filter(data2,!is.na(data2$dias.enfermo==FALSE))

datos2<-filter(data2,!is.na(data2$dias.antes.notificar==FALSE))

datos2<-filter(data2,!is.na(data2$dias.desde.primer.caso==FALSE))

data2=datos2

data2$Sexo[data2$Sexo=="f"]="F"
data2$Sexo[data2$Sexo=="m"]="M"

p2 <- predict(svm4,data2,type="raw")
str(p2)
# confusionMatrix(p2,data2[["atencion"]])









