head(p1)
p2 <- as.data.frame(p1)
p1 <- predict(svm4,data.test,type="prob")
head(p1)
result_cv<-as.data.frame(predict(svm4,data.test,type="prob"))   

#diseño de la funcion logloss
LogLoss<-function(act, pred)
{
  eps = 1e-15;
  nr = length(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(length(act))
  return(ll);
}

#Pasamos a variable binaria
at <- fct_collapse(data1$atencion,
                   "0"=c("Fallecido"),
                   "1"=c("Recuperado"))
at <- fct_collapse(data.test$atencion,
                   "0"=c("Fallecido"),
                   "1"=c("Recuperado"))

data2 <- data.frame(at,data1)
data.test2 <- data.frame(at,data.test)

colnames(result_cv)<-c("1","0")
LogLoss(as.numeric(as.character(data.test2$at)),result_cv$`1`)

# performing platt scaling on the dataset
dataframe<-data.frame(result_cv$`1`,data.test2$at)
colnames(dataframe)<-c("x","y")

model_log<-glm(y~x,data = dataframe,family = binomial)

result_cv_platt<-predict(model_log,dataframe[-2],type = "response")

result_cv_platt <- as.data.frame(result_cv_platt)
head(result_cv_platt)
LogLoss(as.numeric(as.character(data.test2$at)),result_cv_platt)

anyNA(result_cv)


#Graficamos para comparar
library(SpecsVerification)
# The line below computes the reliability plot data for cross validation dataset without platt scaling
k <-ReliabilityDiagram(result_cv$`1`,as.numeric(as.character(data.test2$at)),bins = 10,plot = T)
k <-ReliabilityDiagram(result_cv_platt$result_cv_platt,as.numeric(as.character(data.test2$at)),bins = 10,plot = T)

head(result_cv_platt)

df <- as.data.frame(result_cv_platt)
df1 <- mutate(df,intervalo_inf =df$result_cv_platt-1.96*(sd(df$result_cv_platt)/sqrt(length(df$result_cv_platt))))
head(df1)
df2 <- mutate(df1,intervalo_sup =df$result_cv_platt+1.96*(sd(df$result_cv_platt)/sqrt(length(df$result_cv_platt))))

View(df2)

write.csv(df2,file="intervalos.csv")

