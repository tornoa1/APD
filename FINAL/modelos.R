#------------------------------------------------------------------#
#                               LIBRARIAS                          #
#------------------------------------------------------------------#
library(ggplot2)
library(reshape)
library(ggpubr)
library(reshape2)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(purrr)
library(precrec)
library(ROSE)
library(ROCR)
library(randomForest)
library(ada)
#------------------------------------------------------------------#
#                       CARGANDO LA BASE DE DATOS                  #
#------------------------------------------------------------------#

#Si tiene 0 y 1, entonces es convertir en factor (clasificacion)

Datos<-read.delim("clipboard")
head(Datos,10)
Datos$Spam <- factor(Datos$Spam) #porque tiene 0 y 1

#------------------------------------------------------------------#
#               DATOS DE ENTRENAMIENTO Y DE PRUEBA                 #
#------------------------------------------------------------------#
RNGkind(sample.kind = "Rounding")
set.seed(100)
indice<- createDataPartition(Datos$Spam, p=0.7, list=FALSE)
data.train <- Datos[ indice, ]
#data.train <- read.delim("clipboard")
data.test  <- Datos[-indice, ]
#data.test  <- read.delim("clipboard")
dim(data.train)
#El número de filas de la data de prueba 
dim(data.test)

#------------------------------------------------------------------#
#                      EL ALGORITMO ADABOOST                       #
#------------------------------------------------------------------#
set.seed(200)
ctrl <- trainControl(method="cv", number=3)

modelo_boos <- train(Spam ~ ., 
                     data = data.train, 
                     method = "ada", tuneLength=6,
                     trControl=ctrl, metric="Accuracy")

plot(modelo_boos)
modelo_boos$bestTune
pred_boos <- predict(modelo_boos, newdata=data.test[,-1],type="raw")
confusionMatrix(data= pred_boos, reference= factor(data.test$Spam), positive="Si")

# Predicción correcta y error
# ---------------------------
accuracy <- mean(data.test$Spam==pred_boos) ; accuracy
error <- mean(data.test$Spam!=pred_boos) ; error

# Curva ROC
# -----------
pred_boos2 <- predict(modelo_boos, newdata=data.test[,-1],type="prob")
roc.curve(data.test$Spam, pred_boos2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

# Importancia de Variables
# -------------------------
impor=varImp(modelo_boos)
impor
plot(impor)

#------------------------------------------------------------------#
#                      EL ALGORITMO CART                           #
#------------------------------------------------------------------#

# Aplicando el modelo con Validación Cruzada Repetida 
set.seed(200)
ctrl <- trainControl(method="cv", number=10)
# classProbs=TRUE,summaryFunction = twoClassSummary)
modelo_cart <- train(Spam ~ ., 
                     data = data.train, 
                     method = "rpart", 
                     trControl = ctrl, 
                     tuneGrid = expand.grid(cp=seq(0,0.5,0.001)),
                     metric="Accuracy" )

rpart.plot(modelo_cart$finalModel, roundint=FALSE,digits=-3, type=1, extra=101,cex = .7, nn=TRUE)

pred_cart <- predict(modelo_cart, newdata=data.test[,-1],type="raw")
confusionMatrix(data= pred_cart, reference= as.factor(data.test$Spam), positive="Si")

# Curva ROC
# -----------
pred_cart2 <- predict(modelo_cart, newdata=data.test[,-1],type="prob")
roc.curve(data.test$Spam, pred_cart2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

# Importancia de Variables
# -------------------------
impor=varImp(modelo_cart)
impor
plot(impor)
#------------------------------------------------------------------#
#                      EL ALGORITMO BAGGING                        #
#------------------------------------------------------------------#
RNGkind(sample.kind = "Rounding")
set.seed(100)
ctrl <- trainControl(method="cv",number=10)
modelo_bag <- train(Spam ~ ., 
                    data = data.train, 
                    method = "treebag",
                    trControl = ctrl, 
                    tuneLength = 5, 
                    metric="Accuracy")

modelo_bag
#El accuracy que se obtuvo en la validación cruzada con el mdelo Bagging
0.8866667

pred_bag <- predict(modelo_bag, newdata=data.test[,-1],type="raw")
confusionMatrix(data= pred_bag, reference= as.factor(data.test$Spam), positive="1")

#La tasa (proporción) de error de del modelo Bagging
# 1 - accuracy
1 - 0.9024

#Usando el modelo Bagging, la probabilidad de clasificar 
#correctamente a un correo como SPAM
# es la sensibilidad, osea, 0.8571


# Curva ROC
# -----------
pred_bag2 <- predict(modelo_bag, newdata=data.test[,-1],type="prob")
roc.curve(data.test$Spam, pred_bag2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

# Importancia de Variables
# -------------------------
#cual es la variable mas importante
impor=varImp(modelo_bag)
impor
plot(impor)


#------------------------------------------------------------------#
#                   EL ALGORITMO RANDOM FOREST                     #
#------------------------------------------------------------------#
#la proporción de correos que fueron predichos con el modelo como 
#SPAM cuando estos realmente no son SPAM

#0 26 (verdadero negativo)  2 (falso negativo)
#1  1 (falso positivo) 12 (verdadero postivo)
#1 / (26 + 2 + 12)

RNGkind(sample.kind = "Rounding")
set.seed(100)
modelLookup(model='rf')
ctrl <- trainControl(method="cv",number=10)

modelo_rf <- train(Spam ~ ., 
                   data = data.train, 
                   method = "rf", 
                   trControl = ctrl, 
                   tuneLength = 5,
                   metric="Accuracy")

pred_rf <- predict(modelo_rf, newdata=data.test[,-1],type="raw")
confusionMatrix(data= pred_rf, reference= as.factor(data.test$Spam), positive="1")

#La tasa (proporción) de predicción correcta de RANDOM FOREST
#el valor es el accuracy, osea 0.9268

#Usando el modelo de RANDOM FOREST, la probabilidad estimada de la 
#tercera observación de la data test
data.test
predict(modelo_rf, data.frame(Archivos=3, Palabras=68, Password=0, Dólar=1),type = "prob")

#Si se considerarse el mejor modelo a aquel que tenga mayor accuracy, y 
#se deseare predecir si un correo es SPAM, cuando el Número de archivos adjuntos
#que posee el mensaje es 2, el número de palabras que posee el mensaje es 15, 
#el número de veces que aparece la palabra "Password" en el mensaje es 2, 
#el número de veces que aparece símbolo del dólar ($) es 4 veces . 
#Entonces: La probabilidad de que el mensaje sea SPAM equivale a
predict(modelo_rf, data.frame(Archivos=2, Palabras=15, Password=2, Dólar=4),type = "prob")
0.422

# Curva ROC
# -----------
pred_rf2 <- predict(modelo_rf, newdata=data.test[,-1],type="prob")
roc.curve(data.test$Spam, pred_rf2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

# Importancia de Variables
# -------------------------
impor=varImp(modelo_rf)
impor
plot(impor)

# Comparando el entrenamiento en los modelos 
modelos  <- list(#CART          = modelo_cart,
                 Bagging       = modelo_bag,
                 Random_Forest = modelo_rf)
                 #AdaBoosting   = modelo_boos)

comparacion_modelos <- resamples(modelos) 
Resumen<-summary(comparacion_modelos)
Resumen
bwplot(comparacion_modelos)
dotplot(comparacion_modelos)
densityplot(comparacion_modelos, metric = "Accuracy",auto.key=TRUE)

#mayor variabilidad o mejore dispersion se determina por el tamaño de la caja
#si es mas pequeña, en este caso es el random forest

# correlation between results
modelCor(comparacion_modelos)
splom(comparacion_modelos)


#Curvas ROC
# Formato 1
# -----------

modelos  <- list(CART          = modelo_cart,
                 Bagging       = modelo_bag,
                 Random_Forest = modelo_rf,
                 AdaBoosting   = modelo_boos)

CURVES_ROC_PRC <- function(modelos, datos.test, type) {
  modelos=modelos
  datos.test=data.test
  type="ROC"
  test_prob <- function(model, data) {
    list(1-data.frame(d=predict(model, data, type = "prob"))[1])
  }
  prob_list <- modelos %>% map(test_prob, data = datos.test)
  mscurves <-evalmod(mmdata(prob_list,datos.test[,1],modnames=names(prob_list)))
  aucs <- data.frame(subset(precrec::auc(mscurves), curvetypes==type)[,-c(2:3)])
  names(aucs) <- c("Modelos", "ROC-auc")
  aucs <- aucs[order(aucs$`ROC-auc`, decreasing = T),]
  stable.p <- ggtexttable(aucs,theme = ttheme("classic"), rows = NULL)
  curv <- autoplot(mscurves,curvetype = type) +
    ggtitle(paste("Curvas ",type,"(AUC) para los modelos")) +
    geom_line(size = 0.8) +
    theme(legend.position="top", plot.title = element_text(hjust = 0.5)) +
    #if(type == "ROC") {
    annotation_custom(ggplotGrob(stable.p), xmin = 0.7, ymin = 0.1,
                      ymax = 0.4,xmax = 0.9) +
    geom_abline(intercept = 0, slope = 1, color = "black", size = 0.7,
                linetype = 2) 
  #} else {annotation_custom(ggplotGrob(stable.p), 
  #xmin = 0.4, ymin = 0.05, ymax = 0.4, xmax = 0)} 
  return(c(curvas=list(curv), auc=list(aucs)))
}
library(conflicted)
conflict_prefer("autoplot", "ggplot2")
res <- CURVES_ROC_PRC(modelos, data.test, type="ROC")
res$curvas


# Comparación
# -----------
test_AC<- function(model, data) {
  mean(predict(model, data, type = "raw")== data[,1])}
model_list_AC<- modelos %>% map(test_AC, data = data.test)
l<-data.frame(reshape2::melt(model_list_AC)[,c(2,1)])
Acur<-cbind(data.frame(Resumen$statistics$Accuracy)[,4],
            Resumen$statistics$Kappa[,4])
comparación<-data.frame(l,Acur)
names(comparación)<-c("Modelos","Accuracy","Accuracy CV","Kappa CV")

comparación=merge(comparación,res$auc, by.x = 'Modelos', by.y='Modelos')
comparación
d=reshape2::melt(comparación)
ggplot(data=d, aes(x=reorder(Modelos,value), y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=round(value,3)), vjust=0.5, hjust=1.5,color="white",
            position = position_dodge(0.9), size=3.2)+
  scale_fill_brewer(palette="Paired")+ coord_flip()+
  labs(title="Comparación de  diferentes métricas")+xlab("Modelos")+ylab("Métricas")+
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))



predict(modelo_boos, data.frame(Sexo="M",
                                E.Civil="Casado",	Educacion="Sup.Incomp",
                                Prioridad="Si",	Lic.Conducir="Si",Edad=40,
                                Tarjetas=3,	Deuda=0,	Saldo=5000,
                                CrediScore=0.50,	Años_empleo=5,
                                Ingresos=4500),type="prob")
