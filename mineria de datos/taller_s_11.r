#BBDD IRIS
head(iris)
dim(iris)
## [1] 150   5
unique(iris$Species)
irisScale = scale(iris[,-5])

# determinar numero Optimo k (cluster)
#Elbow
fviz_nbclust(iris[,-5], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)

# Average silhouette
fviz_nbclust(iris[,-5], kmeans, method = "silhouette")
### Gap statistic
fviz_nbclust(irisScale, kmeans, method = "gap_stat")
## MODELO KMEANS
fitK <- kmeans(iris[,-5], 3) ## Genera Moldelo ML no supervisado Kmeans/Identificar clases de las especies con valores numericos para un K=3
iris$cluster<-fitK$cluster ## agrega la columna cluster a la BBDD IRIS
fviz_cluster(fitK, data = iris[,-c(5,6)],geom = c("point")) ## dibuja los clusters indentificados para K=3
##Seleccionar variables - columnas, para analisis descriptivo de las variable de entrada
bbdd<-select(iris,1:4) 

# ESTADISTICA DESCRIPTIVA PARA VARIABLES DE ENTRADA
summary(bbdd) ## entrega descripcion estadistica de las variables de entrada
##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500

boxplot(bbdd,col = rainbow(ncol(trees)),las = 2, xlab = "", ylab = "") ## dibuja summary junto a outlayers para variables de entrada

#CORRELACION ML no supervisado, solo variables de entrada
cor(bbdd) ## entrega correlacion de las variables de entrada
##              Sepal.Length Sepal.Width Petal.Length Petal.Width
## Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
## Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
## Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
## Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000
Tabla_Corr <- chart.Correlation(bbdd,method = "pearson", plot=FALSE) ## dibuja correlacion de las variables de entrada

# ESTADISTICA DESCRIPTIVA PARA VARIABLES DE ENTRADA Y VARIABLE DE DECISION (VARIABLE DE SALIDA)
bbdd1 <- select(iris,6) ##Seleccionar variable - columnas, para analisis descriptivo de la variable de decision
summary(bbdd1) ## entrega descripcion estadistica de la variable de decision
##     cluster   
##  Min.   :1.0  
##  1st Qu.:2.0  
##  Median :3.0  
##  Mean   :2.5  
##  3rd Qu.:3.0  
##  Max.   :3.0

#CORRELACION ML supervisado varables de entrada respecto a variable de decision
bbdd2 <- select(iris,1:4,6) ##Seleccionar variable - columnas, para analisis descriptivo de la variable de decision y variables de entrada
cor(bbdd2) ## entrega correlacion de las variables de entrada y valiable de decision
##              Sepal.Length Sepal.Width Petal.Length Petal.Width    cluster
## Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411  0.7595069
## Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259 -0.2592837
## Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654  0.8226209
## Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000  0.7973752
## cluster         0.7595069  -0.2592837    0.8226209   0.7973752  1.0000000
Tabla_Corr <- chart.Correlation(bbdd2,method = "pearson", plot=FALSE) ## dibuja correlacion de las variables de entrada y variable de decision

##MACHINE LEARNIN NO SUPERVISADO

## PREDICCIONES / ACCURACY
table(Obs =iris$Species,Pred=iris$cluster) ## Predicciones, se puede sacar accuracy
##             Pred
## Obs           1  2  3
##   setosa     17 33  0
##   versicolor  4  0 46
##   virginica   0  0 50
iris$cluster<-factor(iris$cluster) ## tabla visual de las predicciones
iris$clus<-mapvalues(iris$cluster,c("1","2","3"),c("setosa","versicolor" ,"virginica")) ##Categoriza variables de decision str 
caret::confusionMatrix(iris$Species,iris$clus) ## entrega resumen del modelado, matriz de confusi?n y accuracy


## Confusion Matrix and Statistics
## 
##             Reference
## Prediction   setosa versicolor virginica
##   setosa         17         33         0
##   versicolor      4          0        46
##   virginica       0          0        50
## 
## Overall Statistics
##                Accuracy : 0.4467          
##                  95% CI : (0.3655, 0.5299)

Indicadores_KMEANS <- confusionMatrix(iris$Species,iris$clus)
Indicadores_KMEANS
## Confusion Matrix and Statistics
## 
##             Reference
## Prediction   setosa versicolor virginica
##   setosa         17         33         0
##   versicolor      4          0        46
##   virginica       0          0        50
## 
## Overall Statistics
##                Accuracy : 0.4467          
##                  95% CI : (0.3655, 0.5299)


##MACHINE LEARNING SUPERVISADO
bbdd3 <- select(iris,1:4,7) ##Seleccionar variable - columnas, para analisis de clasificacion/prediccion, donde se inlcuye columna 7 que est? como factor

##Entrenamiento y prueba 
set.seed(123)
training.samples <- bbdd3$clus %>% 
  createDataPartition(p = 0.8, list = FALSE)
ta.train <- bbdd3[training.samples, ]
ta.test <- bbdd3[-training.samples, ]

####### Modelo KNN ######

##Modelo KNN con datos de entrenamiento de tasa de aprobacion
knn_ta<-train.kknn(`clus` ~ .,data=ta.train,kmax=10)

##Prediccion del modelo KNN con datos de prueba de tasa de aprobacion
pred_knn_ta <-predict(knn_ta, newdata = ta.test)
pred_knn_ta
##  [1] versicolor versicolor versicolor setosa     versicolor versicolor
##  [7] versicolor setosa     setosa     virginica  virginica  virginica 
## [13] virginica  virginica  virginica  virginica  setosa     virginica 
## [19] virginica  virginica  virginica  virginica  virginica  virginica 
## [25] virginica  virginica  virginica  virginica  virginica 
## Levels: setosa versicolor virginica

Indicadores_KNN<-confusionMatrix(ta.test$`clus`,pred_knn_ta)
Indicadores_KNN
## Confusion Matrix and Statistics
## 
##             Reference
## Prediction   setosa versicolor virginica
##   setosa          4          0         0
##   versicolor      0          6         0
##   virginica       0          0        19
## 
## Overall Statistics
##                Accuracy : 1          
##                  95% CI : (0.8806, 1)
## 
## Statistics by Class:
## 
##                      Class: setosa Class: versicolor Class: virginica
## Sensitivity                 1.0000            1.0000           1.0000
## Specificity                 1.0000            1.0000           1.0000
## Pos Pred Value              1.0000            1.0000           1.0000
## Neg Pred Value              1.0000            1.0000           1.0000
## Prevalence                  0.1379            0.2069           0.6552
## Detection Rate              0.1379            0.2069           0.6552
## Detection Prevalence        0.1379            0.2069           0.6552
## Balanced Accuracy           1.0000            1.0000           1.0000

##Matriz de confusion del modelo KNN con datos de validacion tasa de aprobacion
matriz_confusion_ta_knn <- table(ta.test$`clus`, pred_knn_ta)

####### Modelo SVM ######

##Modelo SVM con datos de entrenamiento de tasa de aprobacion
svm_ta<-svm(`clus`~.,data=ta.train,kernel="radial",type = "C-classification")

##Prediccion del modelo SVM con datos de prueba de tasa de aprobacion
pred_svm_ta <-predict(svm_ta, newdata = ta.test)

Indicadores_SVM<-confusionMatrix(ta.test$`clus`,pred_svm_ta)
Indicadores_SVM
## Confusion Matrix and Statistics
## 
##             Reference
## Prediction   setosa versicolor virginica
##   setosa          3          0         1
##   versicolor      1          5         0
##   virginica       0          0        19
## 
## Overall Statistics                                 
##                Accuracy : 0.931           
##                  95% CI : (0.7723, 0.9915)



##Matriz de confusion del modelo SVM con datos de validacion tasa de aprobacion
matriz_confusion_ta_svm <- table(ta.test$`clus`, pred_svm_ta)


####### Modelo NB ######
##Modelo NB con datos de entrenamiento de tasa de aprobacion
nb_ta<-naiveBayes(`clus`  ~ ., data = ta.train)
pred_nb_ta <-predict(nb_ta, newdata = ta.test)
####### Modelo RF ######
##Modelo RF con datos de entrenamiento de tasa de aprobacion
rf_ta<-randomForest(`clus` ~ .,data = ta.train,ntree = 250, na.action = na.roughfix)
pred_rf_ta <-predict(rf_ta, newdata = ta.test)

# COMPARACION ACCURACY

Resumen<-data.frame(KMEANS=Indicadores_KMEANS$overall,SVM=Indicadores_SVM$overall,KNN=Indicadores_KNN$overall,
                    NB=Indicadores_NB$overall,DT=Indicadores_DT$overall,RF=Indicadores_RF$overall)
round(Resumen*100,2)
##                KMEANS   SVM    KNN    NB    RF
## Accuracy        44.67 93.10 100.00 93.10 96.55
## Kappa           17.00 86.02 100.00 86.45 93.26
## AccuracyLower   36.55 77.23  88.06 77.23 82.24
## AccuracyUpper   52.99 99.15 100.00 99.15 99.91
## AccuracyNull    64.00 68.97  65.52 65.52 65.52
## AccuracyPValue 100.00  0.20   0.00  0.06  0.01
## McnemarPValue     NaN   NaN    NaN   NaN   NaN

