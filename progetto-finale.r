################# Descrizione DataSet ################# 

# age: The person’s age in years

# sex: The person’s sex (1 = male, 0 = female)

# cp: chest pain type
#	  - Value 0: asymptomatic
#	  - Value 1: atypical angina
#	  - Value 2: non-anginal pain
#	  - Value 3: typical angina

# trestbps: The person’s resting blood pressure (mm Hg on admission to the hospital)

# chol: The person’s cholesterol measurement in mg/dl

# fbs: The person’s fasting blood sugar (> 120 mg/dl, 1 = true; 0 = false)

# restecg: resting electrocardiographic results
#	  - Value 0: showing probable or definite left ventricular hypertrophy by Estes’ criteria
#	  - Value 1: normal
#	  - Value 2: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)

# thalach: The person’s maximum heart rate achieved

# exang: Exercise induced angina (1 = yes; 0 = no)

# oldpeak: ST depression induced by exercise relative to rest (‘ST’ relates to positions on the ECG plot)

# slope: the slope of the peak exercise ST segment 
#   - Value 0: downsloping
#   - Value 1: flat
#   - Value 2: upsloping

# caa: The number of major vessels
#   - Value 0: No free hearth vessels
#   - Value 1: One free hearth vessel
#   - Value 2: Two free hearth vessels
#   - Value 3: Three free hearth vessels
#   - Value 4: All free hearth vessels

# thal: A blood disorder called Thalassemia
#   - Value 0: NULL (dropped from the dataset previously)
#	  - Value 1: fixed defect (no blood flow in some part of the heart)
#	  - Value 2: normal blood flow
#	  - Value 3: reversible defect (a blood flow is observed but it is not normal)

# target: Heart disease (1 = no, 0= yes)

################# Script ################# 

library(dplyr)
library(GGally)
library(Hmisc)
library(corrplot)
library(glmnet)
library(ggplot2)
library(ggpubr)
library(betareg)

# Set del seme per evitare risultati diversi in run successivi
set.seed(100)

# https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset
path <- paste(getwd(), "/Documenti/GitHub/ms-sl-2022/", "dataset/heart.csv", sep = "", collapse = NULL)
# path<-paste(getwd(), "/dataset/heart.csv", sep = "", collapse = NULL)
DataSet <- read.csv(file = path, sep = ",", header = TRUE)
dim(DataSet)
describe(DataSet)
head(DataSet)

# Confermiamo che non ci sono valori mancanti
sum(is.na(DataSet))

################# Visualizzazione Outliers ################# 

# Osserviamo la presenza di outlier nei valori del colesterolo, nello specifico
# un valore oltre 500.
boxplot(DataSet$chol, main = "cholesterol")

# Osserviamo la presenza di outlier nei valori della pressione saguigna, nello specifico
# si rileva la presenza valori oltre 170.
boxplot(DataSet$trtbps, main = "blood pressure")

# Osserviamo la presenza di un singolo outlier nei valori massimi del battito cardiaco, 
# in particolare un valore minore di 80.
boxplot(DataSet$thalachh,  main = "maximum heart rate achieved")

# Non si rileva la presenza di outlier nei valori dell'età
boxplot(DataSet$age,  main = "age")

################# Rename dei dati ################# 

DataSetForGraph = DataSet%>%rename(
  "chest_pain" = "cp", 
  "cholestrol" = "chol", 
  "exercise_induced_angina" = "exng", 
  "number_of_vessel" = "caa", 
  "blood_pressure" = "trtbps", 
  "fast_blood_sugar" = "fbs", 
  "max_heart_rate_achieved" = "thalachh", 
  "thalassemia" = "thall", 
  "peak_exercice_slope" = "slp", 
  "ecg_result" = "restecg"
)

# Output
DataSetForGraph = DataSetForGraph%>%mutate(output = recode(output, 
                                                        "1" = "Yes", 
                                                        "0" = "No"))

# Sex
DataSetForGraph = DataSetForGraph%>%mutate(sex = recode(sex, 
                                                    "1" = "Male", 
                                                    "0" = "Female"))
# Exercise induced angina
DataSetForGraph = DataSetForGraph%>%mutate(exercise_induced_angina = recode(exercise_induced_angina, 
                                                                        "1" = "yes", 
                                                                        "0" = "No"))
# Chest pain
DataSetForGraph = DataSetForGraph%>%mutate(chest_pain = recode(chest_pain, 
                                                           "0" = "asymptomatic", 
                                                           "1" = "atypical angina", 
                                                           "2" = "non-anginal pain", 
                                                           "3" = "typical angina"))
# Slope of the peak exercise ST segment
DataSetForGraph = DataSetForGraph%>%mutate(peak_exercice_slope = recode(peak_exercice_slope, 
                                                                    "0" = "upsloping", 
                                                                    "1" = "flat", 
                                                                    "2" = "downsloping"))
# Fat blood sugar
DataSetForGraph = DataSetForGraph%>%mutate(fast_blood_sugar = recode(fast_blood_sugar, 
                                                                 "0" = "fasting blood sugar < 120", 
                                                                 "1" = "fasting blood sugar > 120"))
# ECG result
DataSetForGraph = DataSetForGraph%>%mutate(ecg_result = recode(ecg_result, 
                                                           "0" = "left ventricular hypertrophy", 
                                                           "1" = "normal", 
                                                           "2" = "ST-T wave abnormality"))
# Thalassemia (blood disorder)
DataSetForGraph = DataSetForGraph%>%mutate(thalassemia = recode(thalassemia, 
                                                            "1" = "fixed defect", 
                                                            "2" = "normal blood flow", 
                                                            "3" = "reversable defect"))
# Number of free major heart vessels
DataSetForGraph = DataSetForGraph%>%mutate(number_of_vessel = recode(number_of_vessel, 
                                                            "0" = "no free heart vessels", 
                                                            "1" = "one free heart vessel", 
                                                            "2" = "two free heart vessels", 
                                                            "3" = "three free heart vessels", 
                                                            "4" = "all free heart vessels"))

################# Istogrammi ################# 

# Sex histogram
ggplot(data = DataSetForGraph, aes(x = sex, fill = sex)) + 
  geom_bar() + 
  ggtitle("Sex histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

# Age histogram
ggplot(data = DataSetForGraph, aes(x = age, fill = sex)) + 
  geom_histogram(col = "black") + 
  ggtitle("Age  histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

# Chest pain histogram
ggplot(data = DataSetForGraph, aes(x = chest_pain, fill = sex)) + 
  geom_bar() + theme(axis.text = element_text(size = 8)) + 
  ggtitle("Chest pain histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

# Slope of the peak exercise ST segment histogram
ggplot(data = DataSetForGraph, aes(x = peak_exercice_slope, fill = sex)) + 
  geom_bar() + 
  ggtitle("Slope of the peak exercise histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

# Exercise induced angina histogram
ggplot(data = DataSetForGraph, aes(x = exercise_induced_angina, fill = sex)) + 
  geom_bar() + 
  ggtitle("Exercise induced angina histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

# Fast blood sugar histogram
ggplot(data = DataSetForGraph, aes(x = fast_blood_sugar, fill = sex)) + 
  geom_bar() + 
  ggtitle("Fast blood sugar histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

# Thalassemia (blood disorder) histogram
ggplot(data = DataSetForGraph, aes(x = thalassemia, fill = sex)) + 
  geom_bar() + 
  ggtitle("Thalassemia") + 
  theme(plot.title = element_text(hjust = 0.5))

# ECG result histogram
ggplot(data = DataSetForGraph, aes(x = ecg_result, fill = sex)) + 
  geom_bar() + 
  ggtitle("ECG result histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

# Number of free major heart vessels histogram
ggplot(data = DataSetForGraph, aes(x = number_of_vessel, fill = sex)) + 
  geom_bar() + 
  ggtitle("Number of free vessels histogram") + 
  theme(plot.title = element_text(hjust = 0.5))
 
################# Grafici per regressori continui ################# 

# Regression plot for cholesterol and blood pressure
plot_chol_bp = ggplot(data = DataSetForGraph, aes(x = cholestrol, y = blood_pressure, col = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
ggarrange(plot_chol_bp, ncol = 1, nrow = 1) + 
  ggtitle("Regression plot: Cholestrol - Blood Pressure") + 
  theme(plot.title = element_text(hjust = 0.5))

# Regression plot for cholesterol and max heart rate achieved
plot_chol_mhr = ggplot(data = DataSetForGraph, aes(x = cholestrol, y = max_heart_rate_achieved, col = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
ggarrange(plot_chol_mhr, ncol = 1, nrow = 1) + 
  ggtitle("Regression plot: Cholestrol - Max Heart Rate") + 
  theme(plot.title = element_text(hjust = 0.5))

# Regression plot for cholesterol and age
plot_chol_age = ggplot(data = DataSetForGraph, aes(x = cholestrol, y = age, col = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
ggarrange(plot_chol_age, ncol = 1, nrow = 1) + 
  ggtitle("Regression plot: Cholestrol - Age") + 
  theme(plot.title = element_text(hjust = 0.5))

# Density plot of blood pressure
ggplot(DataSetForGraph, aes(x = blood_pressure, fill = sex)) + 
  geom_density() + 
  ggtitle("Blood Pressure") + 
  theme(plot.title = element_text(hjust = 0.5))

# Density plot of cholesterol
ggplot(DataSetForGraph, aes(x = cholestrol, fill = sex)) + 
  geom_density() + 
  ggtitle("Cholesterol Level") + 
  theme(plot.title = element_text(hjust = 0.5))

################# Verifica della presenza di multicollinearità ################# 

# Possiamo osservare che vi è presenza di media multiccolinearità dal momento che vi sono coppie di regressori
# Che presentano una correlazione positiva/negativa.
# A tal proposito è necessario utilizzare delle tecniche di regolarizzazione.
DataSet.cor = cor(DataSet)
corrplot(DataSet.cor)
DataSet.cor
n <- nrow(DataSet)

################# Modello con tutti i regressori ################# 
attach(DataSet)
model <- lm(output~., data = DataSet)
summary(model)

# Output histogram
ggplot(data = DataSetForGraph, aes(x = output, fill = sex)) + 
  geom_bar() + 
  ggtitle("Heart attack") + 
  theme(plot.title = element_text(hjust = 0.5))

################# Applicazione delle tecniche di regolarizzazione con corss-validation ################# 

# discretizzazione di lambda in una sequenza decrescente di valori
qq <- seq(4, -4, length = 150)
griglia = 10^qq

# divisione tra matrice dei regressori e vettore variabile dipendente
n <- nrow(DataSet)
names(DataSet)
xx <- DataSet[, -14]
x <- as.matrix(xx)
dim(x)
names(x)
y <- DataSet$output

################# Ridge regression ################# 

# Di default glmnet() standardizza i regressori
ridgeAllLambda = glmnet (x, y, alpha = 0, lambda = griglia)
coef(ridgeAllLambda)[, 1]

# Grafico andamento dei regressori rispetto al lambda
plot(ridgeAllLambda, main = "Ridge Regression con regressori standardizzati", xvar = "lambda", label = TRUE)

# Applichiamo la K-Fold cross-validation con K = 10 (default)
ridgeKfold10 = cv.glmnet(x, y, lambda = griglia, alpha = 0)
plot(ridgeKfold10, main = "RIDGE: k-fold CV K = 10")

# Estraiamo il lambda minimo a cui corrisponde la minore media deli MSE calcolati sul test-set
ridgeBestLambda <- ridgeKfold10$lambda.min
ridgeBestLambda

# Utilizziamo il lambda migliore per stimare il modello finale
ridgeModBestLambda = glmnet(x, y, alpha = 0, lambda = ridgeBestLambda)
coef(ridgeModBestLambda)[, 1]

################# LASSO ################# 

# Usiamo la funzione glmnet() ponendo alpha = 1 per la regressione LASSO con l’intero dataset
LassoAllLambda = glmnet (x, y, alpha = 1, lambda = griglia)
# È possibile osservare come la tecnica LASSO, anche per valori piccoli di lambda, pone alcuni regressori a zero
plot(LassoAllLambda, main = "LASSO; regressori standardizzati", xvar = "lambda", label = TRUE)
coef(LassoAllLambda)[, 1]
dim(coef(LassoAllLambda))

# Applichiamo la K-Fold cross-validation con K = 10 (default)
lassoKfold10 = cv.glmnet(x, y, lambda = griglia, alpha = 1)
plot(lassoKfold10, main = "LASSO: k-fold CV K = 10")
lassoBestLambda <- lassoKfold10$lambda.min
# Il lambda minimo a cui corrisponde il più piccolo valore del MSE test
lassoBestLambda

# Stima del modello di regressione LASSO con il lambda.min
lassoModBestLambda = glmnet(x, y, alpha = 1, lambda = lassoBestLambda)
coef(lassoModBestLambda)[, 1]

################# Elastic-Net ################# 

ElasticNetAllLambda01 <- glmnet(x, y, lambda = griglia, alpha = .1)
plot(ElasticNetAllLambda01, main = "ELASTIC NET; regressori standardizzati alpha = 0.1", xvar = "lambda", label = TRUE)

ElasticNetAllLambda03 <- glmnet(x, y, lambda = griglia, alpha = .3)
plot(ElasticNetAllLambda03, main = "ELASTIC NET; regressori standardizzati alpha = 0.3", xvar = "lambda", label = TRUE)

ElasticNetAllLambda07 <- glmnet(x, y, lambda = griglia, alpha = .7)
plot(ElasticNetAllLambda07, main = "ELASTIC NET; regressori standardizzati alpha = 0.7", xvar = "lambda", label = TRUE)

ElasticNetAllLambda09 <- glmnet(x, y, lambda = griglia, alpha = .9)
plot(ElasticNetAllLambda09, main = "ELASTIC NET; regressori standardizzati alpha = 0.9", xvar = "lambda", label = TRUE)

# k-fold CROSS VALIDATION per ELASTIC NET
elasticNet01Kfold10 = cv.glmnet(x, y, lambda = griglia, alpha = 0.1)
plot(elasticNet01Kfold10, main = "Elastic Net alpha = 0.1: k-fold CV K = 10")
elasticNet01BestLambda <- elasticNet01Kfold10$lambda.min 
elasticNet01BestLambda
elasticNet01ModBestLambda = glmnet(x, y, alpha = 0.1, lambda = elasticNet01BestLambda)
coef(elasticNet01ModBestLambda)[, 1]

# Con alpha = 0.3
elasticNet03Kfold10 = cv.glmnet(x, y, lambda = griglia, alpha = 0.3)
plot(elasticNet03Kfold10, main = "Elastic Net alpha = 0.3: k-fold CV K = 10")
elasticNet03BestLambda <- elasticNet03Kfold10$lambda.min 
elasticNet03BestLambda
elasticNet03ModBestLambda = glmnet(x, y, alpha = 0.3, lambda = elasticNet03BestLambda)
coef(elasticNet03ModBestLambda)[, 1]

# Con alpha = 0.5
elasticNet05Kfold10 = cv.glmnet(x, y, lambda = griglia, alpha = 0.5)
plot(elasticNet05Kfold10, main = "Elastic Net alpha = 0.5: k-fold CV K = 10")
elasticNet05BestLambda <- elasticNet05Kfold10$lambda.min
elasticNet05BestLambda
elasticNet05ModBestLambda = glmnet(x, y, alpha = 0.5, lambda = elasticNet05BestLambda)
coef(elasticNet05ModBestLambda)[, 1]

# Con alpha = 0.7
elasticNet07Kfold10 = cv.glmnet(x, y, lambda = griglia, alpha = 0.7)
plot(elasticNet07Kfold10, main = "Elastic Net alpha = 0.7: k-fold CV K = 10")
elasticNet07BestLambda <- elasticNet07Kfold10$lambda.min
elasticNet07BestLambda
elasticNet07ModBestLambda = glmnet(x, y, alpha = 0.7, lambda = elasticNet07BestLambda)
coef(elasticNet07ModBestLambda)[, 1]

# Con alpha = 0.9
elasticNet09Kfold10 = cv.glmnet(x, y, lambda = griglia, alpha = 0.9)
plot(elasticNet09Kfold10, main = "Elastic Net alpha = 0.9: k-fold CV K = 10")
elasticNet09BestLambda <- elasticNet09Kfold10$lambda.min
elasticNet09BestLambda
elasticNet09ModBestLambda = glmnet(x, y, alpha = 0.9, lambda = elasticNet09BestLambda)
coef(elasticNet09ModBestLambda)[, 1]

################# Confronto fra Ridge, Lasso ed Elastic-Net #################

# Confronto delle stime dei parametri dei modelli con valore di lambda migliore 
# (corrispondente al minor valore dell'MSE di test)
cbind(coef(ridgeModBestLambda)[, 1], coef(lassoModBestLambda)[, 1], 
      coef(elasticNet01ModBestLambda)[, 1], coef(elasticNet03ModBestLambda)[, 1], 
      coef(elasticNet05ModBestLambda)[, 1], coef(elasticNet07ModBestLambda)[, 1], 
      coef(elasticNet09ModBestLambda)[, 1])

# SCELTA del modello sulla base del minor MSE test
# Per confrontare i modelli bisogna estrarre MSE più piccolo a cui corrisponde il lambda minimo
mseMinRR <- ridgeKfold10$cvm[ridgeKfold10$lambda == ridgeKfold10$lambda.min]
mseMinLASSO <- lassoKfold10$cvm[lassoKfold10$lambda == lassoKfold10$lambda.min]
mseMinEN01 <- elasticNet01Kfold10$cvm[elasticNet01Kfold10$lambda == elasticNet01Kfold10$lambda.min]
mseMinEN03 <- elasticNet03Kfold10$cvm[elasticNet03Kfold10$lambda == elasticNet03Kfold10$lambda.min]
mseMinEN05 <- elasticNet05Kfold10$cvm[elasticNet05Kfold10$lambda == elasticNet05Kfold10$lambda.min]
mseMinEN07 <- elasticNet07Kfold10$cvm[elasticNet07Kfold10$lambda == elasticNet07Kfold10$lambda.min]
mseMinEN09 <- elasticNet09Kfold10$cvm[elasticNet09Kfold10$lambda == elasticNet09Kfold10$lambda.min]
vettMSE <- cbind(mseMinLASSO, mseMinRR, mseMinEN01, mseMinEN03, mseMinEN05, mseMinEN07, mseMinEN09)
vettMSE
minMSE <- min(vettMSE)
minMSE

################# Previsione della variabile dipendete per alcuni pazienti di esempio #################

if(minMSE == vettMSE[, 1]){
  bestPredictionModel <- ridgeModBestLambda
}
if(minMSE == vettMSE[, 2]){
  bestPredictionModel <- lassoModBestLambda
}
if(minMSE == vettMSE[, 3]){
  bestPredictionModel <- elasticNet01ModBestLambda
}
if(minMSE == vettMSE[, 4]){
  bestPredictionModel <- elasticNet03ModBestLambda
}
if(minMSE == vettMSE[, 5]){
  bestPredictionModel <- elasticNet05ModBestLambda
}
if(minMSE == vettMSE[, 6]){
  bestPredictionModel <- elasticNet07ModBestLambda
}
if(minMSE == vettMSE[, 7]){
  bestPredictionModel <- elasticNet09ModBestLambda
}

# Person 1
# Age: 20
# Sex: Male (1)
# Chest pain: Asymptomatic (0)
# Blood pressure: 120
# Cholesterol: 80
# Fasting blood sugar: <120 (0)
# ECG result: Normal (1)
# Maximum heart rate achieved: 130
# Exercise induced angina: No (0)
# ST depression induced by exercise relative to rest: No (0)
# Slope of the peak exercise ST segment: Downsloping (0)
# Number of major vessels: All free (4)
# Thalassemia: Normal blood flow (2)
male20data <- c(20, 1, 0, 120, 80, 0, 1, 130, 0, 0, 0, 4, 2)
male20output <- predict(bestPredictionModel, male20data, type = "response")
male20output

# Person 2
# Age: 50
# Sex: Male (1)
# Chest pain: Atypical angina (1)
# Blood pressure: 170
# Cholesterol: 200
# Fasting blood sugar: >120 (1)
# ECG result: Having ST-T wave abnormality (2)
# Maximum heart rate achieved: 180
# Exercise induced angina: Yes (1)
# ST depression induced by exercise relative to rest: No (0)
# Slope of the peak exercise ST segment: Upsloping (2)
# Number of major vessels: Two free (2)
# Thalassemia: Fixed defect (1)
male50data <- c(50, 1, 1, 170, 200, 1, 2, 180, 1, 0, 2, 1, 1)
male50output <- predict(bestPredictionModel, male50data, type = "response")
male50output

# Person 3
# Age: 70
# Sex: Male (1)
# Chest pain: Typical angina (3)
# Blood pressure: 170
# Cholesterol: 230
# Fasting blood sugar: >120 (1)
# ECG result: Having ST-T wave abnormality (2)
# Maximum heart rate achieved: 160
# Exercise induced angina: Yes (1)
# ST depression induced by exercise relative to rest: No (0)
# Slope of the peak exercise ST segment: Upsloping (2)
# Number of major vessels: No free vessels (0)
# Thalassemia: Fixed defect (1)
male70data <- c(70, 1, 3, 170, 230, 1, 2, 160, 1, 0, 2, 0, 1)
male70output <-predict(bestPredictionModel, male70data, type = "response")
male70output


################# Appendice 1 - Algorimo di ML #################

#Standardizzazione

#la standardizzazione non è necessaria per i regressori con valori caratterizzati da una scala limitata
x_toscale <- x[, c("age", "trtbps", "chol", "thalachh", "oldpeak")] #da standardizzare
dropped <- x[,  c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")] #regressori da non standardizzare

head(x_toscale)

scaled_regressors <- scale(x_toscale) #standardizziamo

head(scaled_regressors)

cleaned_regressors <- cbind(scaled_regressors, dropped, label = y) #dataset completo standardizzato

head(cleaned_regressors)

#divisione in training set e test set
index <- sample(1:nrow(cleaned_regressors), 0.7*nrow(cleaned_regressors))
training_set <- cleaned_regressors[index, ]
y_training <- training_set[, "label"] #var dipendente training
test_set <- cleaned_regressors[-index, ]
y_test <- test_set[, "label"] #var dipendente test
test_set <- test_set[, -14] #eliminiamo la variabile dipendente dal test set 

head(test_set)

dim(training_set)
dim(test_set)

### WORKING IN PROGRESS... Daje! ###

################# Appendice 2 - La regressione Beta #################

################# Descrizione DataSet ################# 

# Serial.no: row number

# GRE.Score: University rating according to GRE program, integer (out of 340)

# TOEFL.Score: TOEFL student english exam points, integer (out of 120)

# Univeristy.Rating: University rating, integer (out of 5) 

# SOP: Statement of Purpose, continuous (out of 5)

# LOR: Letter of Recommendation Strength, continuous (out of 5)

# CGPA: Cumulative Grade Point Average, student overall academic performance, continuous (out of 10)

# Research: Student research experience, binary (either 0 or 1)

# Chance.of.Admit: Probability of student admission to the college

# https://www.kaggle.com/datasets/mohansacharya/graduate-admissions
path2 <- paste(getwd(), "/Documenti/GitHub/ms-sl-2022/", "dataset/Admission_Predict.csv", sep = "", collapse = NULL)
dataSetBeta <- read.csv(file = path2, sep = ",", header = TRUE)
attach(dataSetBeta)
dim(dataSetBeta)
describe(dataSetBeta)
head(dataSetBeta)

# Confermiamo che non ci sono valori mancanti
sum(is.na(dataSetBeta))

################# Grafici esplorativi #################

dataSetBetaForGraph = dataSetBeta%>%mutate(Research = recode(Research, "0" = "No", "1" = "Yes"))

# Istogramma ricerca si/no
ggplot(dataSetBetaForGraph, aes(x = Research, fill = Research)) + geom_bar() + ggtitle("Research histogram") + theme(plot.title = element_text(hjust = 0.5))

# Istogramma rating universitario
ggplot(dataSetBetaForGraph, aes(x = University.Rating, fill = Research)) + geom_bar() + ggtitle("University.Rating histogram") + theme(plot.title = element_text(hjust = 0.5))

# Desnità score GRE
ggplot(dataSetBetaForGraph, aes(x = GRE.Score, fill = Research)) + geom_density() + ggtitle("GRE Indicator density") + theme(plot.title = element_text(hjust = 0.5))

# Desnità score esame TOEFL
ggplot(dataSetBetaForGraph, aes(x = TOEFL.Score, fill = Research)) + geom_density() + ggtitle("TOEFL.Score Indicator density") + theme(plot.title = element_text(hjust = 0.5))

# Desnità indicatore CGPA, overall academic performance of a student
ggplot(dataSetBetaForGraph, aes(x = CGPA, fill = Research)) + geom_density() + ggtitle("CGPA Indicator density") + theme(plot.title = element_text(hjust = 0.5))

################# Stima di un modello di regressione Beta #################

# Modello con tutti i regressori 
betaModel <- betareg(formula = Chance.of.Admit ~ GRE.Score+TOEFL.Score+University.Rating+SOP+LOR+CGPA+Research, data = dataSetBeta)
summary(betaModel)# Pseudo R-squared: 0.8276

# Eliminiamo i regressori University.Rating e SOP doto che sono meno statisticamente significativi
betaModel2 <- betareg(formula = Chance.of.Admit~GRE.Score+TOEFL.Score+LOR+CGPA+Research, data = dataSetBeta)
summary(betaModel2) # Pseudo R-squared: 0.8253














