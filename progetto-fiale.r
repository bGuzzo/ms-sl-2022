################# Note & Open Point ################# 

# Nota: per semplicità non cotrolliamo la presenza di outlier in tutte le colonne
# Open Point: controllare outliers su tutti i regressori? 
# Open Point: Inserire istogramma dei regressori? 
# Open Point: fare il confronto tra RR e Lasso?
# Open Point: omettere i grafici sull'MSE in funzione della complessità e altre porve fatte dal prof (e da noi nell secondo progetto)?

################# Script ################# 

library(dplyr)
library(GGally)
library(Hmisc)
library(corrplot)
library(glmnet)

# Set del seme per evitare risultati diversi in run successivi
set.seed(100)

# https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset
WW<-read.csv(file.choose(), sep=",", header=TRUE)
dim(WW)
describe(WW)
head(WW)

# Confermiamo che non ci sono valori mancanti
sum(is.na(WW))

################# Visualizzazione Outliers ################# 

# Osserviamo la presenza di outlier nei valori del colesterolo, nello specifico
# un valore oltre 500.
boxplot(WW$chol, main = "cholesterol")

# Osserviamo la presenza di outlier nei valori della pressione saguigna, nello specifico
# si rileva la presenza valori oltre 170.
boxplot(WW$trtbps, main = "blood pressure")

# Osserviamo la presenza di un singolo outlier nei valori massimi del battito cardiaco,
# in particolare un valore minore di 80.
boxplot(WW$thalachh,  main = "maximum heart rate achieved")

# Non si rileva la presenza di outlier nei valori dell'età
boxplot(WW$age,  main = "age")


################# Rimozione Outliers ################# 

# Rimozione outlier colesterolo
quartiles <- quantile(WW$chol, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(WW$chol)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
WW_2 <- subset(WW, WW$chol > Lower & WW$chol < Upper)
boxplot(WW_2$chol, main = "cholesterol, no outliers")

# Rimozione outlier pressione sanguigna
quartiles <- quantile(WW$trtbps, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(WW$trtbps)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
WW_2 <- subset(WW, WW$trtbps > Lower & WW$trtbps < Upper)
boxplot(WW_2$trtbps, main = "blood pressure, no outliers")

# Rimozione outlier battito cardiaco
quartiles <- quantile(WW$thalachh, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(WW$thalachh)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
WW_2 <- subset(WW, WW$thalachh > Lower & WW$thalachh < Upper)
boxplot(WW_2$thalachh,  main = "maximum heart rate achieved, no outliers")

################# Istogramma di alcuni regressori e della var dipendente ################# 

hist(WW_2$age,main = "Istogramma età")
hist(WW_2$chol,main = "Istogramma colesterolo")
hist(WW_2$trtbps,main = "Istogramma pressione sanguigna")
hist(WW_2$thalachh,main = "Istogramma massimo battito cardiaco")
hist(WW_2$output,main = "Istogramma variabile dipendente")

################# Verifica della presenza di multicollinearità ################# 

# Possiamo osservare che vi è presenza di multiccolinearità dal momento che vi sono coppie di regressori
# Che presentano una significativa collreazione positiva/negativa.
# È quindi necessario utilizzare delle tecniche di regolarizzazione per ovviare a tale ostacolo
WW_2.cor = cor(WW_2)
corrplot(WW_2.cor)
WW_2.cor
n<-nrow(WW_2)


################# Applicazione delle tecniche di regolarizzazione con corss-validation ################# 

# discretizzazione di lambda in una sequenza decrescente di valori
qq<-seq (12,-3, length =300)
griglia =10^qq

# divisione tra matrice dei regressori (senza outliers) e vettore variabile dipendente
n<-nrow(WW_2)
names(WW_2)
xx<-WW_2[,-14]
x<-as.matrix(xx)
dim(x)
names(x)
y<-WW_2$output

################# Ridge regression ################# 

# Di default glmnet() standardizza i regressori
ridge.mods.ALL = glmnet (x,y,alpha=0, lambda=griglia)
coef(ridge.mods.ALL)

# Grafico andamento dei regressori rispetto al lambda
plot(ridge.mods.ALL, main="Ridge Regression con regressori standardizzati",xvar="lambda", label=TRUE)

# Applichiamo la K-Fold cross-validation con K=10 (default)
cv.outK10=cv.glmnet(x,y,lambda=griglia, alpha=0)
plot(cv.outK10)

# Estraiamo il lambda minimo a cui corrisponde la minore media deli MSE calcolati sul test-set
bestLambda.RR <- cv.outK10$lambda.min
bestLambda.RR # = 0.09047357

# Utilizziamo il lambda migliore per stimare il modello finale
ridge.mod.kCV=glmnet(x,y,alpha=0,lambda=bestLambda.RR)
coef(ridge.mod.kCV)[,1]

################# LASSO ################# 

# Usiamo la funzione glmnet() ponendo alpha=1 per la regressione LASSO con l’intero dataset
LASSO.mods.ALL =glmnet (x,y, alpha =1, lambda=griglia)
# È possibile osservare come la tecnica LASSO, anche per valori piccoli di lambda, pone alcuni regressori a zero
plot(LASSO.mods.ALL, main="LASSO; regressori standardizzati",xvar="lambda",label=TRUE)
coef(LASSO.mods.ALL)[,1]
dim(coef(LASSO.mods.ALL))

# Applichiamo la K-Fold cross-validation con K=10 (default)
cv.outK10.LASSO=cv.glmnet(x,y,lambda=griglia, alpha=1)
plot(cv.outK10.LASSO, main="LASSO: k-fold CV per WW_2")
bestLambda.LASSO<-cv.outK10.LASSO$lambda.min
# Il lambda minimo a cui corrisponde il più piccolo valore del MSE test
bestLambda.LASSO # = 0.008977952

# Stima del modello di regressione LASSO con il lambda.min
LASSO.mod.kCV=glmnet(x,y,alpha=1,lambda=bestLambda.LASSO)
coef(LASSO.mod.kCV)[,1]

################# Elastic-Net ################# 




################# Stima del modello migliore per la previsione #################





























