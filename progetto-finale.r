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
library(ggplot2)
library(ggpubr)

# Set del seme per evitare risultati diversi in run successivi
set.seed(100)

# https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-data set
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
#quartiles <- quantile(WW$chol, probs=c(.25, .75), na.rm = FALSE)
#IQR <- IQR(WW$chol)
#Lower <- quartiles[1] - 1.5*IQR
#Upper <- quartiles[2] + 1.5*IQR 
#WW_2 <- subset(WW, WW$chol > Lower & WW$chol < Upper)
#boxplot(WW_2$chol, main = "cholesterol, no outliers")

# Rimozione outlier pressione sanguigna
#quartiles <- quantile(WW$trtbps, probs=c(.25, .75), na.rm = FALSE)
#IQR <- IQR(WW$trtbps)
#Lower <- quartiles[1] - 1.5*IQR
#Upper <- quartiles[2] + 1.5*IQR 
#WW_2 <- subset(WW, WW$trtbps > Lower & WW$trtbps < Upper)
#boxplot(WW_2$trtbps, main = "blood pressure, no outliers")

# Rimozione outlier battito cardiaco
#quartiles <- quantile(WW$thalachh, probs=c(.25, .75), na.rm = FALSE)
#IQR <- IQR(WW$thalachh)
#Lower <- quartiles[1] - 1.5*IQR
#Upper <- quartiles[2] + 1.5*IQR 
#WW_2 <- subset(WW, WW$thalachh > Lower & WW$thalachh < Upper)
#boxplot(WW_2$thalachh,  main = "maximum heart rate achieved, no outliers")

################# Istogramma di alcuni regressori (valori discreti) ################# 

heart_1=WW%>%rename("Chest_pain"="cp","cholestrol"="chol",
                       "exercise_induced_angina"="exng","number_of_vessel"="caa",
                       "blood_pressure"="trtbps","fast_blood_sugar"="fbs",
                       "max_heart_rate_achieved"="thalachh","stress_test"="thall")

heart1=heart_1%>%mutate(sex=recode(sex,"1"="Male","0"="Female"))
heart1=heart1%>%mutate(exercise_induced_angina=recode(exercise_induced_angina,"1"="yes","0"="No"))
heart1=heart1%>%mutate(Chest_pain=recode(Chest_pain,"0"="t-angina","1"="a-angina","2"="non-anginal","3"="asymptomatic"))
heart1=heart1%>%mutate(slp=recode(slp,"0"="upsloping","1"="flat","2"="downsloping"))
heart1=heart1%>%mutate(fbs=recode(fbs,"0"="fasting blood sugar > 120","1"="fasting blood sugar < 120"))
heart1=heart1%>%mutate(restecg=recode(restecg,"0"="normal","1"="ST-T wave abnormality","2"="left ventricular hypertrophy"))
heart1=heart1%>%mutate(restecg=recode(restecg,"0"="normal","1"="ST-T wave abnormality","2"="left ventricular hypertrophy"))

ggplot(data=heart1,aes(x=sex,fill=sex))+geom_bar()
ggplot(data=heart1,aes(x=age,fill=sex))+geom_histogram(col="black")
ggplot(data=heart1,aes(x=Chest_pain,fill=sex))+geom_bar()+theme(axis.text=element_text(size = 8))
ggplot(data=heart1,aes(x=slp,fill=sex))+geom_bar()
ggplot(data=heart1,aes(x=exercise_induced_angina,fill=sex))+geom_bar()
ggplot(data=heart1,aes(x=fbs,fill=sex))+geom_bar()
ggplot(data=heart1,aes(x=restecg,fill=sex))+geom_bar()


################# Grafici per regressori continui ################# 

plot_chol_bp=ggplot(data=heart1,aes(x=cholestrol,y=blood_pressure,col=sex))+geom_point()+geom_smooth(method="lm",se=FALSE)
ggarrange(plot_chol_bp,ncol=1,nrow=1)

plot_chol_mhr=ggplot(data=heart1,aes(x=cholestrol,y=max_heart_rate_achieved,col=sex))+geom_point()+geom_smooth(method="lm",se=FALSE)
ggarrange(plot_chol_mhr,ncol=1,nrow=1)

plot_chol_age=ggplot(data=heart1,aes(x=cholestrol,y=age,col=sex))+geom_point()+geom_smooth(method="lm",se=FALSE)
ggarrange(plot_chol_age,ncol=1,nrow=1)

ggplot(WW, aes(x=trtbps, y=..count..)) + geom_density(color = "red") + ggtitle("Blood Pressure") + theme(plot.title = element_text(hjust = 0.5))
ggplot(WW, aes(x=chol, y=..count..)) + geom_density(color = "red") + ggtitle("Cholesterol Level") + theme(plot.title = element_text(hjust = 0.5))


################# Verifica della presenza di multicollinearità ################# 

# Possiamo osservare che vi è presenza di multiccolinearità dal momento che vi sono coppie di regressori
# Che presentano una significativa collreazione positiva/negativa.
# È quindi necessario utilizzare delle tecniche di regolarizzazione per ovviare a tale ostacolo
WW.cor = cor(WW)
corrplot(WW.cor)
WW.cor
n<-nrow(WW)

################# Applicazione delle tecniche di regolarizzazione con corss-validation ################# 

# discretizzazione di lambda in una sequenza decrescente di valori
qq<-seq (15, -5, length =150)
griglia =10^qq

# divisione tra matrice dei regressori e vettore variabile dipendente
n<-nrow(WW)
names(WW)
xx<-WW[,-14]
x<-as.matrix(xx)
dim(x)
names(x)
y<-WW$output

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

EN.modes.ALL <- glmnet(x, y, lambda=griglia, alpha=.1)
plot(EN.modes.ALL, main="ELASTIC NET; regressori standardizzati alpha=0.1",xvar="lambda",label=TRUE)

EN.modes.ALL <- glmnet(x, y, lambda=griglia, alpha=.3)
plot(EN.modes.ALL, main="ELASTIC NET; regressori standardizzati alpha=0.3",xvar="lambda",label=TRUE)

EN.modes.ALL <- glmnet(x, y, lambda=griglia, alpha=.7)
plot(EN.modes.ALL, main="ELASTIC NET; regressori standardizzati alpha=0.7",xvar="lambda",label=TRUE)

EN.modes.ALL <- glmnet(x, y, lambda=griglia, alpha=.9)
plot(EN.modes.ALL, main="ELASTIC NET; regressori standardizzati alpha=0.9",xvar="lambda",label=TRUE)


# k-fold CROSS VALIDATION per ELASTIC NET
cv.outK10.EN01=cv.glmnet(x,y,lambda=griglia, alpha=0.1)
plot(cv.outK10.EN01, main="Elastic Net alpha=0.1: k-fold CV per WW_2")
bestLambda.EN01<-cv.outK10.EN01$lambda.min 
bestLambda.EN01
EN01.mod.kCV=glmnet(x,y,alpha=0.1,lambda=bestLambda.EN01)
coef(EN01.mod.kCV)[,1]

# Con alpha = 0.3
cv.outK10.EN03=cv.glmnet(x,y,lambda=griglia, alpha=0.3)
plot(cv.outK10.EN03, main="Elastic Net alpha=0.3: k-fold CV per WW_2")
bestLambda.EN03<-cv.outK10.EN03$lambda.min 
bestLambda.EN03
EN03.mod.kCV=glmnet(x,y,alpha=0.3,lambda=bestLambda.EN03)
coef(EN03.mod.kCV)[,1]

# Con alpha = 0.5
cv.outK10.EN05=cv.glmnet(x,y,lambda=griglia, alpha=0.5)
plot(cv.outK10.EN05, main="Elastic Net alpha=0.5: k-fold CV per WW_2")
bestLambda.EN05<-cv.outK10.EN05$lambda.min
bestLambda.EN05
EN05.mod.kCV=glmnet(x,y,alpha=0.5,lambda=bestLambda.EN05)
coef(EN05.mod.kCV)[,1]

# Con alpha = 0.7
cv.outK10.EN07=cv.glmnet(x,y,lambda=griglia, alpha=0.7)
plot(cv.outK10.EN07, main="Elastic Net alpha=0.7: k-fold CV per WW_2")
bestLambda.EN07<-cv.outK10.EN07$lambda.min
bestLambda.EN07
EN07.mod.kCV=glmnet(x,y,alpha=0.7,lambda=bestLambda.EN07)
coef(EN07.mod.kCV)[,1]

# Con alpha = 0.9
cv.outK10.EN09=cv.glmnet(x,y,lambda=griglia, alpha=0.9)
plot(cv.outK10.EN09, main="Elastic Net alpha=0.9: k-fold CV per WW_2")
bestLambda.EN09<-cv.outK10.EN09$lambda.min
bestLambda.EN09
EN09.mod.kCV=glmnet(x,y,alpha=0.9,lambda=bestLambda.EN09)
coef(EN09.mod.kCV)[,1]



################# Stima del modello migliore per la previsione #################





























