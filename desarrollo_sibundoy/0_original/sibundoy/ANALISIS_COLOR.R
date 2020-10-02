library(readxl)
getwd()
setwd("E:/SIBUNDOY/Compilado Sibundoy/8 BASE DE DATOS OBSERVACIONES DE CAMPO")
data <- data.frame(read_excel("OBSERVACIONES_20160603_VF.xls", sheet = "VERTICALIZADO", na = "NA"))
str(data)
summary(data)
#View(data)
names(data)
dim(data)
library(aqp)
color <- munsell2rgb(the_hue=data$HUE, the_value=data$V,
                     the_chroma=data$C, return_triplets=T)
dim(color)
data <- cbind(data,color)
names(data)
head(data)
summary(data)
str(data)

H1 <- subset(data, HORIZONTE!="1",select=c(ID_PERFILHOR,TXT,PH,NAF,HCL,H202,ESP,DIP,r,g,b,HORDIAG))
summary(H1)
dim(H1)
tail(H1)

round(cor(H1[c(7:9)],use = "complete.obs"),2)
names(H1)
library(caret)
str(H1)
summary(H1)

#H1 <- subset(H1, HORDIAG!="NA"&r!="NA")
H1 <- H1[complete.cases(H1), ]
dim(H1)
summary(H1)
names(H1)
H1$HORDIAG <- as.factor(H1$HORDIAG)
H1$TXT <- as.factor(H1$TXT)
H1$NAF <- as.factor(H1$NAF)
H1$H202 <- as.factor(H1$H202)
H1$DIP <- as.factor(H1$DIP)
H1$HCL <- as.factor(H1$HCL)
H1$ID_PERFILHOR <- as.factor(H1$ID_PERFILHOR)
str(H1)


allrows <- 1:nrow(H1)

trainIndex <- createDataPartition(allrows, p = 0.75 ,list = F,times = 1)
tail(trainIndex)
Train <- H1[ trainIndex,]
Test <- H1[-trainIndex,]
summary(Train)
names(Train)
summary(Test)

library(doMC)
library(doParallel)


ls(getModelInfo())


control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
preProcess=c("center", "scale")
str(Train)
Train$HORDIAG <- as.factor(Train$HORDIAG)
names(H1)
levels(H1$HORDIAG)
levels(H1$TXT)
levels(H1$NAF)
levels(H1$HCL)
levels(H1$H202)
levels(H1$DIP)


cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
# Linear Discriminant Analysis
# set.seed(seed)
names(Train)
Train$HCL <- NULL
(fit.lda <- train(HORDIAG~PH+ESP+r+g+b, data=Train, method="lda",
                  metric=metric, preProc=c("center", "scale"), trControl=control))
str(fit.lda)
fit.lda
confusionMatrix(fit.lda)
install.packages("kknn")
library(kknn)
library(klaR)

# Logistic Regression
(fit.glm <- train(HORDIAG~r+g+b, data=Train, method="glm",
                  metric=metric, trControl=control))
confusionMatrix(fit.glm)

(fit.svmRadial <- train(HORDIAG~., data=Train, method="svmRadial",
                        metric=metric, preProc=c("center", "scale"), trControl=control,
                        fit=FALSE))

(fit.kknn <- train(HORDIAG~PH+ESP+r+g+b, data=Train, method="kknn",
                        metric=metric, preProc=c("center", "scale"), trControl=control))

(fit.nb <- train(HORDIAG~PH+ESP+r+g+b, data=Train, method="nb",
                 metric=metric, trControl=control))

(fit.cart <- train(HORDIAG~., data=Train, method="rpart",
                   metric=metric, trControl=control))


(fit.rf <- train(HORDIAG~PH+ESP+r+g+b, data=Train, method="rf", metric=metric,
                 trControl=control))

library(gbm)
(fit.gbm <- train(HORDIAG~., data=Train, method="gbm",
                  metric=metric, trControl=control, verbose=FALSE))
