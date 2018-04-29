library(tidyverse)
library(caret)
library(mlbench)
library(glmnet)
library(glmnetUtils)
click <- as.tibble(read_csv("C:/Users/taoya/Desktop/MSCS6520/Homework/homework6/criteo_ad_click_sampled.csv", col_names = FALSE))

click<- mutate(click, X1 = as.factor(X1), X15 = as.factor(X15), X16 = as.factor(X16),
               X17 = as.factor(X17), X18 = as.factor(X18),X19 = as.factor(X19), X20 = as.factor(X20),
               X21 = as.factor(X21), X22 = as.factor(X22))

click_X1 <- click %>% group_by(X1) %>% summarise(count = n())


ggplot(data = click, mapping = aes(x = X1, y = X2))+geom_boxplot()
ggsave("X2.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X3))+geom_boxplot()
ggsave("X3.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X4))+geom_boxplot()
ggsave("X4.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X5))+geom_boxplot()
ggsave("X5.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X6))+geom_boxplot()
ggsave("X6.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X7))+geom_boxplot()
ggsave("X7.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X8))+geom_boxplot()
ggsave("X8.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X9))+geom_boxplot()
ggsave("X9.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X10))+geom_boxplot()
ggsave("X10.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X11))+geom_boxplot()
ggsave("X11.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X12))+geom_boxplot()
ggsave("X12.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X13))+geom_boxplot()
ggsave("X13.jpg")
ggplot(data = click, mapping = aes(x = X1, y = X14))+geom_boxplot()
ggsave("X14.jpg")

click_X15 <- click %>% group_by(X1, X15) %>% summarise(count = n())
ggplot(data = click_X15, mapping = aes(x = X1, y = X15)) + 
  geom_tile(mapping = aes(fill = count),colour = "white")+
  scale_fill_gradient(low = "white", high = "Steelblue")
ggsave("X15.jpg")

click_X16 <- click %>% group_by(X1, X16) %>% summarise(count = n())
ggplot(data = click_X16, mapping = aes(x = X1, y = X16)) + 
  geom_tile(mapping = aes(fill = count),colour = "white")+
  scale_fill_gradient(low = "white", high = "Steelblue")
ggsave("X16.jpg")

click_X17 <- click %>% group_by(X1, X17) %>% summarise(count = n())
ggplot(data = click_X17, mapping = aes(x = X1, y = X17)) + 
  geom_tile(mapping = aes(fill = count),colour = "white")+
  scale_fill_gradient(low = "white", high = "Steelblue")
ggsave("X17.jpg")

click_X18 <- click %>% group_by(X1, X18) %>% summarise(count = n())
ggplot(data = click_X18, mapping = aes(x = X1, y = X18)) + 
  geom_tile(mapping = aes(fill = count),colour = "white")+
  scale_fill_gradient(low = "white", high = "Steelblue")
ggsave("X18.jpg")

click_X19 <- click %>% group_by(X1, X19) %>% summarise(count = n())
ggplot(data = click_X19, mapping = aes(x = X1, y = X19)) + 
  geom_tile(mapping = aes(fill = count),colour = "white")+
  scale_fill_gradient(low = "white", high = "Steelblue")
ggsave("X19.jpg")

click_X20 <- click %>% group_by(X1, X20) %>% summarise(count = n())
ggplot(data = click_X20, mapping = aes(x = X1, y = X20)) + 
  geom_tile(mapping = aes(fill = count),colour = "white")+
  scale_fill_gradient(low = "white", high = "Steelblue")
ggsave("X20.jpg")

click_X21 <- click %>% group_by(X1, X21) %>% summarise(count = n())
ggplot(data = click_X21, mapping = aes(x = X1, y = X21)) + 
  geom_tile(mapping = aes(fill = count),colour = "white")+
  scale_fill_gradient(low = "white", high = "Steelblue")
ggsave("X21.jpg")

click_X22 <- click %>% group_by(X1, X22) %>% summarise(count = n())
ggplot(data = click_X22, mapping = aes(x = X1, y = X22)) + 
  geom_tile(mapping = aes(fill = count),colour = "white")+
  scale_fill_gradient(low = "white", high = "Steelblue")
ggsave("X22.jpg")

set.seed(121)
trainIndex <-
  createDataPartition(click$X1,
                      p = 0.8,
                      list = FALSE,
                      times = 1)
clickTrain <- click[trainIndex, ]
clickTest <- click[-trainIndex, ]
scaler <- preProcess(clickTrain, method = c("center", "scale"))
clickTrain <- predict(scaler, clickTrain)
clickTest <- predict(scaler, clickTest)

#round 1

lr_X11_X2<-glmnet(X1 ~ X11 + X2,
              data = clickTrain,
              family = "binomial",
              na.action = na.omit)

Predictions_X11_X2 <- predict(lr_X11_X2,
                          clickTest,
                          type = "class",
                          na.action = na.pass,
                          s = 0.01)
confusionMatrix(Predictions_X11_X2,clickTest$X1)

lr_X11_X3<-glmnet(X1 ~ X11 + X3,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X3 <- predict(lr_X11_X3,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X3,clickTest$X1)

lr_X11_X4<-glmnet(X1 ~ X11 + X4,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X4 <- predict(lr_X11_X4,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X4,clickTest$X1)

lr_X11_X5<-glmnet(X1 ~ X11 + X5,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X5 <- predict(lr_X11_X5,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X5,clickTest$X1)

lr_X11_X6<-glmnet(X1 ~ X11 + X6,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X6 <- predict(lr_X11_X6,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X6,clickTest$X1)


lr_X11_X7<-glmnet(X1 ~ X11 + X7,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X7 <- predict(lr_X11_X7,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X7,clickTest$X1)

lr_X11_X8<-glmnet(X1 ~ X11 + X8,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X8 <- predict(lr_X11_X8,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X8,clickTest$X1)

lr_X11_X9<-glmnet(X1 ~ X11 + X9,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X9 <- predict(lr_X11_X9,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X9,clickTest$X1)

lr_X11_X10<-glmnet(X1 ~ X11 + X10,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X10 <- predict(lr_X11_X10,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X10,clickTest$X1)

lr_X11_X12<-glmnet(X1 ~ X11 + X12,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X12 <- predict(lr_X11_X12,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X12,clickTest$X1)

lr_X11_X13<-glmnet(X1 ~ X11 + X13,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X13 <- predict(lr_X11_X13,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X13,clickTest$X1)

lr_X11_X14<-glmnet(X1 ~ X11 + X14,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X14 <- predict(lr_X11_X14,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X14,clickTest$X1)

lr_X11_X15<-glmnet(X1 ~ X11 + X15,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X15 <- predict(lr_X11_X15,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X15,clickTest$X1)

lr_X11_X16<-glmnet(X1 ~ X11 + X16,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X16 <- predict(lr_X11_X16,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X16,clickTest$X1)

lr_X11_X17 <-glmnet(X1 ~ X11 + X17,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X17 <- predict(lr_X11_X17,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X17,clickTest$X1)

lr_X11_X18<-glmnet(X1 ~ X11 + X18,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X18 <- predict(lr_X11_X18,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X18,clickTest$X1)

lr_X11_X19<-glmnet(X1 ~ X11 + X19,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X19 <- predict(lr_X11_X19,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X19,clickTest$X1)

lr_X11_X20<-glmnet(X1 ~ X11 + X20,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X20 <- predict(lr_X11_X20,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X20,clickTest$X1)

lr_X11_X21<-glmnet(X1 ~ X11 + X21,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X21 <- predict(lr_X11_X21,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X21,clickTest$X1)

lr_X11_X22<-glmnet(X1 ~ X11 + X22,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X22 <- predict(lr_X11_X22,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X22,clickTest$X1)

# round 2

lr_X11_X17_X2<-glmnet(X1 ~ X11 + X2 +X17,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X17_X2 <- predict(lr_X11_X17_X2,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X17_X2,clickTest$X1)

lr_X11_X17_X3<-glmnet(X1 ~ X11 + X3 +X17,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X17_X3 <- predict(lr_X11_X17_X3,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X17_X3,clickTest$X1)

lr_X11_X17_X4<-glmnet(X1 ~ X11 + X4 +X17,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X17_X4 <- predict(lr_X11_X17_X4,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X17_X4,clickTest$X1)

lr_X11_X17_X5<-glmnet(X1 ~ X11 + X5 + X17,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X17_X5 <- predict(lr_X11_X17_X5,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X17_X5,clickTest$X1)

lr_X11_X17_X6<-glmnet(X1 ~ X11 + X6 +X17,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X17_X6 <- predict(lr_X11_X17_X6,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X17_X6,clickTest$X1)


lr_X11_X17_X7<-glmnet(X1 ~ X11 + X7+X17,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X17_X7 <- predict(lr_X11_X17_X7,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X17_X7,clickTest$X1)

lr_X11_X17_X8<-glmnet(X1 ~ X11 + X8+ X17,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X17_X8 <- predict(lr_X11_X17_X8,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X17_X8,clickTest$X1)

lr_X11_X17_X9<-glmnet(X1 ~ X11 +X17+ X9,
                  data = clickTrain,
                  family = "binomial",
                  na.action = na.omit)

Predictions_X11_X17_X9 <- predict(lr_X11_X17_X9,
                              clickTest,
                              type = "class",
                              na.action = na.pass,
                              s = 0.01)
confusionMatrix(Predictions_X11_X17_X9,clickTest$X1)

lr_X11_X17_X10<-glmnet(X1 ~ X11+X17 + X10,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X10 <- predict(lr_X11_X17_X10,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X10,clickTest$X1)

lr_X11_X17_X12<-glmnet(X1 ~ X11 +X17+ X12,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X12 <- predict(lr_X11_X17_X12,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X12,clickTest$X1)

lr_X11_X17_X13<-glmnet(X1 ~ X11 +X17+ X13,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X13 <- predict(lr_X11_X17_X13,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X13,clickTest$X1)

lr_X11_X17_X14<-glmnet(X1 ~ X11 +X17+ X14,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X14 <- predict(lr_X11_X17_X14,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X14,clickTest$X1)

lr_X11_X17_X15<-glmnet(X1 ~ X11+X17 + X15,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X15 <- predict(lr_X11_X17_X15,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X15,clickTest$X1)

lr_X11_X17_X16<-glmnet(X1 ~ X11 +X17+ X16,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X16 <- predict(lr_X11_X17_X16,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X16,clickTest$X1)


lr_X11_X17_X18<-glmnet(X1 ~ X11+X17 + X18,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X18 <- predict(lr_X11_X17_X18,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X18,clickTest$X1)

lr_X11_X17_X19<-glmnet(X1 ~ X11 +X17+ X19,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X19 <- predict(lr_X11_X17_X19,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X19,clickTest$X1)

lr_X11_X17_X20<-glmnet(X1 ~ X11 +X17+ X20,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X20 <- predict(lr_X11_X17_X20,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X20,clickTest$X1)

lr_X11_X17_X21<-glmnet(X1 ~ X11 +X17+ X21,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X21 <- predict(lr_X11_X17_X21,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X21,clickTest$X1)

lr_X11_X17_X22<-glmnet(X1 ~ X11 +X17+ X22,
                   data = clickTrain,
                   family = "binomial",
                   na.action = na.omit)

Predictions_X11_X17_X22 <- predict(lr_X11_X17_X22,
                               clickTest,
                               type = "class",
                               na.action = na.pass,
                               s = 0.01)
confusionMatrix(Predictions_X11_X17_X22,clickTest$X1)

# round 3

lr_X11_X14_X17_X2<-glmnet(X1 ~ X11 +X14+ X2 +X17,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictions_X11_X14_X17_X2 <- predict(lr_X11_X14_X17_X2,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X2,clickTest$X1)

lr_X11_X14_X17_X3<-glmnet(X1 ~ X11 +X14+ X3 +X17,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictions_X11_X14_X17_X3 <- predict(lr_X11_X14_X17_X3,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X3,clickTest$X1)

lr_X11_X14_X17_X4<-glmnet(X1 ~ X11+X17 +X14+ X4 ,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictions_X11_X14_X17_X4 <- predict(lr_X11_X14_X17_X4,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X4,clickTest$X1)

lr_X11_X14_X17_X5<-glmnet(X1 ~ X11 +X14+X5 + X17,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictions_X11_X14_X17_X5 <- predict(lr_X11_X14_X17_X5,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X5,clickTest$X1)

lr_X11_X14_X17_X6<-glmnet(X1 ~ X11 +X14+ X6 +X17,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictions_X11_X14_X17_X6 <- predict(lr_X11_X14_X17_X6,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X6,clickTest$X1)


lr_X11_X14_X17_X7<-glmnet(X1 ~ X11 +X14+ X7+X17,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictions_X11_X14_X17_X7 <- predict(lr_X11_X14_X17_X7,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X7,clickTest$X1)

lr_X11_X14_X17_X8<-glmnet(X1 ~ X11 +X14+ X8+ X17,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictions_X11_X14_X17_X8 <- predict(lr_X11_X14_X17_X8,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X8,clickTest$X1)

lr_X11_X14_X17_X9<-glmnet(X1 ~ X11 +X14+X17+ X9,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictions_X11_X14_X17_X9 <- predict(lr_X11_X14_X17_X9,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X9,clickTest$X1)

lr_X11_X14_X17_X10<-glmnet(X1 ~ X11 + X14+X17 + X10,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X10 <- predict(lr_X11_X14_X17_X10,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X10,clickTest$X1)

lr_X11_X14_X17_X12<-glmnet(X1 ~ X11 +X14+X17+ X12,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X12 <- predict(lr_X11_X14_X17_X12,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X12,clickTest$X1)

lr_X11_X14_X17_X13<-glmnet(X1 ~ X11 +X14+X17+ X13,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X13 <- predict(lr_X11_X14_X17_X13,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X13,clickTest$X1)



lr_X11_X14_X17_X15<-glmnet(X1 ~ X11 +X14 +X17 + X15,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X15 <- predict(lr_X11_X14_X17_X15,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X15,clickTest$X1)

lr_X11_X14_X17_X16<-glmnet(X1 ~ X11 +X14 +X17+ X16,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X16 <- predict(lr_X11_X14_X17_X16,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X16,clickTest$X1)


lr_X11_X14_X17_X18<-glmnet(X1 ~ X11 +X14+X17 + X18,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X18 <- predict(lr_X11_X14_X17_X18,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18,clickTest$X1)

lr_X11_X14_X17_X19<-glmnet(X1 ~ X11 +X14+X17+ X19,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X19 <- predict(lr_X11_X14_X17_X19,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X19,clickTest$X1)

lr_X11_X14_X17_X20<-glmnet(X1 ~ X11 +X14+X17+ X20,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X20 <- predict(lr_X11_X14_X17_X20,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X20,clickTest$X1)

lr_X11_X14_X17_X21<-glmnet(X1 ~ X11 +X14+X17+ X21,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X21 <- predict(lr_X11_X14_X17_X21,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21,clickTest$X1)

lr_X11_X14_X17_X22<-glmnet(X1 ~ X11 +X14 +X17+ X22,
                       data = clickTrain,
                       family = "binomial",
                       na.action = na.omit)

Predictions_X11_X14_X17_X22 <- predict(lr_X11_X14_X17_X22,
                                   clickTest,
                                   type = "class",
                                   na.action = na.pass,
                                   s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X22,clickTest$X1)

# round 4

lr_X11_X14_X17_X21_X2<-glmnet(X1 ~ X11 +X14+X17+X21 +X2,
                          data = clickTrain,
                          family = "binomial",
                          na.action = na.omit)

Predictions_X11_X14_X17_X21_X2 <- predict(lr_X11_X14_X17_X21_X2,
                                      clickTest,
                                      type = "class",
                                      na.action = na.pass,
                                      s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X2,clickTest$X1)

lr_X11_X14_X17_X21_X3<-glmnet(X1 ~ X11 +X14+X17 +X21+X3,
                          data = clickTrain,
                          family = "binomial",
                          na.action = na.omit)

Predictions_X11_X14_X17_X21_X3 <- predict(lr_X11_X14_X17_X21_X3,
                                      clickTest,
                                      type = "class",
                                      na.action = na.pass,
                                      s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X3,clickTest$X1)

lr_X11_X14_X17_X21_X4<-glmnet(X1 ~ X11 +X14 +X17 +X21 +X4 ,
                          data = clickTrain,
                          family = "binomial",
                          na.action = na.omit)

Predictions_X11_X14_X17_X21_X4 <- predict(lr_X11_X14_X17_X21_X4,
                                      clickTest,
                                      type = "class",
                                      na.action = na.pass,
                                      s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X4,clickTest$X1)

lr_X11_X14_X17_X21_X5<-glmnet(X1 ~ X11 +X14+ X17 +X21+X5 ,
                          data = clickTrain,
                          family = "binomial",
                          na.action = na.omit)

Predictions_X11_X14_X17_X21_X5 <- predict(lr_X11_X14_X17_X21_X5,
                                      clickTest,
                                      type = "class",
                                      na.action = na.pass,
                                      s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X5,clickTest$X1)

lr_X11_X14_X17_X21_X6<-glmnet(X1 ~ X11 +X14+X17+X21+X6 ,
                          data = clickTrain,
                          family = "binomial",
                          na.action = na.omit)

Predictions_X11_X14_X17_X21_X6 <- predict(lr_X11_X14_X17_X21_X6,
                                      clickTest,
                                      type = "class",
                                      na.action = na.pass,
                                      s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X6,clickTest$X1)


lr_X11_X14_X17_X21_X7<-glmnet(X1 ~ X11 +X14+ X17+X21+X7,
                          data = clickTrain,
                          family = "binomial",
                          na.action = na.omit)

Predictions_X11_X14_X17_X21_X7 <- predict(lr_X11_X14_X17_X21_X7,
                                      clickTest,
                                      type = "class",
                                      na.action = na.pass,
                                      s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X7,clickTest$X1)

lr_X11_X14_X17_X21_X8<-glmnet(X1 ~ X11 +X14+ X17+X21+ X8,
                          data = clickTrain,
                          family = "binomial",
                          na.action = na.omit)

Predictions_X11_X14_X17_X21_X8 <- predict(lr_X11_X14_X17_X21_X8,
                                      clickTest,
                                      type = "class",
                                      na.action = na.pass,
                                      s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X8,clickTest$X1)

lr_X11_X14_X17_X21_X9<-glmnet(X1 ~ X11 +X14+X17+X21+ X9,
                          data = clickTrain,
                          family = "binomial",
                          na.action = na.omit)

Predictions_X11_X14_X17_X21_X9 <- predict(lr_X11_X14_X17_X21_X9,
                                      clickTest,
                                      type = "class",
                                      na.action = na.pass,
                                      s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X9,clickTest$X1)

lr_X11_X14_X17_X21_X10<-glmnet(X1 ~ X11 + X14+X17+X21 + X10,
                           data = clickTrain,
                           family = "binomial",
                           na.action = na.omit)

Predictions_X11_X14_X17_X21_X10 <- predict(lr_X11_X14_X17_X21_X10,
                                       clickTest,
                                       type = "class",
                                       na.action = na.pass,
                                       s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X10,clickTest$X1)

lr_X11_X14_X17_X21_X12<-glmnet(X1 ~ X11 +X14+X17+X21+ X12,
                           data = clickTrain,
                           family = "binomial",
                           na.action = na.omit)

Predictions_X11_X14_X17_X21_X12 <- predict(lr_X11_X14_X17_X21_X12,
                                       clickTest,
                                       type = "class",
                                       na.action = na.pass,
                                       s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X12,clickTest$X1)

lr_X11_X14_X17_X21_X13<-glmnet(X1 ~ X11 +X14+X17+X21+ X13,
                           data = clickTrain,
                           family = "binomial",
                           na.action = na.omit)

Predictions_X11_X14_X17_X21_X13 <- predict(lr_X11_X14_X17_X21_X13,
                                       clickTest,
                                       type = "class",
                                       na.action = na.pass,
                                       s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X13,clickTest$X1)



lr_X11_X14_X17_X21_X15<-glmnet(X1 ~ X11 +X14 +X17 +X21+ X15,
                           data = clickTrain,
                           family = "binomial",
                           na.action = na.omit)

Predictions_X11_X14_X17_X21_X15 <- predict(lr_X11_X14_X17_X21_X15,
                                       clickTest,
                                       type = "class",
                                       na.action = na.pass,
                                       s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X15,clickTest$X1)

lr_X11_X14_X17_X21_X16<-glmnet(X1 ~ X11 +X14 +X17+X21+ X16,
                           data = clickTrain,
                           family = "binomial",
                           na.action = na.omit)

Predictions_X11_X14_X17_X21_X16 <- predict(lr_X11_X14_X17_X21_X16,
                                       clickTest,
                                       type = "class",
                                       na.action = na.pass,
                                       s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X16,clickTest$X1)


lr_X11_X14_X17_X21_X18<-glmnet(X1 ~ X11 +X14+X17+X21 + X18,
                           data = clickTrain,
                           family = "binomial",
                           na.action = na.omit)

Predictions_X11_X14_X17_X21_X18 <- predict(lr_X11_X14_X17_X21_X18,
                                       clickTest,
                                       type = "class",
                                       na.action = na.pass,
                                       s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X18,clickTest$X1)

lr_X11_X14_X17_X21_X19<-glmnet(X1 ~ X11 +X14+X17+X21+ X19,
                           data = clickTrain,
                           family = "binomial",
                           na.action = na.omit)

Predictions_X11_X14_X17_X21_X19 <- predict(lr_X11_X14_X17_X21_X19,
                                       clickTest,
                                       type = "class",
                                       na.action = na.pass,
                                       s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X19,clickTest$X1)

lr_X11_X14_X17_X21_X20<-glmnet(X1 ~ X11 +X14+X17+X21+ X20,
                           data = clickTrain,
                           family = "binomial",
                           na.action = na.omit)

Predictions_X11_X14_X17_X21_X20 <- predict(lr_X11_X14_X17_X21_X20,
                                       clickTest,
                                       type = "class",
                                       na.action = na.pass,
                                       s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X20,clickTest$X1)



lr_X11_X14_X17_X21_X22<-glmnet(X1 ~ X11 +X14 +X17+X21+ X22,
                           data = clickTrain,
                           family = "binomial",
                           na.action = na.omit)

Predictions_X11_X14_X17_X21_X22 <- predict(lr_X11_X14_X17_X21_X22,
                                       clickTest,
                                       type = "class",
                                       na.action = na.pass,
                                       s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X21_X22,clickTest$X1)


# round 5

lr_X11_X14_X17_X18_X21_X2<-glmnet(X1 ~ X11 +X14+X17+X18+X21 +X2,
                              data = clickTrain,
                              family = "binomial",
                              na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X2 <- predict(lr_X11_X14_X17_X18_X21_X2,
                                          clickTest,
                                          type = "class",
                                          na.action = na.pass,
                                          s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X2,clickTest$X1)

lr_X11_X14_X17_X18_X21_X3<-glmnet(X1 ~ X11 +X14+X17+X18 +X21+X3,
                              data = clickTrain,
                              family = "binomial",
                              na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X3 <- predict(lr_X11_X14_X17_X18_X21_X3,
                                          clickTest,
                                          type = "class",
                                          na.action = na.pass,
                                          s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X3,clickTest$X1)

lr_X11_X14_X17_X18_X21_X4<-glmnet(X1 ~ X11 +X14 +X17 +X18+X21 +X4 ,
                              data = clickTrain,
                              family = "binomial",
                              na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X4 <- predict(lr_X11_X14_X17_X18_X21_X4,
                                          clickTest,
                                          type = "class",
                                          na.action = na.pass,
                                          s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X4,clickTest$X1)

lr_X11_X14_X17_X18_X21_X5<-glmnet(X1 ~ X11 +X14+ X17+X18 +X21+X5 ,
                              data = clickTrain,
                              family = "binomial",
                              na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X5 <- predict(lr_X11_X14_X17_X18_X21_X5,
                                          clickTest,
                                          type = "class",
                                          na.action = na.pass,
                                          s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X5,clickTest$X1)

lr_X11_X14_X17_X18_X21_X6<-glmnet(X1 ~ X11 +X14+X17+X18+X21+X6 ,
                              data = clickTrain,
                              family = "binomial",
                              na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X6 <- predict(lr_X11_X14_X17_X18_X21_X6,
                                          clickTest,
                                          type = "class",
                                          na.action = na.pass,
                                          s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X6,clickTest$X1)


lr_X11_X14_X17_X18_X21_X7<-glmnet(X1 ~ X11 +X14+ X17+X18+X21+X7,
                              data = clickTrain,
                              family = "binomial",
                              na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X7 <- predict(lr_X11_X14_X17_X18_X21_X7,
                                          clickTest,
                                          type = "class",
                                          na.action = na.pass,
                                          s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X7,clickTest$X1)

lr_X11_X14_X17_X18_X21_X8<-glmnet(X1 ~ X11 +X14+ X17+X18+X21+ X8,
                              data = clickTrain,
                              family = "binomial",
                              na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X8 <- predict(lr_X11_X14_X17_X18_X21_X8,
                                          clickTest,
                                          type = "class",
                                          na.action = na.pass,
                                          s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X8,clickTest$X1)

lr_X11_X14_X17_X18_X21_X9<-glmnet(X1 ~ X11 +X14+X17+X18+X21+ X9,
                              data = clickTrain,
                              family = "binomial",
                              na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X9 <- predict(lr_X11_X14_X17_X18_X21_X9,
                                          clickTest,
                                          type = "class",
                                          na.action = na.pass,
                                          s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X9,clickTest$X1)

lr_X11_X14_X17_X18_X21_X10<-glmnet(X1 ~ X11 + X14+X17+X18+X21 + X10,
                               data = clickTrain,
                               family = "binomial",
                               na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X10 <- predict(lr_X11_X14_X17_X18_X21_X10,
                                           clickTest,
                                           type = "class",
                                           na.action = na.pass,
                                           s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X10,clickTest$X1)

lr_X11_X14_X17_X18_X21_X12<-glmnet(X1 ~ X11 +X14+X17+X18+X21+ X12,
                               data = clickTrain,
                               family = "binomial",
                               na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X12 <- predict(lr_X11_X14_X17_X18_X21_X12,
                                           clickTest,
                                           type = "class",
                                           na.action = na.pass,
                                           s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X12,clickTest$X1)

lr_X11_X14_X17_X18_X21_X13<-glmnet(X1 ~ X11 +X14+X17+X18+X21+ X13,
                               data = clickTrain,
                               family = "binomial",
                               na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X13 <- predict(lr_X11_X14_X17_X18_X21_X13,
                                           clickTest,
                                           type = "class",
                                           na.action = na.pass,
                                           s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X13,clickTest$X1)



lr_X11_X14_X17_X18_X21_X15<-glmnet(X1 ~ X11 +X14 +X17+X18 +X21+ X15,
                               data = clickTrain,
                               family = "binomial",
                               na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X15 <- predict(lr_X11_X14_X17_X18_X21_X15,
                                           clickTest,
                                           type = "class",
                                           na.action = na.pass,
                                           s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X15,clickTest$X1)

lr_X11_X14_X17_X18_X21_X16<-glmnet(X1 ~ X11 +X14 +X17+X18+X21+ X16,
                               data = clickTrain,
                               family = "binomial",
                               na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X16 <- predict(lr_X11_X14_X17_X18_X21_X16,
                                           clickTest,
                                           type = "class",
                                           na.action = na.pass,
                                           s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X16,clickTest$X1)


lr_X11_X14_X17_X18_X21_X19<-glmnet(X1 ~ X11 +X14+X17+X18+X21+ X19,
                               data = clickTrain,
                               family = "binomial",
                               na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X19 <- predict(lr_X11_X14_X17_X18_X21_X19,
                                           clickTest,
                                           type = "class",
                                           na.action = na.pass,
                                           s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X19,clickTest$X1)

lr_X11_X14_X17_X18_X21_X20<-glmnet(X1 ~ X11 +X14+X17+X18+X21+ X20,
                               data = clickTrain,
                               family = "binomial",
                               na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X20 <- predict(lr_X11_X14_X17_X18_X21_X20,
                                           clickTest,
                                           type = "class",
                                           na.action = na.pass,
                                           s = 0.01)
confusionMatrix(Predictions_X11_X14_X17_X18_X21_X20,clickTest$X1)



lr_X11_X14_X17_X18_X21_X22<-glmnet(X1 ~ X11 +X14 +X17+X18+X21+ X22,
                               data = clickTrain,
                               family = "binomial",
                               na.action = na.omit)

Predictions_X11_X14_X17_X18_X21_X22 <- predict(lr_X11_X14_X17_X18_X21_X22,
                                           clickTest,
                                           type = "class",
                                           na.action = na.pass,
                                           s = 0.01)
confusionMatrix

# round 6

lr_X11_X13_X14_X17_X18_X21_X2<-glmnet(X1 ~ X11+X13 +X14+X17+X18+X21 +X2,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X2 <- predict(lr_X11_X13_X14_X17_X18_X21_X2,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X2,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X3<-glmnet(X1 ~ X11 +X13+X14+X17+X18 +X21+X3,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X3 <- predict(lr_X11_X13_X14_X17_X18_X21_X3,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X3,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X4<-glmnet(X1 ~ X11+X13 +X14 +X17 +X18+X21 +X4 ,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X4 <- predict(lr_X11_X13_X14_X17_X18_X21_X4,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X4,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X5<-glmnet(X1 ~ X11+X13 +X14+ X17+X18 +X21+X5 ,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X5 <- predict(lr_X11_X13_X14_X17_X18_X21_X5,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X5,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X6<-glmnet(X1 ~ X11+X13 +X14+X17+X18+X21+X6 ,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X6 <- predict(lr_X11_X13_X14_X17_X18_X21_X6,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X6,clickTest$X1)


lr_X11_X13_X14_X17_X18_X21_X7<-glmnet(X1 ~ X11+X13 +X14+ X17+X18+X21+X7,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X7 <- predict(lr_X11_X13_X14_X17_X18_X21_X7,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X7,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X8<-glmnet(X1 ~ X11+X13 +X14+ X17+X18+X21+ X8,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X8 <- predict(lr_X11_X13_X14_X17_X18_X21_X8,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X8,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X9<-glmnet(X1 ~ X11 +X13+X14+X17+X18+X21+ X9,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X9 <- predict(lr_X11_X13_X14_X17_X18_X21_X9,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X9,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X10<-glmnet(X1 ~ X11 +X13+ X14+X17+X18+X21 + X10,
                                   data = clickTrain,
                                   family = "binomial",
                                   na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X10 <- predict(lr_X11_X13_X14_X17_X18_X21_X10,
                                               clickTest,
                                               type = "class",
                                               na.action = na.pass,
                                               s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X10,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X12<-glmnet(X1 ~ X11 +X13+X14+X17+X18+X21+ X12,
                                   data = clickTrain,
                                   family = "binomial",
                                   na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X12 <- predict(lr_X11_X13_X14_X17_X18_X21_X12,
                                               clickTest,
                                               type = "class",
                                               na.action = na.pass,
                                               s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X12,clickTest$X1)




lr_X11_X13_X14_X17_X18_X21_X15<-glmnet(X1 ~ X11 +X13+X14 +X17+X18 +X21+ X15,
                                   data = clickTrain,
                                   family = "binomial",
                                   na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X15 <- predict(lr_X11_X13_X14_X17_X18_X21_X15,
                                               clickTest,
                                               type = "class",
                                               na.action = na.pass,
                                               s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X15,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X16<-glmnet(X1 ~ X11 +X13+X14 +X17+X18+X21+ X16,
                                   data = clickTrain,
                                   family = "binomial",
                                   na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X16 <- predict(lr_X11_X13_X14_X17_X18_X21_X16,
                                               clickTest,
                                               type = "class",
                                               na.action = na.pass,
                                               s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X16,clickTest$X1)


lr_X11_X13_X14_X17_X18_X21_X19<-glmnet(X1 ~ X11 +X13+X14+X17+X18+X21+ X19,
                                   data = clickTrain,
                                   family = "binomial",
                                   na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X19 <- predict(lr_X11_X13_X14_X17_X18_X21_X19,
                                               clickTest,
                                               type = "class",
                                               na.action = na.pass,
                                               s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X19,clickTest$X1)

lr_X11_X13_X14_X17_X18_X21_X20<-glmnet(X1 ~ X11 +X13+X14+X17+X18+X21+ X20,
                                   data = clickTrain,
                                   family = "binomial",
                                   na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X20 <- predict(lr_X11_X13_X14_X17_X18_X21_X20,
                                               clickTest,
                                               type = "class",
                                               na.action = na.pass,
                                               s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X20,clickTest$X1)



lr_X11_X13_X14_X17_X18_X21_X22<-glmnet(X1 ~ X11 +X13+X14 +X17+X18+X21+ X22,
                                   data = clickTrain,
                                   family = "binomial",
                                   na.action = na.omit)

Predictions_X11_X13_X14_X17_X18_X21_X22 <- predict(lr_X11_X13_X14_X17_X18_X21_X22,
                                               clickTest,
                                               type = "class",
                                               na.action = na.pass,
                                               s = 0.01)
confusionMatrix(Predictions_X11_X13_X14_X17_X18_X21_X22,clickTest$X1)

# round 7

lr_X3_X11_X13_X14_X17_X18_X21_X2<-glmnet(X1 ~ X3+X11+X13 +X14+X17+X18+X21 +X2,
                                      data = clickTrain,
                                      family = "binomial",
                                      na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X2 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X2,
                                                  clickTest,
                                                  type = "class",
                                                  na.action = na.pass,
                                                  s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X2,clickTest$X1)



lr_X3_X11_X13_X14_X17_X18_X21_X4<-glmnet(X1 ~ X3+X11+X13 +X14 +X17 +X18+X21 +X4 ,
                                      data = clickTrain,
                                      family = "binomial",
                                      na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X4 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X4,
                                                  clickTest,
                                                  type = "class",
                                                  na.action = na.pass,
                                                  s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X4,clickTest$X1)

lr_X3_X11_X13_X14_X17_X18_X21_X5<-glmnet(X1 ~ X3+X11+X13 +X14+ X17+X18 +X21+X5 ,
                                      data = clickTrain,
                                      family = "binomial",
                                      na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X5 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X5,
                                                  clickTest,
                                                  type = "class",
                                                  na.action = na.pass,
                                                  s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X5,clickTest$X1)

lr_X3_X11_X13_X14_X17_X18_X21_X6<-glmnet(X1 ~ X3 +X11+X13 +X14+X17+X18+X21+X6 ,
                                      data = clickTrain,
                                      family = "binomial",
                                      na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X6 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X6,
                                                  clickTest,
                                                  type = "class",
                                                  na.action = na.pass,
                                                  s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X6,clickTest$X1)


lr_X3_X11_X13_X14_X17_X18_X21_X7<-glmnet(X1 ~ X3+X11+X13 +X14+ X17+X18+X21+X7,
                                      data = clickTrain,
                                      family = "binomial",
                                      na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X7 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X7,
                                                  clickTest,
                                                  type = "class",
                                                  na.action = na.pass,
                                                  s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X7,clickTest$X1)

lr_X3_X11_X13_X14_X17_X18_X21_X8<-glmnet(X1 ~ X3+X11+X13 +X14+ X17+X18+X21+ X8,
                                      data = clickTrain,
                                      family = "binomial",
                                      na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X8 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X8,
                                                  clickTest,
                                                  type = "class",
                                                  na.action = na.pass,
                                                  s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X8,clickTest$X1)

lr_X3_X11_X13_X14_X17_X18_X21_X9<-glmnet(X1 ~ X3+X11 +X13+X14+X17+X18+X21+ X9,
                                      data = clickTrain,
                                      family = "binomial",
                                      na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X9 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X9,
                                                  clickTest,
                                                  type = "class",
                                                  na.action = na.pass,
                                                  s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X9,clickTest$X1)

lr_X3_X11_X13_X14_X17_X18_X21_X10<-glmnet(X1 ~ X3+X11 +X13+ X14+X17+X18+X21 + X10,
                                       data = clickTrain,
                                       family = "binomial",
                                       na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X10 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X10,
                                                   clickTest,
                                                   type = "class",
                                                   na.action = na.pass,
                                                   s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X10,clickTest$X1)

lr_X3_X11_X13_X14_X17_X18_X21_X12<-glmnet(X1 ~ X3+X11 +X13+X14+X17+X18+X21+ X12,
                                       data = clickTrain,
                                       family = "binomial",
                                       na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X12 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X12,
                                                   clickTest,
                                                   type = "class",
                                                   na.action = na.pass,
                                                   s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X12,clickTest$X1)




lr_X3_X11_X13_X14_X17_X18_X21_X15<-glmnet(X1 ~ X3+X11 +X13+X14 +X17+X18 +X21+ X15,
                                       data = clickTrain,
                                       family = "binomial",
                                       na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X15 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X15,
                                                   clickTest,
                                                   type = "class",
                                                   na.action = na.pass,
                                                   s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X15,clickTest$X1)

lr_X3_X11_X13_X14_X17_X18_X21_X16<-glmnet(X1 ~ X3+X11 +X13+X14 +X17+X18+X21+ X16,
                                       data = clickTrain,
                                       family = "binomial",
                                       na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X16 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X16,
                                                   clickTest,
                                                   type = "class",
                                                   na.action = na.pass,
                                                   s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X16,clickTest$X1)


lr_X3_X11_X13_X14_X17_X18_X21_X19<-glmnet(X1 ~ X3+X11 +X13+X14+X17+X18+X21+ X19,
                                       data = clickTrain,
                                       family = "binomial",
                                       na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X19 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X19,
                                                   clickTest,
                                                   type = "class",
                                                   na.action = na.pass,
                                                   s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X19,clickTest$X1)

lr_X3_X11_X13_X14_X17_X18_X21_X20<-glmnet(X1 ~ X3+X11 +X13+X14+X17+X18+X21+ X20,
                                       data = clickTrain,
                                       family = "binomial",
                                       na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X20 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X20,
                                                   clickTest,
                                                   type = "class",
                                                   na.action = na.pass,
                                                   s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X20,clickTest$X1)



lr_X3_X11_X13_X14_X17_X18_X21_X22<-glmnet(X1 ~ X3+X11 +X13+X14 +X17+X18+X21+ X22,
                                       data = clickTrain,
                                       family = "binomial",
                                       na.action = na.omit)

Predictions_X3_X11_X13_X14_X17_X18_X21_X22 <- predict(lr_X3_X11_X13_X14_X17_X18_X21_X22,
                                                   clickTest,
                                                   type = "class",
                                                   na.action = na.pass,
                                                   s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X17_X18_X21_X22,clickTest$X1)





lr_X3_X11_X13_X14_X16_X17_X18_X19_X21_X22<-glmnet(X1 ~ X3+X11 +X13+X14+X16 +X17+X18+X19+X21+ X22,
                                          data = clickTrain,
                                          family = "binomial",
                                          na.action = na.omit)

Predictions_X3_X11_X13_X14_X16_X17_X18_X19_X21_X22 <- predict(lr_X3_X11_X13_X14_X16_X17_X18_X19_X21_X22,
                                                      clickTest,
                                                      type = "class",
                                                      na.action = na.pass,
                                                      s = 0.01)
confusionMatrix(Predictions_X3_X11_X13_X14_X16_X17_X18_X19_X21_X22,clickTest$X1)


click<- mutate(click, lX2 = log1p(X2),lX3 = log1p(X3+1),lX4 = log1p(X4),lX5 = log1p(X5),lX6 = log1p(X6),
               lX7 = log1p(X7),lX8 = log1p(X8),lX9 = log1p(X9),lX10 = log1p(X10),lX11 = log1p(X11),
               lX12 = log1p(X12),lX13 = log1p(X13),lX14 = log1p(X14))

#round 8

lrmodel1_lX2<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX2,
                                                 data = clickTrain,
                                                 family = "binomial",
                                                 na.action = na.omit)

Predictionsmodel1_lX2 <- predict(lrmodel1_lX2,
                                clickTest,
                                type = "class",
                                na.action = na.pass,
                                s = 0.01)
confusionMatrix(Predictionsmodel1_lX2,clickTest$X1)

lrmodel1_lX4<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX4,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX4 <- predict(lrmodel1_lX4,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX4,clickTest$X1)

lrmodel1_lX5<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX5,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX5 <- predict(lrmodel1_lX5,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX5,clickTest$X1)

lrmodel1_lX6<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX6,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX6 <- predict(lrmodel1_lX6,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX6,clickTest$X1)

lrmodel1_lX7<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX7 <- predict(lrmodel1_lX7,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX7,clickTest$X1)

lrmodel1_lX8<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX8,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX8 <- predict(lrmodel1_lX8,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX8,clickTest$X1)

lrmodel1_lX9<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX9,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX9 <- predict(lrmodel1_lX9,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX9,clickTest$X1)

lrmodel1_lX10<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX10,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX10 <- predict(lrmodel1_lX10,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX10,clickTest$X1)

lrmodel1_lX11<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX11,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX11 <- predict(lrmodel1_lX11,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX11,clickTest$X1)

lrmodel1_lX12<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX12,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX12 <- predict(lrmodel1_lX12,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX12,clickTest$X1)

lrmodel1_lX13<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX13,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX13 <- predict(lrmodel1_lX13,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX13,clickTest$X1)

lrmodel1_lX14<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX14,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel1_lX14 <- predict(lrmodel1_lX14,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel1_lX14,clickTest$X1)

#round 9

lrmodel1_lX7_lX6<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictionsmodel1_lX7_lX6 <- predict(lrmodel1_lX7_lX6,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX6,clickTest$X1)

lrmodel1_lX7_lX8<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX8,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel1_lX7_lX8 <- predict(lrmodel1_lX7_lX8,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX8,clickTest$X1)

lrmodel1_lX7_lX12<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX12,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel1_lX7_lX12 <- predict(lrmodel1_lX7_lX12,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX12,clickTest$X1)

lrmodel1_lX7_lX14<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX14,
                          data = clickTrain,
                          family = "binomial",
                          na.action = na.omit)

Predictionsmodel1_lX7_lX14 <- predict(lrmodel1_lX7_lX14,
                                      clickTest,
                                      type = "class",
                                      na.action = na.pass,
                                      s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX14,clickTest$X1)

#round 10

lrmodel1_lX7_lX6_lX8<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel1_lX7_lX6_lX8 <- predict(lrmodel1_lX7_lX6_lX8,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX6_lX8,clickTest$X1)

lrmodel1_lX7_lX6_lX12<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX12,
                             data = clickTrain,
                             family = "binomial",
                             na.action = na.omit)

Predictionsmodel1_lX7_lX6_lX12 <- predict(lrmodel1_lX7_lX6_lX12,
                                         clickTest,
                                         type = "class",
                                         na.action = na.pass,
                                         s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX6_lX12,clickTest$X1)

lrmodel1_lX7_lX6_lX14<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX14,
                              data = clickTrain,
                              family = "binomial",
                              na.action = na.omit)

Predictionsmodel1_lX7_lX6_lX14 <- predict(lrmodel1_lX7_lX6_lX14,
                                          clickTest,
                                          type = "class",
                                          na.action = na.pass,
                                          s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX6_lX14,clickTest$X1)

#round 11

lrmodel1_lX7_lX6_lX8_lX12<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX12,
                             data = clickTrain,
                             family = "binomial",
                             na.action = na.omit)

Predictionsmodel1_lX7_lX6_lX8_lX12 <- predict(lrmodel1_lX7_lX6_lX8_lX12,
                                         clickTest,
                                         type = "class",
                                         na.action = na.pass,
                                         s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX6_lX8_lX12,clickTest$X1)

lrmodel1_lX7_lX6_lX8_lX14<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictionsmodel1_lX7_lX6_lX8_lX14 <- predict(lrmodel1_lX7_lX6_lX8_lX14,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX6_lX8_lX14,clickTest$X1)

lrmodel1_lX7_lX6_lX8_lX12_lX14<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX12+lX14,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictionsmodel1_lX7_lX6_lX8_lX12_lX14 <- predict(lrmodel1_lX7_lX6_lX8_lX12_lX14,
                                              clickTest,
                                              type = "class",
                                              na.action = na.pass,
                                              s = 0.01)
confusionMatrix(Predictionsmodel1_lX7_lX6_lX8_lX12_lX14,clickTest$X1)

click <- mutate(click, bX2 = cut_interval(X2, 8),bX3 = cut_interval(X3, 7),
                bX4 = cut_interval(X4, 7),bX5 = cut_interval(X5, 11),
                bX6 = cut_interval(X6, 11),bX7 = cut_interval(X7, 8),
                bX8 = cut_interval(X8, 9),bX9 = cut_interval(X9, 7),
                bX10 = cut_interval(X10, 8),bX11 = cut_interval(X11, 9),
                bX12 = cut_interval(X12, 6),bX13 = cut_interval(X13, 11),
                bX14 = cut_interval(X14, 6))
#round 12

lrmodel2_bX2<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX2,
                                  data = clickTrain,
                                  family = "binomial",
                                  na.action = na.omit)

Predictionsmodel2_bX2 <- predict(lrmodel2_bX2,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictionsmodel2_bX2,clickTest$X1)

lrmodel2_bX3<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX3,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel2_bX3 <- predict(lrmodel2_bX3,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel2_bX3,clickTest$X1)

lrmodel2_bX4<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX4,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel2_bX4 <- predict(lrmodel2_bX4,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel2_bX4,clickTest$X1)

lrmodel2_bX5<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX5,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel2_bX5 <- predict(lrmodel2_bX5,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel2_bX5,clickTest$X1)

lrmodel2_bX6<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX6,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel2_bX6 <- predict(lrmodel2_bX6,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel2_bX6,clickTest$X1)

lrmodel2_bX7<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX7,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel2_bX7 <- predict(lrmodel2_bX7,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel2_bX7,clickTest$X1)

lrmodel2_bX8<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX8,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel2_bX8 <- predict(lrmodel2_bX8,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel2_bX8,clickTest$X1)

lrmodel2_bX9<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX9,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel2_bX9 <- predict(lrmodel2_bX9,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel2_bX9,clickTest$X1)

lrmodel2_bX10<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX10,
                     data = clickTrain,
                     family = "binomial",
                     na.action = na.omit)

Predictionsmodel2_bX10 <- predict(lrmodel2_bX10,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel2_bX10,clickTest$X1)

lrmodel2_bX11<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictionsmodel2_bX11 <- predict(lrmodel2_bX11,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictionsmodel2_bX11,clickTest$X1)


lrmodel2_bX12<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX12,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictionsmodel2_bX12 <- predict(lrmodel2_bX12,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictionsmodel2_bX12,clickTest$X1)

lrmodel2_bX13<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX13,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictionsmodel2_bX13<- predict(lrmodel2_bX13,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictionsmodel3_bX13,clickTest$X1)

lrmodel2_bX14<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX14,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictionsmodel2_bX14<- predict(lrmodel2_bX14,
                                 clickTest,
                                 type = "class",
                                 na.action = na.pass,
                                 s = 0.01)
confusionMatrix(Predictionsmodel2_bX14,clickTest$X1)


#round 13

lrmodel3_i_3_11<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11+X3:X11,
                      data = clickTrain,
                      family = "binomial",
                      na.action = na.omit)

Predictionsmodel3_i_3_11 <- predict(lrmodel3_i_3_11,
                                  clickTest,
                                  type = "class",
                                  na.action = na.pass,
                                  s = 0.01)
confusionMatrix(Predictionsmodel3_i_3_11,clickTest$X1)

lrmodel3_i_11_13<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11+X11:X13,
                        data = clickTrain,
                        family = "binomial",
                        na.action = na.omit)

Predictionsmodel3_i_11_13 <- predict(lrmodel3_i_11_13,
                                    clickTest,
                                    type = "class",
                                    na.action = na.pass,
                                    s = 0.01)
confusionMatrix(Predictionsmodel3_i_11_13,clickTest$X1)

lrmodel3_i_13_14<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11+X13:X14,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel3_i_13_14 <- predict(lrmodel3_i_13_14,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel3_i_13_14,clickTest$X1)

lrmodel3_i_14_16<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11+X14:X16,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel3_i_14_16 <- predict(lrmodel3_i_14_16,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel3_i_14_16,clickTest$X1)

lrmodel3_i_16_17<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11+X16:X17,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel3_i_16_17 <- predict(lrmodel3_i_16_17,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel3_i_16_17,clickTest$X1)

lrmodel3_i_17_18<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11+X17:X18,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel3_i_17_18 <- predict(lrmodel3_i_17_18,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel3_i_17_18,clickTest$X1)

lrmodel3_i_18_19<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11+X18:X19,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel3_i_18_19 <- predict(lrmodel3_i_18_19,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel3_i_18_19,clickTest$X1)

lrmodel3_i_19_21<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11+X19:X21,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel3_i_19_21 <- predict(lrmodel3_i_19_21,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel3_i_19_21,clickTest$X1)


lrmodel3_i_21_22<-glmnet(X1 ~ X3+X11+X13+X14+X16+X17+X18+X19+X21+X22+lX7+lX6+lX8+lX14+bX11+X21:X22,
                         data = clickTrain,
                         family = "binomial",
                         na.action = na.omit)

Predictionsmodel3_i_21_22 <- predict(lrmodel3_i_21_22,
                                     clickTest,
                                     type = "class",
                                     na.action = na.pass,
                                     s = 0.01)
confusionMatrix(Predictionsmodel3_i_21_22,clickTest$X1)
