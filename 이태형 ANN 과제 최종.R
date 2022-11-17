DAECHUNG1 <- read.csv("DAECHUNG1.csv")
DAECHUNG1 <- read.csv("DAECHUNG1.csv",header = TRUE)
str(DAECHUNG1)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
DAECHUNG1_norm <- as.data.frame(lapply(DAECHUNG1, normalize))
summary(DAECHUNG1_norm$Chl.a)
summary(DAECHUNG1$Chl.a)

DAECHUNG1_train <- DAECHUNG1_norm[1:743, ]
DAECHUNG1_test <- DAECHUNG1_norm[744:990, ]

library(neuralnet)
DAECHUNG1_model <- neuralnet(Chl.a ~ COD + BOD 
                            + DO + TN + TP, 
                            data = DAECHUNG1_train)
windows(width = 10, height = 10)
plot(DAECHUNG1_model)

model_results <- compute(DAECHUNG1_model, DAECHUNG1_test[1:5])
predicted_Chl.a <- model_results$net.result
cor(predicted_Chl.a, DAECHUNG1_test$Chl.a)
plot(predicted_Chl.a, DAECHUNG1_test$Chl.a)

DAECHUNG1_model2 <- neuralnet(Chl.a ~ COD + BOD +
                               DO + TN + TP,
                             data = DAECHUNG1_train, hidden = 5)
plot(DAECHUNG1_model2)

model_results2 <- compute(DAECHUNG1_model2, DAECHUNG1_test[1:5])
predicted_Chl.a2 <- model_results2$net.result
cor(predicted_Chl.a2, DAECHUNG1_test$Chl.a)
plot(predicted_Chl.a2, DAECHUNG1_test$Chl.a)


MLR <-lm(Chl.a ~ COD + BOD +
           DO + TN + TP,
         data = DAECHUNG1_train)
summary(MLR)
MLR_prediected=predict(MLR, newdata = DAECHUNG1_test[1:5])
cor(MLR_prediected, DAECHUNG1_test$Chl.a)
plot(MLR_prediected, DAECHUNG1_test$Chl.a)
