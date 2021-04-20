#loading data
heart <- read.csv("heart.csv")
View (heart)

#splitting data
set.seed(2)
id <- sample(2,nrow(heart),prob = c(0.7,0.3), replace = TRUE)
heart_train <- heart[id==1,]
heart_test <- heart[id==2,]

# install.packages("randomForest")
library(randomForest)
heart$output <- as.factor(heart$output)
heart_train$output <- as.factor(heart_train$output)
bestmtry<- tuneRF(heart_train,heart_train$output,stepFactor=1.2,improve=0.01,trace=T,plot=T)

#Random Forest
heart_forest <- randomForest(output~. ,data = heart_train)
heart_forest

#getting variable priority
importance(heart_forest)
varImpPlot(heart_forest)

#predicting
pred1_heart <- predict(heart_forest, newdata = heart_test, type = "class")
pred1_heart

#install.packages('caret', dependencies = TRUE)
library(caret)
confusionMatrix(table(pred1_heart,heart_test$output))
