data <- read.csv(file="heart.csv", head=TRUE, sep=",")

install.packages("caTools") 
library(caTools)
split <- sample.split(data, SplitRatio = 0.8)
split
training <- subset(data,split=="TRUE")
testing <- subset(data,split=="FALSE")

model <- glm(output~.-age,training,family="binomial")
summary(model)

res <- predict(model,testing,output="response")

table(Actualvalue=testing$output,Predictedvalue=res>0.5) 


#calculating the threshold

res <- predict(model,training,output="response")

install.packages("ROCR")
library(ROCR)
ROCRPred = prediction(res, training$output)
ROCRPref <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref,colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

#now adjusting the threshold to 0.4

res <- predict(model,testing,output="response")

table(Actualvalue=testing$output,Predictedvalue=res>0.4)
