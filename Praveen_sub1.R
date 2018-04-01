setwd("~/Downloads/Kaggle")

training<-read.csv("train.csv",stringsAsFactors = TRUE)
test<-read.csv("test.csv",stringsAsFactors = FALSE)

training$IsTrainset <- TRUE
test$IsTrainset <- FALSE

test$Survived<-NA

full<-rbind(training,test)

full[full$Embarked=='', "$Embarked"]<-'S'

age.median<-median(full$Age, na.rm= TRUE)

full[is.na(full$Age), "Age"] <- age.median 

fare.median<-median(full$Fare, na.rm=TRUE)

full[is.na(full$Fare), "Fare"] <- fare.median


# categorical casting
full$Pclass<- as.factor(full$Pclass)
full$Sex <-as.factor(full$Sex)
full$Embarked <-as.factor(full$Embarked)



#  split dataset back out into train and test


training<-full[full$IsTrainSet==TRUE,]
test<-full[full$IsTestSet== FALSE,]


training$Survived<- as.factor(training$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked"
survived.formula <- as.formula(survived.equation)




install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = survived.formula, data = training, ntree= 500, mtry=3, nodesize= 0.01*nrow(training))



features.equation <- "Survived ~ Pclass + Sex + Age + sibsp + Fare + Embarked"
Survived <- predict(titanic.model, newdata = test)

PassengerId <- test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file="kaggle_submission.csv", row.names = FALSE)


randomForrest(Survived~.)
