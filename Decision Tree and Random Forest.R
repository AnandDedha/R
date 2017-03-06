library(rpart)
library(ISLR)
library(ggplot2)
library(dplyr)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
df <- data.frame(College)

#Exploratory Analysis 
print(head(df))
plt <- ggplot(df, aes(Grad.Rate, Room.Board)) + geom_point(aes(color= Private))
print(plt)
plt2 <- ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill= Private), color = 'black')
print(plt2)
plt3 <- ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill= Private),color = 'black')
print(plt3)
college.hundred <- filter(df, Grad.Rate >= 100)
print(filter(df, Grad.Rate >100))
df['Cazenovia College', 'Grad.Rate'] <- 100
plt4 <- ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill= Private),color = 'black')
print(plt4)

#Test and train Split()
set.seed(101)
sample = sample.split(df$Private, SplitRatio = 0.7)
train.df <- subset(df, sample== T)
test.df <- subset(df, sample==F)

#Decision tree clasifier
tree <- rpart(Private ~., data = df)
summary(tree)
prp(tree)

#Evaluating the model
tree.predict <- predict(tree, test.df)
print(head(tree.predict))
print(tail(tree.predict))
tree.predict <- as.data.frame(tree.predict)
func1 <- function(x){
  if(x>=0.5){
    return ('yes')
  }else{
    return('no')
  }
}
tree.predict$Private <- sapply(tree.predict$Yes, func1)
print(head(tree.predict))

#Random Forest
forest <- randomForest(Private ~ . , data = train,importance = TRUE)
p <- predict(rf.model,test)
table(p,test$Private)

