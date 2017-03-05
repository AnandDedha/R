library(ISLR)
library(caTools)
library(class)
library(ggplot2)

# Exploratory analysis of the data 

str(Caravan)
print(summary(Caravan$Purchase))
print(any(is.na(Caravan)))
print(var(Caravan[,1]))
print(var(Caravan[,2]))

# Standarize the varaiables as exploratory analysis show the parameters with different units and scales

purchase <- Caravan[,86]   # This is variable or 86th Column which is need to classify
standarised.Caravan <- scale(Caravan[,-86]) # Standarizing the Caravan
print(head(standarised.Caravan))
print(var(standarised.Caravan[,1]))
print(var(standarised.Caravan[,2]))

# Train and test data split
test.index <- 1:1000
test.data <- standarised.Caravan[test.index,]
test.purchase <- purchase[test.index]

train.data <- standarised.Caravan[-test.index,]
train.purchase <- purchase[-test.index]


# KNN model
set.seed(101)
predicted.purchase <- knn(train.data, test.data, train.purchase, k=1)
misclass.error <- mean(test.purchase != predicted.purchase)
print(misclass.error)


# Identifying the K value 
predicted.purchase <- NULL
error.rate <- NULL

for(i in 1:20){
  
  set.seed(101)
  predicted.purchase <- knn(train.data, test.data, train.purchase,k=i)
  error.rate[i] <- mean(test.purchase != predicted.purchase)
}

#Visulization of K value with their error rate to identify the optimial K values
k.values <- 1:20
df <- data.frame(k.values,error.rate)
pl <- ggplot(df, aes(k.values,error.rate)) + geom_point() + geom_line()
print(pl)
