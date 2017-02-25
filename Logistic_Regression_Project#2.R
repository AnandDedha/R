library(dplyr)
library(Amelia)
library(caTools)
adult <- read.csv('adult_sal.csv')
adult <- select(adult, -X)
print(head(adult))
print(str(adult))
print(summary(adult))
print(table(adult$type_employer))
#print(table(adult$type_employer,useNA = "ifany"))
emp_grp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  } else if(job=='State-gov' | job=='Local-gov'){
    return('SL-gov')
  }else if(job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  } else{
    return(job)
  }
}
adult$type_employer <- sapply(adult$type_employer,emp_grp)
print(table(adult$type_employer))
print(table(adult$marital))
marital_grp <- function(marital){
  marital <- as.character(marital)
  if (marital=='Divorced' | marital=='Separated'| marital=='Widowed'){
    return('Not-Married')
  } else if(marital=='Married-AF-spouse' | marital=='Married-civ-spouse' | marital=='Married-spouse-absent'){
    return('Married')
  }else{
    return(marital)
  }
}
adult$marital <- sapply(adult$marital,marital_grp)
print(table(adult$marital))
print(str(adult))
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult[adult == '?'] <- NA
missmap(adult, main="Missing Value diagram", col = c('Black','Yellow'))
adult <- na.omit(adult)
plt <- ggplot(adult, aes(age)) + geom_histogram(aes(fill = income),bins = 30,color='black',binwidth=1)
print(plt)
plt2 <- ggplot(adult, aes(hr_per_week)) + geom_histogram(bins = 30,color='black',binwidth=5)
print(plt2)
set.seed(101)
sample <- sample.split(adult$income, SplitRatio = 0.70 )
test <- subset(adult, sample==T)
train <- subset(adult, sample==F)
log_model = glm(income ~ ., family =(binomial(link = "logit")),data=train)
summary(log_model)
test$predicted.income = predict(model, newdata=test, type="response")
table(test$income, test$predicted.income > 0.5)