library(ggplot2)
bike <- read.csv('bikeshare.csv')
print(head(bike))
pl <- ggplot(bike, aes(x=temp,y= count))
pl<- pl + geom_point(aes(color = temp),alpha = 0.7)
print(pl)
bike$datetime <- as.POSIXct(bike$datetime)
pl2 <- ggplot(bike, aes(x=datetime,y= count)) + geom_point(aes(color = temp),alpha = 0.7) + scale_color_gradient(low = "red",high = "blue")
print(pl2)
correlation <- cor(x= bike$temp , y= bike$count)
print(correlation)
boxplotforseason <- ggplot(bike, aes(factor(season) , y=count)) +geom_boxplot(aes(color=factor(season)))
print(boxplotforseason)
bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})
print(head(bike))
library(dplyr)
bike2 <- filter(bike, workingday == 0)
pl4 <- ggplot(bike2, aes(x= hour,y=count)) + geom_point(aes(color= temp))
print(pl4)
model<- lm(count~temp, bike)
print(summary(model))