library(ggplot2)
library(ggplot2movies)
library(data.table)
library(ggthemes)
tail(movies)

# Histogram
mpl <- ggplot(movies, aes(rating))
mpl2 <- (mpl + geom_histogram(binwidth = .1 , color= 'red' , fill = 'pink', alpha = 0.5))
mpl3 <- mpl2 + xlab('Movie Rating') + ylab ('Count')
print(mpl3 + ggtitle("                     Normal distribution of Movie ratings"))


print(head(mtcars))
boxplot1 <- ggplot(mtcars, aes(x=factor(cyl),y=mpg))
boxplot1 <- boxplot1 + geom_boxplot(aes(fill = factor(cyl)))

#Building Box plot
print(boxplot1)
mtpl <- ggplot(mpg, aes(x= class))
mtpl <- mtpl + geom_bar(aes(fill = drv), position = 'fill')
print(mtpl)

#Building bin2d chart
binpl <- ggplot(movies, aes(x = year, y = rating))
binpl2  <- binpl + geom_bin2d() + scale_fill_gradient(low='pink', high ='red')
print(binpl2)

# Using coordinates and faceting
corpl <- ggplot(mpg, aes(x=displ,y=hwy))
corpl <- corpl + geom_point(color = 'orange')
print(corpl + theme_solarized())


#Mimcy a website diagram
df <- read.csv('Economist_Assignment_Data.csv')
print(head(df))
pl <- ggplot(df, aes(x=CPI, y=HDI))
pl <- pl + geom_point(aes(color= factor(Region)), size = 4, shape= 1)
pl <- pl + geom_smooth(aes(group=1),method = 'lm',formula = y ~ log(x),se = F,color = 'red')
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")
pl <- pl + geom_text(aes(label = Country), color = "gray20", 
                     data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)
#pl <- pl + geom_smooth()
print(pl +  theme_bw() )