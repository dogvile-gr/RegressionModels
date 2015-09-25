data <- mtcars
install.packages("d3heatmap")
install.packages("ggplot2")
install.packages("knitr")
install.packages("GGally")
library("ggplot2")
library("knitr")
library("GGally")
library("corrplot")

data$cyl <- factor(mtcars$cyl)
data$vs <- factor(mtcars$vs)
data$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
data$gear <- factor(mtcars$gear)
data$carb <- factor(mtcars$carb)




#ggplot boxplot 


ggplot(data, aes(x=data$am, y=data$mpg, fill=data$am)) + geom_boxplot()+
        xlab("") +
        ylab("MPG") +
        ggtitle("Boxplot of automatic and manual")+
        labs(title="MPG by Transmission type",fill="type")

#no overlapping between boxplots

# plot pairwise graph of mt cars
g = ggpairs(mtcars, lower = list(continuous = "smooth"),params = c(method="loess"))
g

corrplot(cor(mtcars), model = "number")


# model using all data as predictors
allvariables_model = lm(mpg ~ ., data = mtcars)
summary(allvariables_model)

# step wise selection process
new_model1 <- step(lm(mpg ~ ., data = mtcars), direction = "forward",trace = 0)
new_model2 <- step(lm(mpg ~ ., data = mtcars), direction = "backward",trace = 0)
summary(new_model1)$coef
summary(new_model2)$coef

# compare all models
compare <- anova(allvariables_model, new_model1,new_model2,test = "F")
compare$Pr

par(mfrow=c(2, 2))
plot(new_model2)
