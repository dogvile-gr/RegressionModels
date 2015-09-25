---
title:"Effect of Transmission Type on MPG of Cars"
output:html_document
---
        
        ## Executive Summary

In this report we will use a dataset from the 1974 Motor Trend US magazine to answer the following questions:

- Is an automatic or manual transmission better for MPG?  
- What is the MPG difference between automatic and manual transmissions?  

Using data exploratory analysis, hypothesis testing and simple linear regression, we determine whether is a signficant difference between the mean MPG for automatic and manual transmission cars. In brief, based on anova test it will be tested whether a simple, backward, forward regression model is the best fit for our purpose.        

## Summary of data
The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models). The qualitative variables such as number of cylinders and gears were converted to factors. A description of the variables is available in the appendix.

```{r, setoptions, echo = FALSE}
data(mtcars)
library(knitr)
install.packages("d3heatmap")
install.packages("ggplot2")
install.packages("knitr")
install.packages("GGally")
library("ggplot2")
library("knitr")
library("GGally")
opts_chunk$set(echo = FALSE)
opts_chunk$set(fig.width = 5)
```


```{r}
# convert qualitative data to factors
data$cyl <- factor(mtcars$cyl)
data$vs <- factor(mtcars$vs)
data$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
data$gear <- factor(mtcars$gear)
data$carb <- factor(mtcars$carb)
```


## Exploratory analysis
For our analysis it is been used a boxplot in order to display the MPG group by transmision type. 
In figure 1, it is clear that manual transmission produces more MPG. 
Next, a pairwise graph (figure 2) was created in order to get a greater intuition of what other variables may be of interest. There is a linear relationship between MPG and each of cyl, disp, hp, drat, wt, qsec, vs, am.
It is also used a kind of heatmap of correlation matrix of all variables for better data vizualiation purposes.
The covariance was also computed (figure 3) between every variable and the positive values were noted (qsec = 0.419, vs = 0.664, am = 0.600, gear = 0.480). Then a linear model was fit on all the variables to determine which variables should be used in the final models. 

```{r, echo = FALSE}
# model using all variables as inputs(features) for our model
allvariables_model = lm(mpg ~ ., data = mtcars)

```
## Model
From the initial model, covariance test and visually inspecting the pairwise graph the following variables stood out in particular: qsec, vs, am, wt and gear. Next a stepwise model process was used in order to obtain the most significant predictors to be used. This is done by using the step function which creates multple regression models with different variables and produces list of the best predictors. As shown in figure 5, the most significant predictors in determining the MPG are cyl, hp, wt and am. The summary for this model is show in figure 6, in particular the forumla is given as: lm(formula = mpg ~ cyl + hp + wt + am, data = mtcars). This selection model yielded an R squared value of 84% (figure 6) meaning that very high percentage of variation is explained by the regression model. Next, the new model was compared with a basic model that only uses transmission type as its predictor. A p-value of 1.688e-08 was obtained (figure 7). This value is miniscule which means that the added predictors are significant to improving the model's accuracy. 

```{r}
# step wise selection process
new_model1 <- step(lm(mpg ~ ., data = mtcars), direction = "forward",trace = 0)
new_model2 <- step(lm(mpg ~ ., data = mtcars), direction = "backward",trace = 0)
```

```{r, echo = FALSE}
# compare all models
compare <- anova(allvariables_model, new_model1,new_model2,test = "F")
compare$Pr
```

## Diagnostics
The residuals from the final model are plotted below.

```{r, fig.cap = "Figure 8"}
par(mfrow=c(2, 2))
plot(new_model2)
```

- The Residuals vs Fitted plot shows no pattern between the residuals and fitted values indicating that this regression model is well fit.  
- The QQ plot shows that the points line up as expected meaning that the distribtion is normal and our model predictions are accurate.  
- In both the Scale-Location plot and the Residuals vs Leverage plots, the points are in a group with none too far from the center indicating no point had too much leverage.

## Statistical Inference
A Two Sample t-test was conducted between the different transmission types. The null hypothesis that transmission types don't have an effect on the MPG is discarded for a p-value greater than 0.05. The results are shown in figure 8. The p-value of 0.001374 and difference in  means show that manual transmission has significantly more MPG than automatic.
```{r}
t_test <- t.test(mpg ~ am, data = mtcars)
```


## Conclusions
The transmission type of a car has a significant effect on its fuel efficiency. According to the model, manual transmission, on average, has 1.81 MPG more than automatics. According to the boxplot, manual transmission has ~ 6 MPG more than automatics.

## Appendix
Description of variables  
- mpg Miles/(US) gallon  
- cyl  Number of cylinders  
- disp        Displacement (cu.in.)  
- hp        Gross horsepower  
- drat	Rear axle ratio  
- wt	Weight (lb/1000)  
- qsec	Time to drive per mile  
- vs	V or ordinary engine  
- am	Transmission (0 = automatic, 1 = manual)  
- gear	Number of forward gears  
- carb	Number of carburetors  

```{r, fig.cap = "Figure 1", fig.width = 5, fig.height = 4}
fit1 <- lm(mpg ~ am, data = mtcars)

ggplot(data, aes(x=data$am, y=data$mpg, fill=data$am)) + geom_boxplot()+
        xlab("") +
        ylab("MPG") +
        ggtitle("Boxplot of automatic and manual")+
        labs(title="MPG by Transmission type",fill="type")

```

```{r, fig.cap = "Figure 2", fig.width = 8}
p1 = pairs(mtcars, panel = panel.smooth, main = "Pairwise plot of mtcars data")
```

Figure 3
```{r, efig.cap = "Figure 3", echo = TRUE}
head(cov2cor(cov(sapply(mtcars, as.numeric))), 1)
```
Figure 4
```{r, fig.cap = "Figure 4", echo = TRUE}
everything_model = lm(mpg ~ ., data = mtcars)
everything_model$coeff
```
Figure 5
```{r, fig.cap = "Figure 5", echo = TRUE}
new_model <- step(lm(mpg ~ ., data = mtcars), trace = 0)
summary(new_model)$coef
```
Figure 6
```{r, fig.cap = "Figure 6", echo = TRUE}
new_model <- step(lm(mpg ~ ., data = mtcars), trace = 0)
new_model$coeff
```
Figure 7
```{r, echo = TRUE}
basic_model <- lm(mpg ~ am, data = mtcars)
compare <- anova(basic_model, new_model)
compare$Pr
```
Figure 8
```{r, echo = TRUE}
t_test <- t.test(mpg ~ am, data = mtcars)
t_test
```
