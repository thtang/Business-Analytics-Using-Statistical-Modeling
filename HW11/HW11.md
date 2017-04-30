Question 1) Let's deal with non-linearity first. Create a new dataset that log-transforms several variables from our original dataset (called cars in this case):


```{r}
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?", stringsAsFactors = F)
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight",
"acceleration", "model_year", "origin", "car_name")
cars_log <- with(auto, data.frame(log(mpg), log(cylinders), log(displacement), log(horsepower), log(weight), log(acceleration), model_year, origin))

```
a). Run a new regression on the cars_log dataset, with mpg.log. dependent on all other variables
```{r}
regr <- lm(log.mpg. ~ log.cylinders.+log.displacement.+log.horsepower.+log.weight.+log.acceleration.+model_year+factor(origin), data = cars_log)
summary(regr)
```

i. Which log-transformed factors have a significant effect on log.mpg. at 10% significance?

**log.horsepower., log.weight., log.acceleration., model_year, origin**

ii. Do some new factors now have effects on mpg, and why might this be?

**Horsepower, accerleration** become significant factor after log-transforming. Before that, they are non-linear correlated to mpg, which would not meet the assumption of regression.

iii. Which factors still have insignificant or opposite effect on mpg? Why might this be?

**Cylinders and displacement** still have no significant effect on mpg. I think they shared high multicollinearity with other variable like **weight and horsepower** thus become insignificant.


b). Let's take a closer look at weight, because it seems to be a major explanation of mpg
i. Create a regression (call it regr_wt) of mpg on weight from the original cars dataset
```{r}
regr_wt <- lm(mpg ~ weight, data = auto)
summary(regr_wt)
```

ii. Create a regression (call it regr_wt_log) of log.mpg. on log.weight. from cars_log
```{r}
regr_wt_log <- lm(log.mpg. ~log.weight., data = cars_log)
summary(regr_wt_log)
```

iii. Visualize the residuals of both regressions (raw and log-transformed):
1. density plots of residuals
```{r}
plot(density(regr_wt$residuals), col = 2,ylim = c(0,3),main = "Residual plot")
lines(density(regr_wt_log$residuals),col = 3)
legend("topright",c("weight error","log weight error"),lty=c(1,1),col = c(2,3))
```

2. scatterplot of log.weight. vs. residuals

```{r}
plot(cars_log$log.weight.,regr_wt_log$residuals,col="steelblue2",pch = 16,
     xlab="log.weight", ylab = "residuals", main = "Scatterplot of log.weight. vs. residuals")
```


iv. Which regression produces better residuals for the assumptions of regression?

Assumptions of regression requires error terms to be random and normally distributed with zero mean. From the above two figures, we can find out the regression using log.weight. as independent variable perform well. The errors lie extremely centralized with randomness.

v. How would you interpret the slope of log.weight. vs log.mpg. in simple words?

**1% change in weight leads to -1.0583% change in mpg.**

c). What is the 95% confidence interval of the slope of log.weight. vs. log.mpg.?
i. Create a bootstrapped confidence interval
```{r}
plot(cars_log$log.weight.,cars_log$log.mpg., col=rgb(0.5, 0.7, 0.3, 0.5), 
     pch=19, main = "Bootstrapped Confidence Intervals")
boot_regr<-function(model, dataset) {
  boot_index<-sample(1:nrow(dataset), replace=TRUE)
  data_boot<-dataset[boot_index,]
  regr_boot<-lm(model, data=data_boot)
  abline(regr_boot, lwd=1, col=rgb(0.7, 0.7, 0.7, 0.5))
  return(regr_boot$coefficients)
}
coeffs<-replicate(300, boot_regr(log.mpg. ~ log.weight., cars_log))
abline(a=mean(coeffs["(Intercept)",]), b=mean(coeffs["log.weight.",]), lwd=2)
```

ii. Verify your results with a confidence interval using traditional statistics
```{r}
quantile(coeffs["log.weight.",], c(0.025,0.975))
```

```{r}
plot(density(coeffs["log.weight.",]),xlim = c(-1.5,0),col="cornflowerblue",lwd=2,
     main = "slope density plot")
abline(v=quantile(coeffs["log.weight.",], c(0.025,0.975)))

print(confint(regr_wt_log)) # another function to show the coefficient.
```

After finding out the 95% confidence interval of the slope, we can claim that the coefficient is significant since our result is -1.0583, which lies between 95% confidence interval.

Question 2) Let's tackle multicollinearity next. Consider the regression model:

```{r}
regr_log <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. +
                          log.weight. + log.acceleration. + model_year +
                          factor(origin),  data=cars_log)
```
a). Using regression and R2, calculate the VIF of log.weight. as demonstrated in class
```{r}
log_weight <- lm(log.weight. ~ log.cylinders. + log.displacement. 
                 + log.horsepower. +log.acceleration. + model_year +
                   factor(origin),  data=cars_log)
# R_square
r2_weight <- summary(log_weight)$r.squared
r2_weight

vif_weight <- 1/ (1-r2_weight)
vif_weight
```

log.weight. shares more than half its variance with other independent variables.

b). Let's try a procedure called Stepwise VIF Selection  to remove highly collinear variables. Start by Installing the "car"" package in RStudio -- it has a function called vif() 

i. Use vif(regr_log) to compute VIF of the all the independent variables
```{r}
library(car)
vif(regr_log)
```

ii. Remove the independent variable with the largest VIF score greater than 5 from the model
```{r}
source("vif_func.R")
vif_func(cars_log[,-1],trace = T, thresh = 5)
```

We removed **displacement, horsepower and cylinders**.

iii. Repeat steps (i) and (ii) until no more independent variables have large VIF scores

From the above, after removing the variables that have large VIF scores, ** log.weight., log.accerleration., model_year** and **origin** remain.

iv. Report the final regression model and its summary statistics
```{r}
regr_final <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor(origin),
                 data  = cars_log)
summary(regr_final)
```

```{r}
plot(cars_log$log.mpg.,regr_final$fitted.values,col="deeppink1",pch = 16,
     xlab="true og.mpg", ylab = "predictive log.mpg", main = "Scatterplot of true log.weight. vs. predictive log.mpg")
```

c). Using stepwise VIF selection, have we lost any variables that were previously significant?  
If so, was it reasonable to drop those variables? (hint: look at model fit)

Stopwise VIF selection drops variable **horesepower**, which was previosly significant, since it has high VIF value. Comparing to the adjust R-square of full and selected version regression, I think it's reasonable because they are almost the same. In selected version, we used even much fewer variables!

d). General questions on VIF:

i. If an independent variable has no correlation with other independent variables, what would its VIF score be?

If no correlation, R-squared would be zero. Due to the formula of VIF, it turns out that VIF become 1 (VIF=1) .

ii. Given a regression with only two independent variables (X1 and X2), how correlated would X1 and X2 have to be, to get VIF scores of 5 or higher? To get VIF scores of 10 or higher?

correlation between them above **0.894** could make VIF scores of 5 or higher.
correlation between them above **0.949** could make VIF scores of 10 or higher.


Question 3) Might the relationship of weight on mpg be different for cars from different origins? Let’s try visualizing this. First, plot all the weights, using different colors and symbols for the three origins:
```{r}
library(ggplot2)
cars_log$country <- factor(cars_log$origin,labels=c("USA","Europe","Japan"))
ggplot(cars_log,aes(x= log.weight., y = log.mpg.,col = factor(country)))+
  geom_point()+
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title=element_blank())
```

a). Let’s add three separate regression lines on the scatterplot, one for each of the origins:

```{r}
three_regr <- ggplot(cars_log,aes(x= log.weight., y = log.mpg.,col = factor(country)))+
  ggtitle("Regression lines for each origin")+
  geom_point()+
  geom_smooth(aes(log.weight.,log.mpg.,color=factor(country)),method=lm,se=TRUE)+
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title=element_blank())
three_regr
```

b). [not graded] Do cars from different origins appear to have different weight vs. mpg relationships?

```{r}
three_regr+facet_wrap(~factor(country),scales = "free")
```

Here I separated three countries, and it's obvious that they have different amounts of data points. BOth Europe and Japan show higher standard deviation of regression lines because they have fewer data. In terms of relationships between log.weight. and log.mpg., I don't think there's significant difference between each origin. On another perspective, number of data points might affect the comparison.
