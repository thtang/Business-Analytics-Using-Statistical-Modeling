Question 1) Model fit is often determined by R 2 so let’s dig into what this perspective of model fit is all about.Download demo_simple_regression_sq.R from Canvas – it has a function that runs a regression simulation.This week, the simulation also reports R 2 along with the other metrics from last week.

Scenario 1: Consider a very narrowly dispersed set of points that have a negative or positive steep slope

Scenario 2: Consider a widely dispersed set of points that have a negative or positive steep slope

Scenario 3: Consider a very narrowly dispersed set of points that have a negative or positive shallow slope

Scenario 4: Consider a widely dispersed set of points that have a negative or positive shallow slope

a). Let’s dig into what regression is doing to compute model fit:

i. Plot Scenario 2, storing the returned points: pts <- interactive_regression_rsq()
ii. Run a linear model of x and y points to confirm the R2 value reported by the simulation
iii. Add line segments to the plot to show the regression residuals (errors) Get the values of y( regression line’s estimates of y, given x): y_hat <- regr$fitted.values Add segments: segments(pts$x, pts$y, pts$x, y_hat, col="red", lty="dotted")
iv. Use only pts$x, pts$y, y_hat and mean(pts$y) to compute SSE, SSR and SST, and verify R2 

```{r}
#pts <- interactive_regression_rsq()
pts <- data.frame(x = c(-0.2042956,0.2491857,11.7373779,15.6675489,34.5626018,23.6790513,45.7484731, 35.3184039,44.0857085,11.5862174), y=c(1.960052 ,13.654639 ,10.465206 ,23.648196 ,27.050258, 39.595361, 46.612113 ,48.525773, 33.003866 , 1.747423))
regr <- lm(y~x ,data = pts)
regr_summary <- summary(regr)
regr_summary
```
```{r}
y_hat <- regr$fitted.values
plot(pts, xlim=c(-5,50), ylim=c(-5,50), pch=19, cex=2, col="gray")
abline(regr, lwd=2, col="cornflowerblue")
text(1, 50, paste(c("Raw intercept: ", round(regr$coefficients[1], 2)), collapse=" "), cex=0.80)
text(1, 45, paste(c("Raw slope    : ", round(regr$coefficients[2], 2)), collapse=" "), cex=0.80)
text(1, 40, paste(c("Correlation  : ", round(cor(pts$x, pts$y), 2)), collapse=" "), cex=0.80)
text(1, 35, paste(c("R-squared    : ", round(regr_summary$r.squared, 2)), collapse=" "), cex=0.80)
segments(pts$x, pts$y, pts$x, y_hat, col="red", lty="dotted")
```

```{r}
model_fit_error <- function(pts){
  regr <- lm(y~x ,data = pts)
  y_hat <- regr$fitted.values
  SSE <- sum((pts$y - y_hat)^2)
  SSR <- sum((y_hat - mean(pts$y))^2)
  SST <- sum((pts$y-mean(pts$y))^2)
  R_sq <- SSR/SST
  return(data.frame(SSE=SSE,SSR=SSR,SST=SST,R_square=R_sq))
}
model_fit_error(pts)
```

b). Comparing scenarios 1 and 2, which do we expect to have a stronger R2 ?

Scenarios 1 might have a stronger R2.

c). Comparing scenarios 3 and 4, which do we expect to have a stronger R2 ?

Scenarios 3 might have a stronger R2.

d). Comparing scenarios 1 and 2, which do we expect has bigger/smaller SSE, SSR, and SST?

Scenarios 2 has bigger SSE, smaller SSR (depends on the slope) and bigger SST.

e). Comparing scenarios 3 and 4, which do we expect has bigger/smaller SSE, SSR, and SST?

Scenarios 4 has bigger SSE, smaller SSR (depends on the slope) and bigger SST.

Question 2) We’re going to take a look back at the heady days of car manufacturing, when American, Japanese, and European cars competed to rule the world. Take a look at a data set (auto-data.txt). We are interested in explaining what kind of cars have higher fuel efficiency (measured by mpg ).

Variable description:
1. mpg: miles-per-gallon (dependent variable)
2. cylinders: cylinders in engine
3. displacement: size of engine
4. horsepower: power of engine
5. weight: weight of car
6. acceleration: acceleration ability of car
7. model_year: year model was released
8. origin: place car was designed (1: USA, 2: Europe, 3: Japan)
9. car_name: make and model names

a). Let’s first try exploring this data and problem:

i. Visualize the data in any way you feel relevant (report only relevant/interesting ones)
```{r}
#load daat
library(dplyr)
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?", stringsAsFactors = F)
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight",
"acceleration", "model_year", "origin", "car_name")
auto <- auto %>% mutate(brand = unlist(strsplit(car_name," "))[1])
auto$brand <- as.character(lapply(auto$car_name,function(x){unlist(strsplit(x," "))[1]}))
auto$country <- factor(auto$origin,labels=c("USA","Europe","Japan"))
#visualization
library(ggplot2)
ggplot(data = auto,aes(x = weight,y=mpg, label = brand , color =country))+
  geom_text(check_overlap = TRUE)+
  ggtitle("mpg vs weight \nwith car's brand")
```

Here I used scatter plot to discover the relationship between mpg and weight. The colored words represent the car brands and where it was made. We can easily find out that american cars tend to be heavier and lower feul efficiency. The most important is there exists negative relationship between mpg and weight!

ii. Report a correlation table of all variables, rounding to two decimal places
(in the cor(...) function, set use="pairwise.complete.obs" to handle missing values)
```{r}
cor_table <- cor(auto[1:8], use = "pairwise.complete.obs") %>% round(2)
cor_table
```

iii. From the visualizations and correlations, which variables seem to relate to mpg ?

According to the correlation table, **cylinders, displacement, horsepower, weight, model_year** seem to relate to mpg. Among them, weight might be the most related variables. Notice that most of the variables tend to have negative relationship with mpg!

iv. Which relationships might not be linear? (don’t worry about linearity for rest of this HW)

```{r}
library(lattice)
splom(~auto[1:8])
```

Among those relationship, displacement, horsepower, weight with mpg might not be linear. From the above table, we can see that those variables construct a moon-liked shape insteat of straight line shape.


v. Are any of the independent variables highly correlated ( r > 0.7 ) with others?
```{r}
library(reshape2)
diag(cor_table) <- 0
cor_melt <- melt(cor_table)
hight_cor <- cor_melt[order(abs(cor_melt$value),decreasing = T) & abs(cor_melt$value) >0.7,]

#### eliminate the same combination of variable 
#sort two variable by first character order
hight_cor[1:2] <- t( apply(hight_cor[1:2], 1, sort) )
#eliminate the same variable combination
hight_cor[!duplicated(hight_cor[1:2]),]
```

The above table shows the high correlated variables pair.

b).Let’s try an ordinary linear regression, where mpg is dependent upon all other suitable variables(Note: origin is categorical with three levels, so use factor(origin) in lm(...) to split it into two dummy variables)
```{r}
regr <- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+model_year+factor(origin)
           ,data = auto)
summary(regr)
```

i. Which factors have a ‘significant’ effect on mpg at 1% significance?

**Intercept, displacement, weight, model_year, and origin** have significant effects on mpg at 1% significance.

ii. Looking at the coefficients, is it possible to determine which independent variables are the most effective at increasing mpg? If so, which ones, and if not, why not? (hint: units!)

Nope! Because those variable are in different scales, it's unreasonable to compare their magnitude directly. We should standardize them before constructing the regression model.




c). Let’s try to resolve some of the issues with our regression model above.

i. Create fully standardized regression results: are these values easier to interpret?
(note: consider if you should standardize origin )

```{r}
auto_sd <- cbind(scale(auto[1:7]),auto$origin)
colnames(auto_sd) <- colnames(auto[1:8])
auto_sd <- as.data.frame(auto_sd)
regr_sd <- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+model_year+factor(origin)
           ,data = auto_sd)
summary(regr_sd)
```

After standarization, it make it earilier to interpret the coefficient. According the summary report, we can find out that **weight** is the most effective at increasing mpg. (e.g., by decreasing the **weight** of car, **mpg** might greatly increase.)

ii. Regress mpg over each nonsignificant independent variable, individually. Which ones are significant if we regress mpg over them individually?

```{r}
#function to create regression plot
ggplotRegression <- function (fit) {
require(ggplot2)
ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 2),
                     "Intercept =",signif(fit$coef[[1]],2 ),
                     " Slope =",signif(fit$coef[[2]], 2),
                     " P =",signif(summary(fit)$coef[2,4], 2)))
}
ggplotRegression(lm(mpg ~ cylinders,data = auto_sd))
```
```{r}
ggplotRegression(lm(mpg ~ horsepower,data = auto_sd))
```
```{r}
summary(lm(mpg ~ acceleration,data = auto_sd))
ggplotRegression(lm(mpg ~ acceleration,data = auto_sd))
```

The above figures show the regression line for three nonsignificant independent variable, **cylinders, horsepower, acceleration**. According to the p-value of each regression, all of them are significant if we regress mpg over them individually.

iii. Plot the density of the residuals: are they normally distributed and centered around zero?
(hint: get the residuals of a linear model, e.g. regr <- lm(...) , using regr$residuals
```{r}
regr_sdf <- fortify(regr_sd)
ggplot(regr_sdf,aes(.resid))+
  geom_density()
```

Yes, they're normally distributed and centered around zero.
