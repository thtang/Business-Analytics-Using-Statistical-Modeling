Question 1) Let’s make an automated recommendation system for the PicCollage app.

```{r}
library(data.table)
library(dplyr)
library(lsa)
ac_bundles_dt <- fread("piccollage_accounts_bundles.csv")
ac_bundles_matrix <- as.matrix(ac_bundles_dt[, -1, with=FALSE])
```

a).Let’s explore to see if any sticker bundles seem intuitively similar:

i. Download PicCollage onto your mobile from the iOS/Android appstores and take a look at the style and content of various bundles in their Sticker Store: how many recommendations does each bundle have?

There's no recommedations in my app (Android version).

ii. Find a single sticker bundle that is both in our limited data set and also in the app’s Store 
(e.g., “sweetmothersday”) — you use your intuition to recommend (guess!) five other bundles that might have similar usage patterns as this bundle.

"親親媽咪趣" "暖暖媽咪趣" "我愛家人趣"

b). Let’s find similar bundles using geometric methods:
i. Let’s create cosine similarity based recommendations for all bundles:

1. Create a matrix or data.frame of the top 5 recommendations for all bundles.
```{r}
sim_matrix <- cosine(ac_bundles_matrix)
#str(sim_matrix)

# change the diagnal value to 100 in order to ensure the bundle itself locate in first place
diag(sim_matrix) <- 100 

recom_df <- data.frame(stringsAsFactors = F)
for (bundle in row.names(sim_matrix)){
  recom_df <- rbind(recom_df, names(sim_matrix[bundle ,order(sim_matrix[bundle,],decreasing = T)])[1:6]
                        ,stringsAsFactors = F )
}
rownames(recom_df) <- row.names(sim_matrix)
recom_df <- recom_df[,-1]
colnames(recom_df) <- c("1st","2nd","3rd","4th","5th")
#View(recom_df)
recom_df %>% head(4)

#another way to create sorted name matrix
row_reco <- t(apply(sim_matrix, 1, function(x) names(sort(x,decreasing = T) )))
```

2. Create a new function that automates the above functionality: it should take an accounts-bundles matrix as a parameter, and return a data object with the top 5 recommendations for each bundle in our data set.
```{r}
recom_mtMaker <- function(ac_bundles_matrix){
library(lsa)
sim_matrix <- cosine(ac_bundles_matrix)
diag(sim_matrix) <- 100 
recom_df <- data.frame(stringsAsFactors = F)
for (bundle in row.names(sim_matrix)){
  recom_df <- rbind(recom_df, names(sim_matrix[bundle ,order(sim_matrix[bundle,],decreasing = T)])[1:6]
                        ,stringsAsFactors = F )
}
rownames(recom_df) <- row.names(sim_matrix)
recom_df <- recom_df[,-1]
colnames(recom_df) <- c("1st","2nd","3rd","4th","5th")
return(recom_df)
}

recom_mtMaker(ac_bundles_matrix) %>% head()

```

3. What are the top 5 recommendations for the bundle you chose to explore earlier?
```{r}
recom_df["sweetmothersday",]
```

ii. Let’s create correlation based recommendations.

1. Reuse the function you created above (don’t change it; don’t use the cor() function)

2. But this time give the function an accounts-bundles matrix where each 
bundle (column) has been mean-centered in advance.

```{r}
bundles_means <- apply(ac_bundles_matrix, 2, mean)
bundles_means_matrix <- t(replicate(nrow(ac_bundles_matrix),bundles_means))
ac_bundles_mc_b <- ac_bundles_matrix - bundles_means_matrix

recom_corBased <- recom_mtMaker(ac_bundles_mc_b)
recom_corBased %>% head(4)
```




3. Now what are the top 5 recommendations for the bundle you chose to explore earlier?

```{r}
recom_corBased["sweetmothersday",]
```

iii. Let’s create adjusted-cosine based recommendations.
1. Reuse the function you created above (you should not have to change it)

2. But this time give the function an accounts-bundles matrix where each 
account (row) has been mean-centered in advance.

```{r}
accounts_means <- apply(ac_bundles_matrix, 1, mean)
accounts_means_matrix <- replicate(ncol(ac_bundles_matrix),accounts_means)
ac_bundles_mc_b <- ac_bundles_matrix - accounts_means_matrix

recom_adcorBased <- recom_mtMaker(ac_bundles_mc_b)
recom_adcorBased %>% head(4)
```


3. What are the top 5 recommendations for the bundle you chose to explore earlier?
```{r}
recom_adcorBased["sweetmothersday",]
```

c). Compare the three sets of geometric recommendations similar in nature (theme/keywords) to the recommendations you picked earlier using your intuition. Why do you suppose they are different?

I used Android version with chinese bundles names. Therefore, I can't tell whether geometric recommendations match the bundles I picked ewarly. 


Question 2) Correlation is at the heart of many data analytic methods so let’s explore it further. For each of the scenarios below, create a set of points matching the description. You might have to create each scenario a few times to get a general sense of each. Visual examples of the first four scenarios is shown below.

```{r}
source("demo_simple_regression.R")
interactive_regression()
```

a).Create a relatively narrow but flat set (horizontal) set of random points.
i. What raw slope of the x and y would you generally expect?

The slope of x on y is approximate to zero.

ii. What is the correlation of x and y that you would generally expect?

Since the points lie horizontally, I would generally expect the correlation to be zero.

b). Create a completely random set of points ranging all along the entire x-axis and y-axis (i.e., fill the plot)

i. What raw slope of the x and y would you generally expect?

About Zero.

ii. What is the correlation of x and y that you would generally expect?

About Zero. Because we randomly pointed the data on the canvas.

c). Create a diagonal set of random points trending upwards at 45 degrees

i. What raw slope of the x and y would you generally expect? (note that x, y have the same scale)

The slope should be one.

ii. What is the correlation of x and y that you would generally expect?

The values of x have the same positive trend with  y. So the correlaion should be close to one.

d). Create a diagonal set of random trending downwards at 45 degrees

i. What raw slope of the x and y would you generally expect? (note that x, y have the same scale)

Negative one. Because the trend is 45 degrees downwards.

ii. What is the correlation of x and y that you would generally expect?

x and y have an opposite trend. Therefore, the correlation might close to negative one.

e). Apart from any of the above scenarios, find another pattern of data points with no correlation (r ≈ 0).  
(challenge: can you find a scenario where the pattern visually suggests a relationship?)


f). Apart from any of the above scenarios, find another pattern of data points with perfect correlation (r ≈ 1).
(challenge: can you find a scenario where the pattern visually suggests a different relationship?)

g). Let’s find the relationship between correlation and regression

i. Run the simulation and capture the points you create: pts <- interactive_regression()
```{r}
#pts <- interactive_regression() 
#for better format the report here I hard code the data point.

pts <- data.frame(x = c(-5.393789,-4.913414,-2.191287,9.337717,17.984473,35.117856,44.084861,3.09284),
                  y = c(39.791928,33.717480,26.428143,16.911508,8.912244,3.952686,-2.121762,18.733842))
```

ii. Estimate the regression intercept and slope of pts to ensure they are the same as the values reported in the  simulation plot: summary( lm( pts$y ~ pts$x ))

```{r}
summary( lm( pts$y ~ pts$x ))
```
Yes, they're the same value as shown in above figure.

iii. Estimate the correlation of x and y to see it is the same as reported in the plot: cor(pts)
```{r}
cor(pts)
```
Yes, it is almost the same as reported in the plot.

iv. Now, re-estimate the regression using standardized values of both x and y from pts
```{r}
pts_sd <- data.frame(x=scale(pts$x), y=scale(pts$y)) 
summary( lm( pts_sd$y ~ pts_sd$x ))
```

The Intercept and slope have changed using standardized data.
```{r}
cor(pts_sd)
```
The correlation of x and y remain the same.

v. What is the relationship between correlation and the standardized regression estimates?

The regression coefficient is the correlation and the intercept is 0.
