# Customer Segmentation using K means Clustering in R

##### Author: Joshua Olisa

##### [Tableau Dashboard](https://public.tableau.com/app/profile/joshua.olisa.emodoh/viz/BellaBeats_17250435748210/Dashboard1)

## Problem Statment

You wish to identify the most important shopping groups based on income, age, spending score. The goal is to understand the target customers for the marketing team to plan a strategy.

## My approach

### [Prepare](#1-prepare)

### [Process](#2-process)

### [Analyze](#3-analyze)

### [Share](#4-share)

### [Act](#5-act)

## 1. Prepare

Data Source: 200 Records from Vijay Choudhary:
[Kaggle](https://www.kaggle.com/datasets/vjchoudhary7/customer-segmentation-tutorial-in-python)

## 2. Process

[Back to Top](#author-Joshua-Olisa)

``` r
##### Load Libraries
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
##### Load the data set
customer_data <- read.csv("Mall_Customers.csv ")
str(customer_data)
```

    ## 'data.frame':    200 obs. of  5 variables:
    ##  $ CustomerID            : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Gender                : chr  "Male" "Male" "Female" "Female" ...
    ##  $ Age                   : int  19 21 20 23 31 22 35 23 64 30 ...
    ##  $ Annual.Income..k..    : int  15 15 16 16 17 17 18 18 19 19 ...
    ##  $ Spending.Score..1.100.: int  39 81 6 77 40 76 6 94 3 72 ...

``` r
##### View the data
head(customer_data)
```

    ##   CustomerID Gender Age Annual.Income..k.. Spending.Score..1.100.
    ## 1          1   Male  19                 15                     39
    ## 2          2   Male  21                 15                     81
    ## 3          3 Female  20                 16                      6
    ## 4          4 Female  23                 16                     77
    ## 5          5 Female  31                 17                     40
    ## 6          6 Female  22                 17                     76

``` r
##### Summary of the data
summary(customer_data)
```

    ##    CustomerID        Gender               Age        Annual.Income..k..
    ##  Min.   :  1.00   Length:200         Min.   :18.00   Min.   : 15.00    
    ##  1st Qu.: 50.75   Class :character   1st Qu.:28.75   1st Qu.: 41.50    
    ##  Median :100.50   Mode  :character   Median :36.00   Median : 61.50    
    ##  Mean   :100.50                      Mean   :38.85   Mean   : 60.56    
    ##  3rd Qu.:150.25                      3rd Qu.:49.00   3rd Qu.: 78.00    
    ##  Max.   :200.00                      Max.   :70.00   Max.   :137.00    
    ##  Spending.Score..1.100.
    ##  Min.   : 1.00         
    ##  1st Qu.:34.75         
    ##  Median :50.00         
    ##  Mean   :50.20         
    ##  3rd Qu.:73.00         
    ##  Max.   :99.00

## 3. Analyze

[Back to Top](#author-Joshua-Olisa)

#### Gender distribution pie chart

``` r
#### Calculate the percentage of each gender
gender_data <- customer_data %>%
  group_by(Gender) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  mutate(label = paste0(Gender, ": ", round(percentage, 1), "%"))

### Create the pie chart
ggplot(gender_data, aes(x = "", y = percentage, fill = Gender)) +    
  # x is empty to remove x-axis
  geom_bar(width = 1, stat = "identity") +                           
  # Create bar chart
  coord_polar(theta = "y") +                                         
  # Convert bar chart into a pie chart
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Pie Chart Showing the Gender Distribution") +                              # Add title
  theme_void()  
```

![](Readme-File_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

#### Histogram Distribution of Age, Annual Income and Spending Score

``` r
# AGE
hist(customer_data$Age,
     col = "blue",
     xlab = "Age Class",
     ylab = "Frequncy",
     labels = TRUE)
```

![](Readme-File_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#Anuual Income
hist(customer_data$Annual.Income..k..,
     col = "darkgreen",
     xlab = "Income Class",
     ylab = "Frequncy",
     labels = TRUE)
```

![](Readme-File_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
#Spending score 
hist(customer_data$Spending.Score..1.100.,
     col = "darkorange",
     xlab = "Spending",
     ylab = "Frequncy",
     labels = TRUE)
```

![](Readme-File_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

### Clustering

Clustering is a technique in machine learning that attempts to find
clusters of observations within a dataset.

K-means clustering is a technique in which we place each observation in
a dataset into one of K clusters.

The end goal is to have K clusters in which the observations within each
cluster are quite similar to each other while the observations in
different clusters are quite different from each other.

The steps followed are

#### 1. Find the optimal number of clusters

#### 2. Perform K-Means Clustering with Number of cluster K.

#### 1. Find the optimal number of clusters

To perform k-means clustering in R we can use the built-in kmeans()
function. I used to different methods to find the optimal number of
clusters to the validity of the answer. These methods are Elbow method
and Gap Statistics method

##### Elbow method

``` r
library(purrr)
set.seed(2)
# function to calculate total intra-cluster sum of square(euclidean distance)
ics <- function(k){
  
  kmeans(customer_data[,3:5],k,iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
}

k_values <-1:12

ics_values <- map_dbl(k_values, ics)

plot(k_values, ics_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K",
     ylab = "Total intra-cluster sum of squares")
```

![](Readme-File_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

##### Gap Statistics Method

``` r
library(cluster)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)
```

![](Readme-File_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The optimal number of cluster is 5 (Make bold)

## 4. Share

[Back to Top](#author-Joshua-Olisa)

    ## Importance of components:
    ##                            PC1     PC2     PC3
    ## Standard deviation     26.4625 26.1597 12.9317
    ## Proportion of Variance  0.4512  0.4410  0.1078
    ## Cumulative Proportion   0.4512  0.8922  1.0000

    ##                               PC1        PC2
    ## Age                     0.1889742 -0.1309652
    ## Annual.Income..k..     -0.5886410 -0.8083757
    ## Spending.Score..1.100. -0.7859965  0.5739136

![](Readme-File_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](Readme-File_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

### [Bellabeat Data Analysis Dashboard](https://public.tableau.com/app/profile/joshua.olisa.emodoh/viz/BellaBeats_17250435748210/Dashboard1)

## 5. Act

[Back to Top](#author-Joshua-Olisa)

Conclusion based on our analysis: - Sedentary make up a significant
portion, 81% of users daily active minutes. Users spend on avg 12 hours
a day in sedentary minutes, 4 hours lightly active, and only half-hour
in fairly + very active! - Sedentary minutes took up the majority of
participants’ days and were fairly consistent throughout the week. -
Saturdays, Monday and Wednesday the users take the most steps. - Users
who take more steps per day are more likely to engage in “very active
minutes”

## Recommendations

Data collection: Prioritize the collection of comprehensive user data,
including body fat percentage, demographic data, to improve the accuracy
and depth of analysis on smart devices. Encourage users to use a
wifi-connected scale instead of manual weight entries.

Educational healthy style campaign encourages users to have short active
exercises during the week, longer during the weekends, especially on
Sunday where we see the lowest steps and most sedentary minutes.
