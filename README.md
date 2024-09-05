# Customer Segmentation using K means Clustering in R 
##### Author: Joshua Olisa

##### [Tableau Dashboard](https://public.tableau.com/app/profile/joshua.olisa.emodoh/viz/BellaBeats_17250435748210/Dashboard1)


## Problem Statment
The main aim is to identify the most important shopping groups based on income, age, spending score and understand the target customers in order for the marketing team to plan a strategy.

## The approach

### [Prepare](#1-prepare)
### [Process](#2-process)
### [Analyze](#3-analyze)
### [Share](#4-share)
### [Act](#5-act)


## 1. Prepare 
Data Source: 200 Records from Vijay Choudhary: [Kaggle](https://www.kaggle.com/datasets/vjchoudhary7/customer-segmentation-tutorial-in-python)


## 2. Process
[Back to Top](#author-Joshua-Olisa)


```{r process}
##### Load Libraries
library(tidyverse)

##### Load the data set
customer_data <- read.csv("Mall_Customers.csv ")
str(customer_data)

##### View the data
head(customer_data)

##### Summary of the data
summary(customer_data)
```

 
## 3. Analyze
[Back to Top](#author-Joshua-Olisa)

#### Gender distribution pie chart

```{r}
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


#### Histogram Distribution of Age, Annual Income and Spending Score
```{r}
# AGE
hist(customer_data$Age,
     col = "blue",
     xlab = "Age Class",
     ylab = "Frequncy",
     labels = TRUE)

#Anuual Income
hist(customer_data$Annual.Income..k..,
     col = "darkgreen",
     xlab = "Income Class",
     ylab = "Frequncy",
     labels = TRUE)

#Spending score 
hist(customer_data$Spending.Score..1.100.,
     col = "darkorange",
     xlab = "Spending",
     ylab = "Frequncy",
     labels = TRUE)
```

### Clustering
Clustering is a technique in machine learning that attempts to find clusters of observations within a dataset. 

K-means clustering is a technique in which we place each observation in a dataset into one of K clusters.

The end goal is to have K clusters in which the observations within each cluster are quite similar to each other while the observations in different clusters are quite different from each other.

The steps followed are 

#### 1. Find the optimal number of clusters
#### 2. Perform K-Means Clustering with Number of cluster K.

#### 1. Find the optimal number of clusters
To perform k-means clustering in R we can use the built-in kmeans() function. I used to different methods to find the optimal number of clusters to the validity of the answer. These methods are Elbow method and Gap Statistics method

##### Elbow method
```{r}
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

##### Gap Statistics Method
```{r}
library(cluster)
library(factoextra)
set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

```

The optimal number of cluster is 5 (Make bold)

## 4. Share 
[Back to Top](#author-Joshua-Olisa)

```{r echo=FALSE}
k5 <-  kmeans(customer_data[,3:5],5,iter.max = 100, nstart = 50, algorithm = "Lloyd")

#Visualizing the clustering results using the first two principle components
pccluster = prcomp(customer_data[,3:5],scale = FALSE)
summary(pccluster)
pccluster$rotation[,1:2]

# cluster of Annual Income against Spending Score
set.seed(1)
ggplot(customer_data, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) +
  geom_point(stat = 'identity', aes(color = as.factor(k5$cluster))) +
  scale_color_discrete(name = " ",
                       breaks = c("1","2","3","4","5","6"),
                       labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
  ggtitle("Segements of Mall Customers", subtitle = "Using K-means Clustering")

# cluster of Annual income against Age
ggplot(customer_data, aes(x = Annual.Income..k.., y = Age)) +
  geom_point(stat = 'identity', aes(color = as.factor(k5$cluster))) +
  scale_color_discrete(name = " ",
                       breaks = c("1","2","3","4","5","6"),
                       labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
  ggtitle("Segements of Mall Customers", subtitle = "Using K-means Clustering")

```


### [Bellabeat Data Analysis Dashboard](https://public.tableau.com/app/profile/joshua.olisa.emodoh/viz/BellaBeats_17250435748210/Dashboard1)

## 5. Act
[Back to Top](#author-Joshua-Olisa)

Conclusion based on our analysis:
- Sedentary make up a significant portion, 81% of users daily active minutes. Users spend on avg 12 hours a day in sedentary minutes, 4 hours lightly active, and only half-hour in fairly + very active! 
- Sedentary minutes took up the majority of participants’ days and were fairly consistent throughout the week.
- Saturdays, Monday and Wednesday the users take the most steps.
- Users who take more steps per day are more likely to engage in “very active minutes”


## Recommendations

Data collection: Prioritize the collection of comprehensive user data, including body fat percentage, demographic data, to improve the accuracy and depth of analysis on smart devices. Encourage users to use a wifi-connected scale instead of manual weight entries. 

Educational healthy style campaign encourages users to have short active exercises during the week, longer during the weekends, especially on Sunday where we see the lowest steps and most sedentary minutes.
