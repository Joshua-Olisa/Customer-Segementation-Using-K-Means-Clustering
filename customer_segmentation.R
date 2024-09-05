# Load Libraries
library(tidyverse)

# Load the dataset
customer_data <- read.csv("Mall_Customers.csv")
str(customer_data)

# column names
names(customer_data)

# View the data
head(customer_data)

# Summary of the data
summary(customer_data)

# some statistical value of features
sd(customer_data$Age)
sd(customer_data$Annual.Income..k..)
sd(customer_data$Spending.Score..1.100.)

# PLOT 
#### Calculate the percentage of each gender
gender_data <- customer_data %>%
  group_by(Gender) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  mutate(label = paste0(Gender, ": ", round(percentage, 1), "%"))

### Create the pie chart
ggplot(gender_data, aes(x = "", y = percentage, fill = Gender)) +    # x is empty to remove x-axis
  geom_bar(width = 1, stat = "identity") +                           # Create bar chart
  coord_polar(theta = "y") +                                         # Convert bar chart into a pie chart
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Pie Chart Showing the Gender Distribution") +                              # Add title
  theme_void()  

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

#Clustering KMeans
#1. We specify the number of clusters that we need to create.
#2, The algorithim selects k objects at random from the datasets
#This is the initial cluster or mean
#3. The closet centroid obtains the assignment of a new observation. We base the
#assignment on the Euclidean Distance between object and the centroid
#update
#iteration until finish

#1. We specify the number of clusters that we need to create;
#Using the Elbow method 
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

#or Gap Statistic Method
set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)
 

#so go with 6
k5 <-  kmeans(customer_data[,3:5],5,iter.max = 100, nstart = 50, algorithm = "Lloyd")
#Visualizing the clustering results using the first two principle components

pccluster = prcomp(customer_data[,3:5]c,scale = FALSE)
summary(pccluster)
pccluster$rotation[,1:2]

#ploting 
set.seed(1)
ggplot(customer_data, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) +
  geom_point(stat = 'identity', aes(color = as.factor(k5$cluster))) +
  scale_color_discrete(name = " ",
                       breaks = c("1","2","3","4","5","6"),
                       labels = c("Average Spender with Average income", "Affluent High Spenders", "Low-income Low Spenders", "Budget-Conscious High Spenders", "Wealthy Low Spenders", "Cluster 6")) +
  ggtitle("Segements of Mall Customers", subtitle = "Using K-means Clustering")

###
ggplot(customer_data, aes(x = Annual.Income..k.., y = Age)) +
  geom_point(stat = 'identity', aes(color = as.factor(k5$cluster))) +
  scale_color_discrete(name = " ",
                       breaks = c("1","2","3","4","5","6"),
                       labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
  ggtitle("Segements of Mall Customers", subtitle = "Using K-means Clustering")

###
ggplot(customer_data, aes(x = Spending.Score..1.100., y = Age)) +
  geom_point(stat = 'identity', aes(color = as.factor(k5$cluster))) +
  scale_color_discrete(name = " ",
                       breaks = c("1","2","3","4","5","6"),
                       labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
  ggtitle("Segements of Mall Customers", subtitle = "Using K-means Clustering")
















