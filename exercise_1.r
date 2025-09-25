library(rattle)
library(car)
library(tidyverse)
library(factoextra)
library(cluster)
#Loading the dataset.
cities <- read.csv("C:/Users/Nicko/Desktop/stat_ml/exercise_1/cities.csv")

#Attach each column to use them later easily.
attach(cities)

cities


View(cities)

work <- as.numeric(work)
pollution <- as.numeric(pollution)
sunshine <- as.numeric(sunshine)
class(work)
mean(work)
mean(sunshine)


library(PerformanceAnalytics)
chart.Correlation(cities[-1])

#i
cities[-1]
pairs(cities[-1])
par(mfrow=c(3,4))
for (i in 1:7) hist(cities[,i+1], main = names(cities)[i+1])

scatterplotMatrix(cities[2:6])


#LDA and QDA aren't able to solve our issue, as
#they are used in a supervised level.

#We'll first try to use the K-Means algorithm


km.cities=kmeans(cities[-1], 4, nstart = 10)  #we use cities[-1] because we want to exclude the city names from the algorithm.
km.cities$centers
cl=km.cities$cluster
cl
cbind(cities[-1], km.cities$cluster)
dev.new(12,12)
pairs(cities[-1], col=cl)

dev.new(12,12)
plot(cities[,c(2,7)], col=(cl+1), main = "K-Means clustering results with K=4",
     xlab="", ylab="", pch = 20, cex = 2)

#The distribution of our data isn't spherical, so our clusters have values that
#aren't clear to which cluster they can belong.
#We can see that the results taken from the K-Means algorithm aren't that clear.
#Consequently, we'll use Hierarchical Clustering with the help of other methods
#to find the optimal number of clusters.

df = cities[-1]

hcs = hclust(dist(df), method = "single")
hcc = hclust(dist(df), method = "complete")
hca = hclust(dist(df), method = "average")
hcw = hclust(dist(df), method = "ward.D2")



dev.new(12,12)
par(mfrow = c(2, 2))
plot(hcs, main = "Single linkage", xlab = "", sub = "", cex = 0.9, hang = -1)
plot(hcc, main = "Complete linkage", xlab = "", sub = "", cex = 0.9, hang = -1)
plot(hca, main = "Average linkage", xlab = "", sub = "", cex = 0.9, hang = -1)
plot(hcw, main = "Ward", xlab = "", sub = "", cex = 0.9, hang = -1)

chcs = cutree(hcs, 3)
chcc = cutree(hcc, 3)
chca = cutree(hca, 3)
chcw = cutree(hcw, 3)



summary(silhouette(chcs, dist(df)))$avg.width
summary(silhouette(chcc, dist(df)))$avg.width
summary(silhouette(chca, dist(df)))$avg.width
summary(silhouette(chcw, dist(df)))$avg.width

dev.new(12,12)
par(mfrow = c(2, 2))
plot(hcs, main = "Single linkage", xlab = "", sub = "", cex = 0.9, hang = -1)
rect.hclust(hcs, k = 3, border = 2:3)
plot(hcc, main = "Complete linkage", xlab = "", sub = "", cex = 0.9, hang = -1)
rect.hclust(hcc, k = 3, border = 2:3)
plot(hca, main = "Average linkage", xlab = "", sub = "", cex = 0.9, hang = -1)
rect.hclust(hca, k = 3, border = 2:3)
plot(hcw, main = "Ward", xlab = "", sub = "", cex = 0.9, hang = -1)
rect.hclust(hcw, k = 3, border = 2:3)


dev.new(12,12)
par(mfrow=c(2,1))
fviz_nbclust(df, kmeans, "wss")
fviz_nbclust(df, hcut, "silhouette")
fviz_nbclust(df, hcut, "wss")

#We bind the results from hca with the ward method to our dataset.

cities_ward <- cbind(cities, chcw)
cities_ward

#We try to see if the clustering is correct by weatching the 
#values and correlation between each variable given.
cl_cities <- cbind(city, chcw)
cl_cities <- cbind(cl_cities, sunshine, pollution)
places_act <- cbind(city, chcw, places, activities)
places_act <- cbind(places_act, pollution)
places_act <- cbind(places_act, work)
places_act

