#1st Objective: Ideal number of clusters
library(readxl)
install.packages("NbClust")
library(NbClust)

View(Whitewine)
attach(Whitewine)
data(Whitewine)
str(Whitewine) 
head(Whitewine)

summary(Whitewine)
summary(Whitewine$quality)

y=Whitewine$quality

Whitewine$quality=NULL #remove quality column

set.seed(30) #running a random number depends on computer time, in order to reproducable result assigning any value to this function

clusterNo=NbClust(Whitewine,distance = "euclidean",min.nc = 2,max.nc = 10,method = "kmeans",index="all")

#1st Objective: K-means with the best two clusters
kc<-kmeans(Whitewine,2)
kc
table(y,kc$cluster) #checking consistency of results against coloumn 12 quality 



str(Whitewine) #pre-processing tasks-run str, data.train and summary
data.train<-scale(Whitewine[-12])
summary(data.train)

#Model fitting
set.seed(1234)
nc<-NbClust(data.train,min.nc = 2,max.nc = 10,method = "kmeans")
nc
table(nc$Best.n[1,])
#Chart shows best number is 2
barplot(table(nc$Best.n[1,]), #provide bar charts###
        xlab="Number of Clusters",
        ylab="Number of Criteria", 
        main="Number of Clusters using 30 criteria")
#SSE SCREE PLOT to look for bend or elbow
wss <-0
for (i in 1:15){
  wss[i] <-
    sum(kmeans(data.train, centers=i)$withinss)
}
plot(1:15,
     wss,
     type="b",     ### "b" for both####
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Fit the model of whitewine data to K-Means with k = 2

set.seed(1234)
fit.km <- kmeans(data.train, 2)
fit.km #show the results and interpret the results

fit.km$centers #check centers results 
fit.km$size #check size results

# visualization to see how data set is clustered
install.packages("fpc")
library(fpc)
plotcluster(data.train, fit.km$cluster)


install.packages("MASS")
library(MASS)
parcoord(data.train, fit.km$cluster)

#Evaluation 
#compare classes with clusters fitted by kmeans
confuseTable.km <- table(y, fit.km$cluster)
confuseTable.km

#compare patitions one from dataset and one from result of clustering methods
#get adjusted rand index

install.packages("flexclust")
library(flexclust)
randIndex(confuseTable.km)


