library(readxl)
library(stats)

#Getting data from the sample in the package
df<- read_excel('Autos.xlsx')
X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
clus<- kmeans(X,3)
y <-clus$cluster

multivariate=multivariate_charac(X,y)

#Printing and plotting the results of multivariate_charac
print(multivariate$distance_matrix)
print(multivariate$confusion_matrix)
print(multivariate$correlation)
print(multivariate$squared_correlation)
plot(multivariate$correlation)