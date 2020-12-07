require(readxl)
require(stats)

#Getting data from the sample in the package
df<- read_excel('Autos.xlsx')
X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
clus<- kmeans(X,3)
y <-clus$cluster

measures=indexes(X,y)

#Printing and plotting the results of indexes
print(measures$internal)
print(measures$external)
plot(measures)