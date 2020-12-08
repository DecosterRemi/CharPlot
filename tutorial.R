library(ClusterCharPlot)


library(readxl)
library(stats)

#Getting data from the sample in the package
df<- read_excel('Autos.xlsx')
X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
clus<- kmeans(X,3)
y <-clus$cluster

sep = sep_data(df)
num = df[sep[[2]]]
num
clus<- kmeans(num,3)
y <-clus$cluster
####Pre-processing####
#transform data
######################
#group = kmeans(df[sep_data(df)[[2]]],4) #clustering example
#y <- group$cluster

##Analyse univarié
#Création de l'objet de la classe UniChar
obj <- Perform_UniChar(df,y)
plot(obj)

#Qualitative
vCramer <- vcramer.UniChar(obj,y)
print(vCramer)
vCramer["label"] <- obj$catnames
colnames(vCramer[1])
barplt.UniChar(obj, vCramer, TRUE)

vtest.UniChar(obj,y, 'make')
#unable to plot yet

#Quantitative
#Correlation, caracterisation des partitions
cor <- correlation(obj,y)
print(cor)

tmp <- as.data.frame(cor)
(correlations <- t(tmp[4,]))
print(t(correlations))
ClusterCharPlot::barplt.UniChar(obj, correlations)
obj$colnames

#Valeur test, caracterisation des groupes
vt <- valuetest(obj,y)
print(vt)
radar(vt)

#Effect size, caracterisation des groupes
es <- effectsize(obj,y)
print(es)
radar(es)
