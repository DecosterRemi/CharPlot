library(rlist)
library(FactoMineR)
library(readxl)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(ggforce)
library(ggrepel)
library(caret)
library(e1071)

circleFun <- function(center = c(0,0),diameter = 2, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

correlation_circle <- function(X,data,clusters_name){
  
  # Creation of empty lists
  liste_graph=vector(mode='list',length = length(data)+1)
  liste_corrc=vector(mode='list',length = length(data)+1)
  liste_corr_carr=vector(mode='list',length = length(data)+1)
  
  # PCA for the whole data
  res.acp=princomp(X)
  eval_norm=res.acp$sdev/sqrt(nrow(X))
  eval_norm
  corr=as.data.frame(eval_norm*res.acp$loadings[])
  
  # Squared Correlation
  corr=corr[,1:2]/sqrt(corr[,1]^2+corr[,2]^2)
  corr_carr=corr^2
  corr_carr$CTR1=corr_carr$Comp.1/(eval_norm[1])^2
  corr_carr$CTR2=corr_carr$Comp.2/(eval_norm[2])^2
  corr_carr <- corr_carr[,c(1,3,2,4)]
  
  liste_corrc[[1]]=corr
  liste_corr_carr[[1]]=corr_carr
  
  # PCA for each class
  for (i in 1:length(data)) {
    res.acp=princomp(data[[i]])
    eval_norm=res.acp$sdev/sqrt(nrow(X))
    corr=as.data.frame(eval_norm*res.acp$loadings[])
    corr=corr[,1:2]/sqrt(corr[,1]^2+corr[,2]^2)
    
    corr=corr[,1:2]/sqrt(corr[,1]^2+corr[,2]^2)
    corr_carr=corr^2
    corr_carr$CTR1=corr_carr$Comp.1/(eval_norm[1])^2
    corr_carr$CTR2=corr_carr$Comp.2/(eval_norm[2])^2
    corr_carr <- corr_carr[,c(1,3,2,4)]
  
    liste_corrc[[i+1]]=corr
    liste_corr_carr[[i+1]]=corr_carr
  }
  
  
  names(liste_corrc) <- c('All',clusters_name)
  names(liste_corr_carr) <- c('All',clusters_name)
  class(liste_corrc) <- "list.corr"
  class(liste_corr_carr) <- "list.corr_carr"

  return(list(list_corr=liste_corrc,list_corr_carr=liste_corr_carr))
}

multivariate_charac <- function(X,y,y_true=NULL,metric='euclidian'){

  #splitting the data according to clustering
  data<-list()
  clusters_name<-sort(unique(y))
  for (e in clusters_name){
    data=list.append(data,X[y==e,])
  }

  #centers of the clusters
  mean_data=c()
  for (e in data){
    mean_data=list.append(mean_data,as.matrix(sapply(e,mean)))
  }
  mean_data=matrix(mean_data,nrow = length(clusters_name))

  #distance matrix
  dm<-dist(mean_data,diag=T,method=metric)
  #rownames(dm) <- clusters_name
  #colnames(dm) <- clusters_name

  temp<-correlation_circle(X,data,clusters_name)
  list_corr=temp$list_corr
  list_corr_carr=temp$list_corr_carr
  
  cm=c(0)
  # Confusion matrix if the true clustering is given
  if (!is.null(y_true)){
    cm=confusionMatrix(y,y_true)
  }
  class(cm) <- "confusion.matrix"

  multivariate <- list(X=X,y=y,y_true=y_true,clusters_name=clusters_name,distance_matrix=dm,correlation=list_corr,squared_correlation=list_corr_carr,confusion_matrix=cm)
  class(multivariate) <- "multivariate"
  return(multivariate)

}

print.confusion.matrix <- function(cm){
  if (length(cm)==1){
    cat('This confusion matrix is NULL. This may be due to the fact that you did not give y_true.')
  } else {
    cm_b=cm
    class(cm_b) <- 'confusionMatrix'
    print(cm_b)
  }
}


plot.list.corr <- function(list_corr){
  
  #Adding the center of the arrows
  for (i in 1:length(list_corr)) {
    list_corr[[i]]$center=numeric(nrow(list_corr[[i]]))
  }
  
  #CREATION OF THE GRAPHS
  circle <- circleFun()
  color=c('black','red','green','blue')

  for (i in 1:length(list_corr)) {
    p <- ggplot(circle,aes(x,y))+geom_path()+
      geom_segment(aes(x=center,y=center,xend=Comp.1,yend=Comp.2),data=list_corr[[i]],arrow = arrow(length = unit(0.5,"cm")))+
      geom_label_repel(data=list_corr[[i]], aes(x=Comp.1,y=Comp.2, label = rownames(list_corr[[i]])),colour = "white", fontface = "bold",fill=color[i])
    
    liste_graph[[i]]=p 
  }
  do.call("grid.arrange",c(liste_graph,ncol=2))
}


setwd('C:/Users/romai/Documents/PHYTEM/2020-2021/Cours/Programmation en R/Projet/')
df<- read_excel('Autos.xlsx')
X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
clus<- kmeans(X,3)
y <-clus$cluster

multivariate=multivariate_charac(X,y)
print(multivariate$distance_matrix)
print(multivariate$confusion_matrix)
print(multivariate$correlation)
print(multivariate$squared_correlation)
plot(multivariate$correlation)
