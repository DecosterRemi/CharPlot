
#' `circleFun` returns the x and y coordinates of a circle.
#'
#' @param center Vector with the coordinates of the center. Default value is (0,0)
#' @param diameter Diameter of the circle. Default value is 2.
#' @param npoints Number of points used to create the circle. Default value is 100.
#'
#' @return data.frame with the coordinates of the points of the circle.
#'
#' @import stats
#'
#' @export
#'
#' @examples
#' circle <- circleFun()
#' plot(circle)
#'
circleFun <- function(center = c(0,0),diameter = 2, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


#' Creation of the list of correlations and the list of squared correlations.
#'
#' This function is used in `multivariate_charac()`.
#'
#' @param X data.frame with the predictive variable that were used for the clustering
#' @param data List of data.frame for each clusters.
#' @param clusters_name Vector of the names of the clusters
#'
#' @return List with $list_corr (list of correlations) and $list_corr_carr (list of squared correlations).
#' @import stats
#'
#' @export
#'
#' @examples
#' require(readxl)
#' df<- read_excel('../Autos.xlsx') #specify the path
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#'
#' data<-list()
#' clusters_name<-sort(unique(y))
#' for (e in clusters_name){
#'   data=list.append(data,X[y==e,])
#' }
#'
#' correlation_circle(X,data,clusters_name)
#'
correlation_circle <- function(X,data,clusters_name){

  # Creation of empty lists
  liste_corrc=vector(mode='list',length = length(data)+1)
  liste_corr_carr=vector(mode='list',length = length(data)+1)

  # PCA for the whole data
  res.acp=princomp(X)
  eval_norm=res.acp$sdev/sqrt(nrow(X))
  eval_norm
  corr=as.data.frame(eval_norm*res.acp$loadings[])

  # Squared Correlation
  corr=corr[,1:2]/sqrt(corr[,1]^2+corr[,2]^2) #normalization of the eigenvectors
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



#' Multivariate characterization of the clustering
#'
#' @param X data.frame with the predictive variable that were used for the clustering
#' @param y data.frame (or vector) with the same number of rows (or length) as `X`. This represents the predicted classes.
#' @param y_true data.frame (or vector) with the same number of rows (or length) as `X`. This represents the predicted classes.
#' @param metric Metric used for the distance matrix. Can be chosen among : "euclidean", "maximum", "manhattan", "canberra", "binary" and "minkowski". Default value is "euclidean".
#'
#' @return multivariate - object containing descriptors of the multivariate characterization.
#' @details multivariate objects have the following attributes :
#' \itemize{
#'  \item{"x"}{data.frame of the predictive variables.}
#'  \item{"y"}{data.frame or vector of the clustering.}
#'  \item{"y_true"}{(Optionnal) data.frame or vector of the true clustering.}
#'  \item{"clusters_name"}{Vector of the names of the clusters.}
#'  \item{"distance_matrix"}{Matrix of the distances between the center of clusters.}
#'  \item{"correlation"}{List of the correlations of the whole data and in each cluster.}
#'  \item{"squared_correlation"}{List of the squared correlations of the whole data and in each cluster.}
#'  \item{"confusion_matrix"}{If y_true was provided, this is the confusionMatrix between the predicted clustering and the real one.}
#' }
#'
#' @import stats caret e1071 rlist
#'
#' @export
#'
#' @examples
#' require(readxl)
#' df<- read_excel('../Autos.xlsx') #specify the path
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#' ## Creating a clustering using k-means with k=3
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' multivariate=multivariate_charac(X,y)
#' summary(multivariate)
#'
multivariate_charac <- function(X,y,y_true=NULL,metric='euclidean'){

  clusters_name<-sort(unique(y))
  #splitting the data according to clustering
  data<-vector(mode='list',length = length(clusters_name))

  for (i in 1:length(clusters_name)){
    data[[i]]=X[y==clusters_name[i],]
  }

  #centers of the clusters
  mean_data<-c()
  for (i in 1:length(data)){
    mean_data=list.append(mean_data,as.matrix(sapply(data[[i]],mean)))
  }
  mean_data=matrix(mean_data,nrow = length(clusters_name))

  #distance matrix
  dm<-dist(mean_data,diag=T,method=metric)

  temp<-correlation_circle(X,data,clusters_name)
  list_corr=temp$list_corr
  list_corr_carr=temp$list_corr_carr

  cm=c(0)
  # Confusion matrix if the true clustering is given
  if (!is.null(y_true)){
    cm=confusionMatrix(y,y_true)
  }
  class(cm) <- "confusion.matrix"

  multivariate <- list(X=X,y=y,y_true=y_true,clusters_name=clusters_name,clusters=data,distance_matrix=dm,correlation=list_corr,squared_correlation=list_corr_carr,confusion_matrix=cm)
  class(multivariate) <- "multivariate"
  return(multivariate)

}


#' Print for confusion.matrix object
#'
#' @param cm confusion.matrix object
#'
#' @return None
#' Prints an Error message if the true clustering was not given in `multivariate_charac()`.
#' Otherwise, it prints the confusion matrix using the same method as the class confusionMatrix.
#'
#' @export
#'
#' @examples
#' require(readxl)
#' df<- read_excel('../Autos.xlsx') #specify the path
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' multivariate=multivariate_charac(X,y)
#' print(multivariate$confusion_matrix)
#'
print.confusion.matrix <- function(cm){
  if (length(cm)==1){
    cat('This confusion matrix is NULL. This may be due to the fact that you did not give y_true.')
  } else {
    cm_b=cm
    class(cm_b) <- 'confusionMatrix'
    print(cm_b)
  }
}


#' Plotting graphs to compare the correlation
#'
#' Plotting the correlations in the whole dataset and within each cluster in order to find characteristic specific to clusters.
#'
#' @param list_corr list.corr object - List of the correlations of the whole data and in each cluster.
#'
#' @return None
#'
#' @export
#'
#' @import ggplot2 ggrepel ggforce gridExtra
#'
#' @examples
#' require(readxl)
#' df<- read_excel('../Autos.xlsx') #specify the path
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#' ## Creating a clustering using k-means with k=3
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' multivariate=multivariate_charac(X,y)
#' plot(multivariate$correlation)
#'
plot.list.corr <- function(list_corr){

  liste_graph=vector(mode='list',length = length(data)+1)

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


#' Summary of the object multivariate
#'
#' Prints various attributes and plot the correlation of multivariate.
#'
#' @param multivariate object obtained as the output of `multivariate_charac`
#'
#' @return None
#' @export
#'
#' @examples
#' require(readxl)
#' df<- read_excel('../Autos.xlsx') #specify the path
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#' ## Creating a clustering using k-means with k=3
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' multivariate=multivariate_charac(X,y)
#' summarize(multivariate)
#'
summary.multivariate <- function(multivariate){
  cat("Distance Matrix :",'\n')
  print(multivariate$distance_matrix)
  cat("\n")

  cat("Confusion Matrix :",'\n')
  print(multivariate$confusion_matrix)
  cat("\n")

  cat("Correlations :",'\n')
  print(multivariate$correlation)
  plot(multivariate$correlation)
  cat("\n")

  cat("Squared Correlations :",'\n')
  print(multivariate$squared_correlation)
}


