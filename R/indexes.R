
#' Squared euclidean ditance
#'
#' @param x vector
#' @param y vector
#'
#' @return squared distance
#' @export
#'
#' @examples
#' x=c(0,1)
#' y=c(1,0)
#' dist=d(x,y)
d <- function(x,y){
  di=0
  for (i in 1:length(x)){
    di=di+(x[i]-y[i])**2
  }
  di=as.numeric(di)
  return(di)
}



#' `indexes()`
#' Computing internal (and external) measures of a clustering
#'
#' @param X data.frame with the predictive variable that were used for the clustering
#' @param y data.frame (or vector) with the same number of rows (or length) as `X`. This represents the predicted classes.
#' @param y_true data.frame (or vector) with the same number of rows (or length) as `X`. This represents the predicted classes.
#'
#' @return object of measures class - with internal and external clustering measures
#' 
#' @import stats caret
#' 
#' @export
#' 
#'
#' @details measures objects have the following attributes :
#' \itemize{
#'  \item{"internal"}{internal_ind object with the following measures as attributes:
#'  \itemize{
#'    \item{"Silhouette"}{Silhouette index.}
#'    \item{"Dunn"}{Dunn index.}
#'    \item{"eta_sq"}{Ratio of the intracorrelation on the total correlation.}
#'    }
#'  }
#'  
#'  \item{"external"}{external_ind object with the following measures as attributes:
#'  \itemize{rand_ind=rand_ind,accuracy=accuracy,kappa=kappa,F1=F1,recall=recall
#'    \item{"rand_ind"}{Rand Index.}
#'    \item{"Accuracy"}{Average accuracy of the clustering.}
#'    \item{"kappa"}{Averaged value of kappa.}
#'    \item{"F1"}{Averaged value of F1.}
#'    \item{"recall"}{Average recall of the clustering.}
#'    }
#'  }
#' }
#'
#' @examples
#' require(readxl)
#' require(stats)
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#' 
#' measures=indexes(X,y)
#' 
#' print(measures$internal)
#' print(measures$external)
indexes <- function(X,y,y_true=NULL){
   D=as.matrix(dist(X,diag=T,upper=T))
   n=nrow(X)
   
   # Silhouette
   S_i=0
   for (i in 1:n){
      a_i=sum(D[y!=y[i],i])/(sum(y==y[i])-1)
      
      other_clusters=unique(y[y!=y[i]])
      liste_b_i=vector(mode='numeric',length = length(other_clusters))
      for (j in 1:length(other_clusters)){
         liste_b_i[j]=mean(D[y==other_clusters[j]])
      }
      b_i=min(liste_b_i)
      b_i=min(liste_b_i)
      S_i=S_i+(b_i-a_i)/max(a_i,b_i)/(n*length(data))
   }
   
   #Dunn
   ind_diff=array(0L, dim(D))
   ind_same=array(0L, dim(D))
   for (i in 1:n){
      ind_diff[,i]=y!=y[i]
      ind_same[,i]=y==y[i]
   }
   min_sep=min(D[ind_diff==1])
   max_diam=max(D[ind_same==1])
   
   Dunn=min_sep/max_diam
   
   #Correlation ratio
   clusters_name<-sort(unique(y))
   SCE=0
   SCT=sum(apply(X,1,function(x) d(x,sapply(X,mean))))
   
   for (e in clusters_name){
      SCE=SCE+nrow(X[y==e,])*d(sapply(X[y==e,],mean),sapply(X,mean))
   }
   eta=SCE/SCT
   
   internal=list(Silhouette=S_i, Dunn=Dunn, eta_sq=eta)
   class(internal) <- "internal_ind"
   
   #External indexes
   external=c(0)
   if (!is.null(y_true)){
      y=factor(y)
      y_true=factor(y_true)
      cm=confusionMatrix(y,y_true)
   
      rand_ind=sum(diag(cm$table))/sum(cm$table)
      accuracy=cm$overall[1]
      kappa=cm$overall[2]
      F1=mean(cm$byClass[,"F1"])
      recall=mean(cm$byClass[,"Recall"])
   
      external=list(rand_ind=rand_ind,accuracy=accuracy,kappa=kappa,F1=F1,recall=recall)
      
   }
   class(external) <- "external_ind"
   
   measures=list(internal=internal,external=external)
   class(measures)<-"measures"
   return(measures)
}

#' Plotting measures of clustering
#' 
#' Plots a barplot with the internal indexes and, if y_true was given, a similar barplot for the external indexes
#'
#' @param measures - object of the class measures 
#'
#' @return None
#' 
#' @import ggplot2 gridExtra
#' @export
#'
#' @examples
#' require(readxl)
#' require(stats)
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#' 
#' measures=indexes(X,y)
#' 
#' plot(measures)
plot.measures <- function(measures){
   class(measures$internal)="list"

   nom=names(measures$internal)
   valeurs=as.vector(unlist(measures$internal))
   df <- data.frame(internal_measures=nom,score=valeurs)
   p1=ggplot(data=df, aes(x=internal_measures, y=score,fill=internal_measures))+geom_bar(stat="identity")+theme_minimal()
   
   class(measures$external)="list"
   if (length(measures$external)!=1){

      nom2=names(measures$external)
      valeurs2=as.vector(unlist(measures$external))
      df2 <- data.frame(external_measures=nom2,score=valeurs2)
      p2 <- ggplot(data=df2, aes(x=external_measures, y=score,fill=external_measures))+geom_bar(stat="identity")+theme_minimal()
      
      liste_graph=list(p1,p2)
      return(do.call("grid.arrange",c(liste_graph,ncol=2)))
      
   } else {
         return(p1)
      }
   
   }
