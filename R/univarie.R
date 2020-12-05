#' Class Univarié
#'
#' @param x DataFrame of features numeric
#' @param y Vecteur of classes predict
#'
#' @description Create class for caracteritics univarie of features
#'
#' @return \item{instance}{object of Univarie class}
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe univarié
#' univarie(X, y)
#'
univarie <- function(x,y){
  #create instance
  instance <- list()
  instance$x.values <- x
  instance$y.values <- y
  #names of features
  instance$x.names <- names(x)
  #Number of features
  instance$feature <- ncol(x)
  #number of observations
  instance$n <- apply(x,2,length)
  #number of groups
  instance$K <- length(unique(y))
  #effectifs conditionnels
  instance$nk <- apply(x,2,tapply,y,length)
  #means globale
  instance$m <- apply(x,2,mean)
  #means conditionnelles
  instance$mk <- apply(x,2,tapply,y,mean)
  #Variance
  instance$v <- matrix(apply(x,2,var), nrow=instance$K, ncol = instance$feature, byrow = TRUE)
  class(instance) <- "univarie"
  #return the instance
  return(instance)
}

ggplot.univarie <- function(obj){
  y <- as.factor(test$y.values)
  ggplot(test$x.values, aes(test$x.values[,1], test$x.values[,2], colour=y))+geom_point()
}
#' Function calcul correlation
#'
#' @param x DataFrame of features numeric
#' @param y Vecteur of classes predict
#' @param obj attribut of function univarie
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
correlation <- function(x,y, obj)
  UseMethod(generic = "correlation")

correlation.default <- function(x,y,obj){
  #variabilité totale
  cr <- scale(x, center = TRUE, scale = FALSE)
  SCT <- apply(cr^2,2,sum)
  #variabilité expliquée
  s <- sweep(obj$mk, 2, obj$m)
  SCE <- apply(obj$nk * (s)^2,2,sum)
  #Variance expliquée
  epl <- 100.0*(SCE/SCT)
  result <- rbind(obj$mk,epl)
  rownames(result) <- c(paste("mk G", 1:obj$K), "epl%")
  #return the instance
  return(result)
}


correlation.univarie <- function(x,y,obj){
  #variabilité totale
  cr <- scale(x, center = TRUE, scale = FALSE)
  SCT <- apply(cr^2,2,sum)
  #variabilité expliquée
  s <- sweep(obj$mk, 2, obj$m)
  SCE <- apply(obj$nk * (s)^2,2,sum)
  #Variance expliquée
  epl <- 100.0*(SCE/SCT)
  result <- rbind(obj$mk,epl)
  rownames(result) <- c(paste("mk G", 1:obj$K), "epl%")
  #return the instance
  return(result)
}

#' Function test value
#'
#' @param x DataFrame of features numeric
#' @param y Vecteur of classes predict
#' @param obj attribut of function univarie
#'
#' @description create a function of caracterisation of group with indicator test value
#'
#' @return \item{instance}{object of univarie class}
#' @export
#'
#' @examples
#' library(caret)
#' sample <- createDataPartition(iris$Species, p=0.80, list=FALSE)
#' iris_train <- iris[sample,]
#' x <- iris[,1:4]
#' y<- iris[,5]
#' control <- trainControl(method="cv", number=10)
#' metric <- "Accuracy"
#' fit.knn <- train(Species~., data=iris_train, method='knn', trControl=control, metric=metric)
#' group <- data.frame(predict(fit.knn,x))
#' vt(x, group)
#'
valuetest <- function(x,y, obj)
  UseMethod(generic = "valuetest")

valuetest.default <- function(x,y,obj){
  #Valeur test
  s <- sweep(obj$mk, 2, obj$m)
  vt <- (s)/sqrt(((obj$n-obj$nk)/(obj$n-1))*(obj$v/obj$nk))
  rownames(vt) <- c(paste("Test value G",1:obj$K))
  #Return the value test
  return(vt)
}

valuetest.univarie <- function(x,y,obj){
  #Valeur test
  s <- sweep(obj$mk, 2, obj$m)
  vt <- (s)/sqrt(((obj$n-obj$nk)/(obj$n-1))*(obj$v/obj$nk))
  vt <- t(vt)
  colnames(vt) <- c(paste("Test value G",1:obj$K))
  #Return the value test
  return(vt)
}


#' Function effect size
#'
#' @param x DataFrame of features numeric
#' @param y Vecteur of classes predict
#' @param obj attribut of function univarie
#'
#' @description create a function of caracterisation of group with indicator effect size
#'
#' @return\item{instance}{object of univarie class}
#' @export
#'
#' @examples
#' library(caret)
#' sample <- createDataPartition(iris$Species, p=0.80, list=FALSE)
#' iris_train <- iris[sample,]
#' x <- iris[,1:4]
#' y<- iris[,5]
#' control <- trainControl(method="cv", number=10)
#' metric <- "Accuracy"
#' fit.knn <- train(Species~., data=iris_train, method='knn', trControl=control, metric=metric)
#' group <- data.frame(predict(fit.knn,x))
#' vt(x, group)

effectsize <- function(x,y, obj)
  UseMethod(generic = "effectsize")

effectsize.default <- function(x,y,obj){
  result <- matrix(nrow = obj$K, ncol =  obj$feature)
  for (i in 1:obj$K){
    #Index of data in group i
    indexNames <- which(y==i)
    #data of other groups and groups
    autre <- x[-indexNames,]
    group <- x[indexNames,]
    #means
    mk <- apply(group,2,mean)
    ma <- apply(autre,2,mean)
    #Number of observations
    ng <- nrow(group)
    na <- nrow(autre)
    #Variance
    vg <- apply(group,2,var)
    va <- apply(autre,2,var)
    #Calcul de l'écart-type pooled
    pooled <- sqrt(((ng-1)*vg+(na-1)*va)/(ng+na))
    #Calcul de l'effect size
    es <- (mk-ma)/pooled
    result[i,] <- es
  }
  rownames(result) <- c(paste("Effect size G",1:obj$K, "vs other"))
  colnames(result) <- c(obj$x.names)
  #return result
  return(result)
}


effectsize.univarie <- function(x,y,obj){
  result <- matrix(nrow = obj$K, ncol =  obj$feature)
  for (i in 1:obj$K){
    #Index of data in group i
    indexNames <- which(y==i)
    #data of other groups and groups
    autre <- x[-indexNames,]
    group <- x[indexNames,]
    #means
    mk <- apply(group,2,mean)
    ma <- apply(autre,2,mean)
    #Number of observations
    ng <- nrow(group)
    na <- nrow(autre)
    #Variance
    vg <- apply(group,2,var)
    va <- apply(autre,2,var)
    #Calcul de l'écart-type pooled
    pooled <- sqrt(((ng-1)*vg+(na-1)*va)/(ng+na))
    #Calcul de l'effect size
    es <- (mk-ma)/pooled
    result[i,] <- es
  }
  rownames(result) <- c(paste("Effect size G",1:obj$K, "vs other"))
  colnames(result) <- c(obj$x.names)
  #return result
  return(result)
}

##Graphique radar
#graphique radar
radar <- function(vt){
  vtradar <- vt %>%
    as_tibble() %>%
    mutate_each(rescale)

  radar <- cbind(row.names(vt), vtradar)

  graph <- ggradar(radar, legend.position="right", legend.text.size=9, group.point.size=3, group.line.width=0.5)
  return(graph)
}

#Representation of data in the two axes there more contributes
ggplot.univarie <- function(obj){
  y <- as.factor(obj$y.values)
  co <- correlation(obj$x.values, obj$y.values, obj)
  max <- tail(sort(co[5,]), 2)
  ggplot(test$x.values, aes(obj$x.values[,names(max[1])], obj$x.values[,names(max[2])], colour=y))+geom_point()
}
