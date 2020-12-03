# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Class Univarié
#'
#' @param x
#' @param y
#'
#' @description Create class for caracteritics univarie of features
#'
#' @return \item{instance}{object of Univarie class}
#' @export
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
#'
univarie <- function(x,y){
  #create instance
  instance <- list()
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
  class(instance) <- "univarié"
  #return the instance
  return(instance)
}


#' Function calcul correlation
#'
#' @param x
#' @param y
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
  result <- cbind(t(obj$mk),epl)
  colnames(result) <- c(paste("mk G", 1:obj$K), "epl%")
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
  result <- cbind(t(obj$mk),epl)
  colnames(result) <- c(paste("mk G", 1:obj$K), "epl%")
  #return the instance
  return(result)
}

#' Function test value
#'
#'
#' @param x
#' @param y
#'
#'#' @description create a function of caracterisation of group with indicator test value
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
valuetest <- function(x,y, obj)
  UseMethod(generic = "valuetest")

valuetest.default <- function(x,y,obj){
  #Valeur test
  s <- sweep(obj$mk, 2, obj$m)
  vt <- (s)/sqrt(((obj$n-obj$nk)/(obj$n-1))*(obj$v/obj$nk))
  vt <- t(vt)
  colnames(vt) <- c(paste("Test value G",1:obj$K))
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
#' @param x
#' @param y
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
  result <- matrix(ncol = obj$K, nrow =  obj$feature)
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
    result[,i] <- es
  }
  colnames(result) <- c(paste("Effect size G",1:obj$K, "vs other"))
  rownames(result) <- c(obj$x.names)
  #return result
  return(result)
}


effectsize.univarie <- function(x,y,obj){
  result <- matrix(ncol = obj$K, nrow =  obj$feature)
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
    result[,i] <- es
  }
  colnames(result) <- c(paste("Effect size G",1:obj$K, "vs other"))
  rownames(result) <- c(obj$x.names)
  #return result
  return(result)
}
