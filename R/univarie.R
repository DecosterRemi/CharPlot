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
#' Function correlation
#'
#' @param x
#' @param y
#'
#' @description Create function for caracterist partition with correlation
#'
#' @return \item{instance}{object of Univarie class}
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
#' correlation(x, group)
#'
#'
correlation <- function(x,y){
  #create instance
  instance <- list()
  instance$x.values <- x
  instance$y.values <- y
  #number of observations
  instance$size <- length(x)
  #number of groups
  instance$K <- length(unique(y))
  #effectifs conditionnels
  instance$nk <- table(y)
  #means globale
  instance$m <- mean(x)
  #means conditionnelles
  instance$mk <- tapply(x,y,mean)
  #variabilité totale
  instance$SCT <- sum((x-instance$m)^2)
  #variabilité expliquée
  instance$SCE <- sum(instance$nk * (instance$mk - instance$m)^2)
  #Variance expliquée
  instance$corr <- 100.0*(instance$SCE/instance$SCT)
  class(instance) <- "univarié"
  #return the instance
  return(instance)
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

vt<- function(x,y){
  instance <- list()
  #number of groups
  K <- length(unique(y))
  #number of observations
  n <- length(x)
  nk <- table(y)
  #means globale
  m <- mean(x)
  mk <- tapply(x,y,mean)
  #Variance
  v <- var(x)
  #Valeur test
  instance$vt <- (mk-m)/sqrt(((n-nk)/(n-1))*(v/nk))
  names(instance$vt) <- c(paste("Test value G",1:K))
  class(instance) <- "univarié"
  #Return the value test
  return(instance$vt)
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

es<- function(x,y){
  instance <- list()
  #number of groups
  K <- length(unique(y))
  #Transformation in matrix
  x <- matrix(x)
  for (i in 1:K){
    #Index of data in group i
    indexNames <- which(y==i)
    #data of other groups and groups
    autre <- x[-indexNames,]
    group <- x[indexNames,]
    #means
    mk <- mean(group)
    ma <- mean(autre)
    #Number of observations
    ng <- length(group)
    na <- length(autre)
    #Calcul de l'écart-type pooled
    pooled <- sqrt(((ng-1)*var(group)+(na-1)*var(autre))/(ng+na))
    #Calcul de l'effect size
    es <- (mk-ma)/pooled
    instance$result = c(instance$result, es)
  }
  names(instance$result) <- c(paste("Effect size G",1:K))
  class(instance) <- "univarié"
  #return result
  return(instance$result)
}
