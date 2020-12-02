# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#creation de la classe UniNum
#constructeur -- donnees represente un data frame
#' Class Constructor
#' @param data data frame
#'
#' @description UniNum class creation
#'
#' @return \item{instance}{  object of UniNum class}
#' @export
#'
#' @examples
#' dataset <- matrix(sample(c(1, 1:5), 25, replace= TRUE),5)
#' df <- as.data.frame(dataset)
#' Perform_UniNum(df)


Perform_UniNum <- function(data){
  #controle - data.frame
  ok <- is.data.frame(data)
  if (!ok){
    stop("Ce n'est pas un data frame")
  }
  #controle du type de chaque variable
  nb_ok <- sum(sapply(data,function(x){is.numeric(x)}))
  if (nb_ok < ncol(data)){
    stop("Il y a des variables non numeriques dans le data frame")
  }
  #ok - on peut y aller
  instance <- list()
  instance$X <- data
  instance$calc <- princomp(data,cor=T,scores=T)
  instance$vp <- instance$calc$sdev^2
  group = sample(1,length(data[,1]), replace = TRUE)
  group = as.data.frame(group)
  #All samples initialize to group 1
  instance$group <- group
  class(instance) <- "UniNum"
  return(instance)
}


#surcharge de print
#' print override
#'
#' @param object UniNum instance
#' @description Prints eigen values from the data of an UniNum object
#' @return
#' @export
#'
#' @examples
#' object <- Perform_UniNum(data)
#' print(object)


#TODO print sth else
print.UniNum <- function(object){
  #affichage de la liste des variables
  cat("Variables : ", colnames(object$X),"\n")
  #affichage des valeurs propres
  cat("Valeurs propres : ",object$vp)
}

#' Correlation plot
#'
#' @param object of UniNum class
#' @param usedComp a float
#' @description Charts a correlation matrix
#' @import corrplot
#' @return
#' @export
#'
#' @examples
#' obj <- Perform_UniNum(data)
#' correlationplot.UniNum(obj,usedComp = 1)

correlationplot.UniNum <- function(object,usedComp=1){
  #Verifier la disponibilite du package
  # res <- require(corrplot)
  # if (res == FALSE){
  #   install.packages("corrplot")
  #   res <- require(corrplot)
  # }
  # #verification
  # if (res == FALSE){
  #   stop("Chargement du package impossible")
  # }
  #on continue sinon
  if (is.null(usedComp) || usedComp < 1 || usedComp > length(object$vp)){
    #correlation standard
    corrplot(cor(object$X))
  } else
  {
    #ordre des variables selon la correlation avec la composante
    ordre <- order(object$calc$loadings[,usedComp])
    #correlation ordonnee selon une composante
    corrplot(cor(object$X[ordre]))
  }
}
