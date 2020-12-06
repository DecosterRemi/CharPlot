#' Class UniChar
#'
#' @param df a data frame representing all your data
#' @param y a data frame representing groups from a clustering process on numerical variables of df
#'
#' @description UniChar class creation.
#' To perform characterisation on a single variable after a clustering process.
#'
#' @import stats
#'
#' @return \item{instance}{object of UniChar class}
#'
#' @export
#' @examples
#' df<- read_excel('Autos.xlsx')
#' ####Pre-processing####
#' #transform data
#' ######################
#' y = kmeans(df[sep_data(df)[[2]]],4) #clustering example
#' Perform_UniChar(df,y)
#'
#'
Perform_UniChar <- function(df, y){
  #create instance
  instance <- list()
  #data
  instance$data <- df
  instance$y.values <- y
  tmp = sep_data(df) #splitting data on their type
  #qualitative data
  instance$categ=df[tmp[[1]]]
  #quantitative data
  x=df[tmp[[2]]]
  instance$num=x

  #names of numerical and categorical features
  instance$colnames <- colnames(instance$num)  #old instance$x.names
  instance$catnames <- colnames(instance$categ)
  #Number of numerical features
  instance$ncol <- ncol(x) #old instance$feature
  #number of groups
  instance$K <- length(unique(y)) #for example, K from a kmeans() process
  #number of observations
  instance$n <- apply(df, 2, length)

  #conditional effectives
  instance$nk <- apply(x,2,tapply,y,length)
  #global mean
  instance$m <- apply(x,2,mean)
  #conditional means
  instance$mk <- apply(x,2,tapply,y,mean)
  #Variance
  instance$v <- matrix(apply(x,2,var), nrow=instance$K, ncol = instance$ncol, byrow = TRUE)

  class(instance) <- "UniChar"
  #return the instance
  return(instance)
}



#' Cramer's v
#'
#' @param obj of UniChar class
#' @param y a data frame representing groups from a clustering process on obj$data
#' @param label=NULL a specific label variable if you don't want all Cramer's v (can be several)
#'
#' @description Calculate Cramer's v values on all yout categorical data or just on labeled ones if provided
#' @import creditmodel
#' @return Cramer's v value(s) according to your groups y on your categorical data
#'
#' @export
#' @examples
#' data <- read_excel('Autos.xlsx')
#' obj <- Perform_UniChar(data)
#' clus<- kmeans(obj$num,4)
#' group <-clus$cluster
#' y <- as.data.frame(group)
#' vcramer.UniChar(obj,y)
#'

vcramer.UniChar <- function(obj, y, label = NULL){
  df = obj$categ
  #return all Cramer's v
  if(is.null(label)){
    vCramer <- char_cor_vars(df,y)
    result <- as.data.frame(vCramer)
    rownames(result) <- c(obj$catnames) #old x.names
    return(result)
  #only return Cramer's v with labeled variables
  } else{
    vCramer <-char_cor_vars(df[label],y)
    result <- as.data.frame(vCramer)
    rownames(result) <- label
    return(result)
  }
}


#' sep_data
#'
#' @param df a data frame
#'
#' @description Function to split categorical and numerical variable labels.
#' Used to instance categorical and numerical data in the UniChar class.
#'
#' @return \item{list_categ_num}{  a list of 2 vectors. Labels of categorical and numerical variables}

#' @export
#' @examples
#' data <- read_excel('Autos.xlsx')
#' df <- as.data.frame(data)
#' sep_data(df)

sep_data <- function(df){
  colonne = colnames(df)
  categ=c()
  num = c()
  for(i in 1:length(df)){
    if(class(df[[i]])!="numeric"){
      categ=c(categ,colonne[i])
    } else {
      num = c( num, colonne[i])
    }
  }
  list_categ_num = list(categ, num)
  return(list_categ_num)
}




####Numerical




correlation <- function(y, obj)
  UseMethod(generic = "correlation")


# correlation.default <- function(y,obj){
#   x = obj$num
#   #Total variability
#   cr <- scale(x, center = TRUE, scale = FALSE)
#   SCT <- apply(cr^2,2,sum)
#   #Explained variability
#   s <- sweep(obj$mk, 2, obj$m)
#   SCE <- apply(obj$nk * (s)^2,2,sum)
#   #Explained variance
#   epl <- 100.0*(SCE/SCT)
#   result <- rbind(obj$mk,epl)
#   rownames(result) <- c(paste("mk G", 1:obj$K), "epl%")
#   #return the correlation
#   return(result)
# }


#' correlation.UniChar
#'
#' @param y a vector of groups issued from a clustering process
#' @param obj of UniChar class
#'
#' @description To calculate correlation between numerical features of obj.
#'
#' @return correlation
#'
#' @export
#' @examples
#' correlation.UniChar(y,obj)

correlation.UniChar <- function(y,obj){
  x = obj$num
  #Total variability
  cr <- scale(x, center = TRUE, scale = FALSE)
  SCT <- apply(cr^2,2,sum)
  #Explained variability
  s <- sweep(obj$mk, 2, obj$m)
  SCE <- apply(obj$nk * (s)^2,2,sum)
  #Explained variance
  epl <- 100.0*(SCE/SCT)
  result <- rbind(obj$mk,epl)
  rownames(result) <- c(paste("mk G", 1:obj$K), "epl%")
  #return the correlation
  return(result)
}


valuetest <- function(y, obj)
  UseMethod(generic = "valuetest")

valuetest.default <- function(y,obj){
  x = obj$num
  #test value
  s <- sweep(obj$mk, 2, obj$m)
  vt <- (s)/sqrt(((obj$n-obj$nk)/(obj$n-1))*(obj$v/obj$nk))
  rownames(vt) <- c(paste("Test value G",1:obj$K))
  #Return the test value
  return(vt)
}


#' Test value
#'
#'
#' @param y a vector of groups issued from a clustering process
#' @param obj of UniChar class
#'
#' @description Caracterisation function of groups with test value indicator.
#'  Applied on numerical features of obj.
#'
#' @return test_values
#'
#' @export
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

valuetest.UniChar <- function(y,obj){
  x = obj$num
  #test value
  s <- sweep(obj$mk, 2, obj$m)
  vt <- (s)/sqrt(((obj$n-obj$nk)/(obj$n-1))*(obj$v/obj$nk))
  vt <- t(vt)
  rownames(vt) <- c(paste("Test value G",1:obj$K))
  #Return the test value
  return(vt)
}




effectsize <- function(y, obj)
  UseMethod(generic = "effectsize")

effectsize.default <- function(y,obj){
  x = obj$num
  result <- matrix(nrow = obj$K, ncol =  obj$ncol) #old obj$feature changé par ncol
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
    #pooled std deviation
    pooled <- sqrt(((ng-1)*vg+(na-1)*va)/(ng+na))
    #Effect size
    es <- (mk-ma)/pooled
    result[i,] <- es
  }
  rownames(result) <- c(paste("Effect size G",1:obj$K, "vs other"))
  colnames(result) <- c(obj$colnames) #old x.names
  #return result
  return(result)
}

#' Effect size
#'
#' @param y a vector of groups issued from a clustering process
#' @param obj of UniChar class
#'
#' @description Caracterisation function of groups with effect size indicator.
#'  Applied on numerical features of obj.
#'
#' @return effect_sizes
#'
#' @export
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

effectsize.UniChar <- function(y,obj){
  x = obj$num
  result <- matrix(nrow = obj$K, ncol =  obj$ncol) #old obj$feature changé par ncol
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
    #pooled std deviation
    pooled <- sqrt(((ng-1)*vg+(na-1)*va)/(ng+na))
    #Effect size
    es <- (mk-ma)/pooled
    result[i,] <- es
  }
  rownames(result) <- c(paste("Effect size G",1:obj$K, "vs other"))
  colnames(result) <- c(obj$colnames) #old x.names
  #return result
  return(result)
}



#Radar graph

radar <- function(ind)
  UseMethod(generic = "radar")

# radar.default <- function(fct){
#   vtradar <- fct %>%
#     as_tibble() %>%
#     mutate_each(rescale)
#
#   radar <- cbind(row.names(fct), vtradar)
#
#   graph <- ggradar(radar, legend.position="right", legend.text.size=9, group.point.size=3, group.line.width=0.5)
#   return(graph)
# }

#' Radar plot
#'
#' @param ind an indicator to represent graphically (ex: effect size, Cramer's V, etc.)
#' @description Radar plot the indicator
#'
#' @import ggplot2
#'
#' @return graph
#'
#' @export
#' @examples
#' #TODO
radar.UniChar <- function(ind){
  vtradar <- ind %>%
    as_tibble() %>%
    mutate_each(rescale)

  radar <- cbind(row.names(ind), vtradar)

  graph <- ggradar(radar, legend.position="right", legend.text.size=9, group.point.size=3, group.line.width=0.5)
  return(graph)
}

#Data Representation with the two most correlated features
#' Bidimensional plot of numerical features of obj
#'
#' @param obj of UniChar class
#' @description Bidimensional plot aplied on numerical features of obj.
#'              Selecteting the two most correlated features.
#'
#' @import ggplot2
#'
#' @return none

#' @export
#' @examples
#' #TODO
ggplot.UniChar <- function(obj){
  y <- as.factor(obj$y.values)
  co <- correlation.UniChar(obj$y.values, obj) #old obj$x.values with 3 parameters (now 2)
  max <- tail(sort(co[5,]), 2)
  ggplot(test$x.values, aes(obj$x.values[,names(max[1])], obj$x.values[,names(max[2])], colour=y))+geom_point()
}



