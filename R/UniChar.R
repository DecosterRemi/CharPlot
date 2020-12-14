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

#' v.test
#'
#' @param obj of UniChar class
#' @param y a data frame representing groups from a clustering process on obj$data
#' @param label a specific label variable
#'
#' @description calculate test values according to the labeled data and the group y
#' @import FactoMineR
#'
#' @return \item{result}{ a dataframe with test values}
#' @export
#'
#' @examples
#' autos = readxl::read_excel("Autos.xlsx")
#' tmp = sep_data(autos)
#' clus = kmeans(autos[tmp[[2]]], 4)
#' y <- clus$cluster
#' obj <- Perform_UniChar(autos,y)
#' vtest.UniChar(obj, y, 'make')
vtest.UniChar <- function(obj, y, label){
  v.test = c()
  df <- obj$categ[label]
  df['group'] <- y
  res = catdes(df, num.var=1, proba=1)
  for(elt in res$quanti){
    df <- as.data.frame(elt)
    if( colnames(df)[1]=="v.test" ){
      v.test <- c(v.test, df["v.test"][[1]])
    }
  }
  result <- as.data.frame(v.test)
  rownames(result) <- (unique(obj$categ[label]))[[1]]
  return(result)
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
#' df <- read_excel('Autos.xlsx')
#' sep_data(df)

sep_data <- function(df){
  colonne = colnames(df)
  categ=c()
  num = c()
  for(i in 1:length(df)){
    class = class(df[[i]])
    if (class=="numeric" ){
      num = c( num, colonne[i])
    } else if (class=="integer"){
      num = c( num, colonne[i])
    } else {
      categ=c(categ,colonne[i])
    }
  }
  list_categ_num = list(categ, num)
  return(list_categ_num)
}

####Numerical

#' Correlation
#'
#' @param obj of UniChar class
#' @param y a vector of groups issued from a clustering process
#'
#' @description To calculate correlation between numerical features of obj.
#'
#' @return correlation
#'
#' @export
#' @examples
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#' cor <- correlation(obj,y)
#' print(cor)

correlation <- function(obj,y)
  UseMethod(generic = "correlation")


#' correlation.default
#'
#' @param obj of UniChar class
#' @param y a vector of groups issued from a clustering process
#'
#' @description To calculate correlation between numerical features of obj.
#'
#' @return correlation
#'
#' @export
#' @examples
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#' cor <- correlation(obj,y)
#' print(cor)

correlation.default <- function(obj,y){
  stop("Object is not defined in class")
}


#' correlation.UniChar
#'
#' @param obj of UniChar class
#' @param y a vector of groups issued from a clustering process
#'
#' @description To calculate correlation between numerical features of obj.
#'
#' @return correlation
#'
#' @export
#' @examples
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#' cor <- correlation(obj,y)
#' print(cor)

correlation.UniChar <- function(obj,y){
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
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Create object UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Function value test
#' vt <- valuetest(obj, y)
#' print(vt)

valuetest <- function(obj,y)
  UseMethod(generic = "valuetest")

#' valuetest.default
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
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Create object UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Function value test
#' vt <- valuetest(obj, y)
#' print(vt)

valuetest.default <- function(obj,y){
  stop("Object is not defined in class")
}

#' Test value
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
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Create object UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Function value test
#' vt <- valuetest(obj, y)
#' print(vt)

valuetest.UniChar <- function(obj,y){
  x = obj$num
  #test value
  s <- sweep(obj$mk, 2, obj$m)
  vt <- (s)/sqrt(((obj$n-obj$nk)/(obj$n-1))*(obj$v/obj$nk))
  rownames(vt) <- c(paste("Test value G",1:obj$K))
  #Return the test value
  return(vt)
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
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Effect size
#' es <- effectsize(obj, y)
#' print(es)

effectsize <- function(obj,y)
  UseMethod(generic = "effectsize")

#' effectsize.default
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
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Effect size
#' es <- effectsize(obj, y)
#' print(es)

effectsize.default <- function(obj,y){
  stop("Object is not defined in class")
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
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Effect size
#' es <- effectsize(obj, y)
#' print(es)

effectsize.UniChar <- function(obj,y){
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
    pooled <- sqrt(((ng-1)*vg+(na-1)*va)/(ng+na-2))
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

#' Radar plot
#'
#' @param ind an indicator to represent graphically (ex: effect size, Cramer's V, etc.)
#' @description Radar plot the indicator
#'
#' @import ggradar dplyr scales tibble
#'
#' @return graph
#'
#' @export
#' @examples
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Effect size
#' vt <- valuetest(obj, y)
#'
#' #Plot radar
#' radar(vt)

radar <- function(ind)
  UseMethod(generic = "radar")

#' Radar plot
#'
#' @param ind an indicator to represent graphically (ex: effect size, Cramer's V, etc.)
#' @description Radar plot the indicator
#'
#' @import ggradar dplyr scales tibble
#'
#' @return graph
#'
#' @export
#' @examples
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Effect size
#' vt <- valuetest(obj, y)
#'
#' #Plot radar
#' radar(vt)

radar.default <- function(ind){
  vtradar <- ind %>%
    as_tibble() %>%
    mutate_each(rescale)
  radar <- cbind(row.names(ind), vtradar)
  ggradar(radar, legend.position="right", legend.text.size=9, group.point.size=3, group.line.width=0.5)
}

#' Radar plot
#'
#' @param ind an indicator to represent graphically (ex: effect size, Cramer's V, etc.)
#' @description Radar plot the indicator
#'
#' @import ggradar dplyr scales tibble
#'
#' @return graph
#'
#' @export
#' @examples
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Effect size
#' vt <- valuetest(obj, y)
#'
#' #Plot radar
#' radar(vt)

radar.UniChar <- function(ind){
  vtradar <- ind %>%
    as_tibble() %>%
    mutate_each(rescale)
  radar <- cbind(row.names(ind), vtradar)
  r <- ggradar(radar, legend.position="right", legend.text.size=9, group.point.size=3, group.line.width=0.5)
  return(r)
}

#Data Representation with the two most correlated features
#' Bidimensional plot of numerical features of obj
#'
#' @param obj of UniChar class
#'
#' @description Bidimensional plot aplied on numerical features of obj.
#'              Selecteting the two most correlated features.
#'
#' @import ggplot2
#'
#' @return none
#' @export
#' @examples
#' #Import data
#' df<- read_excel('Autos.xlsx')
#' X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]
#'
#' #Clustering
#' clus<- kmeans(X,3)
#' y <-clus$cluster
#'
#' #Attribut de la classe UniChar
#' obj <- Perform_UniChar(df,y)
#'
#' #Plot clustering
#' plot(obj)

plot.UniChar <- function(obj){
  group <- as.factor(obj$y.values)
  co <- correlation(obj,obj$y.values)
  axemax <- names(tail(sort(co[obj$K+1,]), 2))
  df <- obj$num[,axemax]
  p <- ggplot(obj$num, aes(x = df[[1]], y = df[[2]], colour=group))+geom_point()+labs(x=axemax[[1]], y = axemax[[2]])
  return(p)
}


#' barplt.UniChar
#'
#' @param obj of Unichar class
#' @param ind an indicator to represent graphically. Must be a single column Data frame
#' @param categ default FALSE. Set true if your indicator was calculated on categorical data
#'
#' @import ggplot2
#'
#' @return \item{bargraph}{ bar plot to show}
#' @export
#' @examples

barplt.UniChar <- function(obj, ind, categ = FALSE){
  if(categ){
    label = obj$catnames
  } else{
    label = obj$colnames
  }
  df <- as.data.frame(ind)
  df['label'] <- label
  print(df)
  bargraph <-ggplot(data=df, aes(x=df[[2]], y=df[[1]])) + geom_bar(stat="identity", fill="steelblue") + theme_minimal()  +labs(y= colnames(df[1]), x = 'labels')
  return(bargraph)
}
