library(ClusterCharPlot)
library(readxl)

df<- read_excel('Autos.xlsx')
####Pre-processing####
#transform data
######################
group = kmeans(df[sep_data(df)[[2]]],4) #clustering example
y <- group$cluster
obj <- Perform_UniChar(df,y)

vCramer <- vcramer.UniChar(obj,y)
radar.UniChar(vCramer)
