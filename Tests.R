library(ClusterCharPlot)

obj <- Perform_ACP(as.data.frame(matrix(sample(c(1, 1:5), 25, replace= TRUE),5)))
correlationplot.ACP(obj, 0.5)
print(obj)



