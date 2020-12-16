# ClusterCharPlot

## Aim 
Univariate and multivariate characterization of a clustering process with ggplot2. Calculate partition evaluation measures.

## Installation
After unzipping the project, open it with RStudio.
Please first install devtools and roxygen2 to build the package: 
```R
  install.packages("roxygen2");
  install.packages("devtools");
  install.packages("dplyr");
  install.packages("scales");
  install.packages("tibble"); 
  install.packages('stats');
  install.packages('caret');
  install.packages('e1071');
  install.packages('rlist');
  install.packages('ggplot2');
  install.packages('ggrepel');
  install.packages('ggforce');
  install.packages('gridExtra');
  install.packages('creditmodel');
  install.packages("FactoMineR");
  devtools::install_github("ricardo-bion/ggradar", 
                          dependencies = TRUE);
```
  
Then within RStudio, please go to Build --> Configure Build Tools..., then tick "Generate documentation with Roxygen". 
Roxygen Options pop up must appear (otherwise click on Configure...). Tick "Install and Restart" or "Build & Reload" according to your RStudio version.
Finally, build the project with "Clean and Rebuild" and you are all set up. If you encounter troubles during the installation, please try to run ```R roxygen2::roxygenise();```, it might solve some of them.

You can now use our functions anywhere by loading our package with:
```R
  library(ClusterCharPlot);
```
And test them with our file tutorial.R 
