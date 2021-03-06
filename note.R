# some quick commend to use 

library(blogdown)

list.of.packages <- c("ggplot2", "data.table","plyr","QuantPsyc",
                      "glmnet","leaps","randomForest","gbm","caret","xgboost","Ckmeans.1d.dp",
                      "DiagrammeR", "knitr", "here",
                      "curl", "gplots", "dendextend", "colorspace",
                      "raster", "rgdal", "Hmisc", "plyr", "RColorBrewer", "googledrive",
                      "SHAPforxgboost",
                      "leafletCN",
                      "devtools", "drake", "reshape2", "scatterplot3d", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



# here()
# blogdown::new_site(theme = 'alanorth/hugo-theme-bootstrap4-blog')
# setwd(here("hugo-xmag"))

blogdown::stop_server()
blogdown::serve_site()

blogdown::build_site()


