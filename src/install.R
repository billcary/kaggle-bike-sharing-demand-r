

install.packages('lubridate', dependencies=TRUE, quiet = TRUE) 
install.packages('randomForest', dependencies=TRUE, quiet = TRUE)
install.packages('caret', dependencies=TRUE, quiet = TRUE)
install.packages('Metrics', dependencies=TRUE, quiet = TRUE)
install.packages('doParallel', dependencies=TRUE, quiet = TRUE)
install.packages('devtools', dependencies = TRUE, quiet = TRUE)
install.packages('gbm', dependencies = TRUE, quiet = TRUE)
install.packages('cubist', dependencies = TRUE, quiet = TRUE)

library(devtools)
install_url(url = 'http://cran.r-project.org/src/contrib/Archive/weathermetrics/weathermetrics_1.0.tar.gz')

install.packages("h2o", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-lambert/5/R", getOption("repos"))), quiet = TRUE)