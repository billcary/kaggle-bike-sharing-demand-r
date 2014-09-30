

install.packages('lubridate', dependencies=TRUE) 
install.packages('randomForest', dependencies=TRUE)
install.packages('caret', dependencies=TRUE)
install.packages('Metrics', dependencies=TRUE)
install.packages('doParallel', dependencies=TRUE)
install.packages('devtools', dependencies = TRUE)

library(devtools)
install_url(url = 'http://cran.r-project.org/src/contrib/Archive/weathermetrics/weathermetrics_1.0.tar.gz')