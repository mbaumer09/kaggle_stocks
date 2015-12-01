library(h2o)
library(dplyr)
setwd("C:/Users/USER/Dropbox/Side Project/Machine Learning/Kaggle/Winton Stock Market Challenge")

if(!exists("current.train")){
      current.train <- "stocks"
      train <- read.csv("D:/R Data/Kaggle/Winton Stock Market Challenge/train.csv")
      test <- read.csv("D:/R Data/Kaggle/Winton Stock Market Challenge/test.csv")
}

if(current.train != "stocks"){
      train <- read.csv("D:/R Data/Kaggle/Winton Stock Market Challenge/train.csv")
      test <- read.csv("D:/R Data/Kaggle/Winton Stock Market Challenge/test.csv")
      current.train <- "stocks"
}

preproc.train <- train[,c(2:26,208)]
na.index <- which(is.na(preproc.train[,1]))
preproc.train[na.index,1] <- 0
preproc.train[,1] <- factor(preproc.train[,1])

preproc.test <- test[,c(2:26)]
na.index <- which(is.na(preproc.test[,1]))
preproc.test[na.index,1] <- 0
preproc.test[,1] <- factor(preproc.test[,1])

## start a local cluster with 1GB RAM
localH2O = h2o.init(max_mem_size = '6g', # use 6GB of RAM of *GB availabke on Kaggle
                    nthreads = -1) 
## import MNIST data as H2O
train_h2o = as.h2o(localH2O, preproc.train)
test_h2o = as.h2o(localH2O, preproc.test)
## set timer
s <- proc.time()

model =
      h2o.deeplearning(x = 1:25,  # column numbers for predictors
                       y = 26,   # column number for label
                       training_frame = train_h2o, # data in H2O format
                       activation = "RectifierWithDropout", # algorithm
                       input_dropout_ratio = 0.2, # % of inputs dropout
                       hidden_dropout_ratios = c(0.5), # % for nodes dropout, length must equal number of hidden layers
                       balance_classes = FALSE, 
                       hidden = c(200), # one layer of 200 nodes
                       momentum_stable = 0.99,
                       nesterov_accelerated_gradient = T, # use it for speed
                       epochs = 25, # max. no. of epochs
                       ignore_const_cols = TRUE,
                       rate = .005) 

## print model info and time elapsed
print(model)
print(proc.time() - s)

## classify test set
h2o_y_test <- h2o.predict(model, test_h2o)

## convert H2O format into data frame and  save as csv
df_y_test = as.data.frame(h2o_y_test)
df_y_test = data.frame(ImageId = seq(1,length(df_y_test$predict)), Label = df_y_test$predict)
#write.csv(df_y_test, file = "submission-r-h2o.csv", row.names=F)

## shut down virutal H2O cluster
h2o.shutdown(localH2O)