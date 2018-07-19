require(readr)
require(dplyr)
require(ggplot2)
require(GGally)
require(tidyquant)
require(RcppRoll)
require(TTR)
require(RCurl)
require(h2o)
require(kableExtra)
require(prettydoc)

ResponseAndData_All <- function(resp, aTrain, aTest){
  
  resp <- resp
  pred <- setdiff(names(aTrain), resp)
  
  train <- aTrain
  test <- aTest
  train.hex <- as.h2o(train)
  test.hex <- as.h2o(test)
  
  return(list(resp=resp, pred=pred, train=train, test=test, train.hex=train.hex, test.hex=test.hex))
  
}

TrainAndPlot_3sd <- function(pred, resp, train.hex, test.hex, test){
  
  con.glm = h2o.glm(x = pred, y = resp, training_frame = train.hex, family = "gaussian", alpha = 0.5)
  con.fit = h2o.predict(object = con.glm, newdata = test.hex)
  targets <- as.vector(test[resp])
  predictions <- as.vector(con.fit)
  sqrt_result <- sqrt(mean((predictions - targets) ** 2))
  
  
  
  diff_df <- data.frame(date=test$Date,
                        sector=test$Sector,
                        symbol=test$Symbol,
                        qrt_year=test$QtrYear,
                        predictions=predictions, targets=targets, 
                        diff=round((predictions-targets),5))
  colnames(diff_df)[7] = "diff"
  
  std <- sd(diff_df$diff)
  
  gplt<- diff_df %>%
    ggplot(aes(x=diff))+
    geom_histogram(fill="skyblue")+
    geom_vline(xintercept = c(3*std, -3*std), linetype="dashed",color="red")+
    annotate(geom="text", x=3*std, y=100, label="3 SDV", color="black")+
    ggtitle("Prediction & Tragets Difference Histogram")
  
  return(list(sqrt_result=sqrt_result, diff_df=diff_df, gplt=gplt))
  
}

normalize_Func <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}



# Functions for Random Forest
Data_RF <- function(aTrain, aTest){
  
  train <- aTrain
  test <- aTest
  train.hex <- as.h2o(train)
  test.hex <- as.h2o(test)
  train.hex <- h2o.assign(train.hex, "train.hex")
  test.hex <- h2o.assign(test.hex, "test.hex")
  
  return(list(train=train, test=test, train.hex=train.hex, test.hex=test.hex))
  
}

Train_Plot_RF <- function(pred, resp, train.hex, test.hex, test){
  
  rf_model <- h2o.randomForest(         
    training_frame = train.hex,
    x=pred,                        
    y=resp,                         
    ntrees = 100,   
    stopping_rounds = 1, score_each_iteration = T, seed = 101) 
  
  rf_summary <-summary(rf_model)
  
  rf.fit = h2o.predict(object=rf_model, newdata=test.hex)
  
  targets <- as.vector(test[resp])
  predictions <- as.vector(rf.fit)
  
  diff_df <- data.frame(date=test$Date,
                        sector=test$Sector,
                        symbol=test$Symbol,
                        qrt_year=test$QtrYear,
                        predictions_rf=predictions, 
                        targets_rf=targets,
                        diff = round((predictions-targets),5)
  )
  
  colnames(diff_df)[7] = "diff"
  
  std <- sd(diff_df$diff)
  
  return(list(summary = rf_summary, diff_df=diff_df, std=std))
  
}


