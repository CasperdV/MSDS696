#Load the required libraries
library(readr)                                                              # to read the data
library(dplyr)                                                              # to manipulate dataframes
library(BalancedSampling)                                                   # to create the training and validation split
library(keras)                                                              # the neural network
library(ggplot2)                                                            # for plotting
library(parallel)                                                           # for using parallelized version of lapply
library(reshape2)                                                           # for reshaping dataframes from wide to long (melt)

#Clear the environment and set the seed
rm(list=ls())                                                               # clear the environment
use_session_with_seed(42, quiet = TRUE)                                     # set a seed for reproducibility

#Create custom functions
get.data <- function(file,delimiter,time_variable,id_variable,periods) {
  #Load an ordered subset of raw data
  data <- data.frame(read_delim(file,delimiter,col_types = cols(),progress = FALSE))
  data <- data[data[,time_variable] %in% periods,]
  data <- data[order(data[,id_variable],data[,time_variable]),]
  rownames(data) <- NULL
  return(data)
}

state.map <- function(data,time_variable,id_variable,states) {
  #Create a mapping of state variable values to state numbers
  map.states <- unique(data[,states])
  for (var in states) {map.states <- map.states[order(map.states[,var]),]}
  map.states <- data.frame(State = row_number(map.states[,1]),map.states)
  rownames(map.states) <- NULL
  return(map.states)
}

training.set <- function(data,time_variable,id_variable,base_periods,prediction_periods) {
  #Create the training datasets for the specified period
  data.train <- expand.grid(unique(min(base_periods):(max(base_periods) + length(prediction_periods))),data[data[,time_variable] %in% base_periods,id_variable],KEEP.OUT.ATTRS = FALSE)
  names(data.train) <- c(time_variable,id_variable)
  data.train <- full_join(data.frame(data[data[,time_variable] %in% base_periods,which(!names(data) == time_variable)],Lag = data[data[,time_variable] %in% base_periods,time_variable]),data.train,by = 'Beneficiary')
  data.train <- data.train[data.train[,time_variable] > data.train[,'Lag'],]
  data.train <- data.train[data.train[,time_variable] <= (data.train[,'Lag'] + length(prediction_periods)),]
  data.train <- left_join(data.train,data.frame(data[,c(id_variable,time_variable)],Future.State = data[,'State']),by = c('Beneficiary', 'Period'))
  data.train[is.na(data.train$Future.State),'Future.State'] <- 0 
  data.train$Lag <- data.train[,time_variable] - data.train$Lag
  data.train <- data.train[which(!names(data.train) == time_variable)]
  return(data.train)
}

one.hot <- function(data,drop_last=TRUE,variable_name='') {
  #One-hot encode categorical variables
  categorical <- data.frame(mclapply(seq_along(data), function(x) {
    data[,x] <- factor(data[,x],levels = sort(unique(data[,x])))
    if (drop_last) {
      set <- data.frame(to_categorical(data[,x]))[,as.numeric(levels(data[,x])) + 1]
      set <- set[-length(set)]
      names(set) <- paste0(names(data[x]),'.',variable_name,levels(data[,x])[-length(levels(data[,x]))])
      set
    } else {
      set <- data.frame(to_categorical(data[,x]))[,as.numeric(levels(data[,x])) + 1]
      names(set) <- paste0(names(data[x]),'.',variable_name,levels(data[,x]))
      set
    }
  }))
  return(categorical)
}

training.split <- function(prob,strata,categorical,continuous,responses) {
  #Split the data between training and validation sets
  p = rep(prob,length(strata))
  i = cubestratified(p,cbind(p),strata)
  predictors.train <- as.matrix.data.frame(cbind(categorical[i == 0,],continuous[i == 0,]))
  predictors.validate <- as.matrix.data.frame(cbind(categorical[i == 1,],continuous[i == 1,]))
  response.train <- as.matrix.data.frame(responses[i == 0,])
  response.validate <- as.matrix.data.frame(responses[i == 1,])
  training.data <- list(predictors.train = predictors.train,
                        predictors.validate = predictors.validate,
                        response.train = response.train,
                        response.validate = response.validate)
  return(training.data)
}

build.model <- function(predictors.train,response.train) {
  #Build and compile the neural network
  model <- keras_model_sequential() %>%
    layer_dense(units = dim(predictors.train)[2] * 2, activation = 'relu',kernel_initializer = 'random_normal',input_shape = dim(predictors.train)[2]) %>%
    layer_dense(units = dim(predictors.train)[2] * 2, activation = 'relu',kernel_initializer = 'random_normal') %>%
    layer_dense(units = dim(response.train)[2], activation='softmax',kernel_initializer = 'random_normal')
  compile(model, loss = 'categorical_crossentropy',optimizer = 'adam',metrics = 'accuracy')
  return(model)
}

#Set the model parameters
data.file <- 'data/beneficiaries.txt'                                       # path to data file
data.delimiter <- '|'                                                       # column delimiter used in data file
beneficiary.id <- 'Beneficiary'                                             # column name of the column containing the beneficiary id
time <- 'Period'                                                            # column name of the column containing the time variable
states <- c('Plan','Income','Type','Chronic')                               # column name of the column containing the current membership state
categorical.predictors <- c('Gender')                                       # all variables that should be one-hot encoded (excluding state variables - these are added automatically)
continuous.predictors <- c('Age','Duration','Adults','Children')            # all variables that should be scaled and centered
ageing.variables <- c('Age','Duration')                                     # variables that increase with time (deterministic)
training.periods <- 1:36                                                    # periods for which data is "known" and used for training
prediction.periods <- 37:48                                                 # months for which data is "unknown" and not used for training
base.period <- 36                                                           # last "known" period from which predictions will be made

#Set the internal parameters
T <- seq((max(training.periods)-max(prediction.periods)+base.period) %% 12,(max(training.periods)-max(prediction.periods)+base.period),12)
T <- T[T > 0]

#Load the raw data
data <- get.data(data.file,data.delimiter,time,beneficiary.id,min(T):(max(T) + length(prediction.periods)))
data <- data[c(time,beneficiary.id,categorical.predictors,continuous.predictors,states)]

#Create the mapping from state variables to state numbers
data <- left_join(data,rbind(0,state.map(data,time,beneficiary.id,states)),by=states)
data <- data[which(!names(data) %in% states)]

#Create the future state variable for the base data
data.train <- training.set(data,time,beneficiary.id,T,prediction.periods)
rm(data)

#One-hot encode the response variables
responses <- one.hot(data.train['Future.State'],drop_last=FALSE)
  
#One-hot encode the categorical predictor variables
categorical <- one.hot(data.train[,c(categorical.predictors,'Lag','State')],drop_last=TRUE)
  
#Scale and center the continuous predictor variables
continuous <- data.frame(scale(data.train[,c(continuous.predictors)]))
  
#Split the data between training and validation sets
data.train <- training.split(0.3,data.train[,'Future.State'],categorical,continuous,responses)
rm(categorical,continuous,responses)
  
#Build and compile the artificial neural network
#model <- build.model(data.train$predictors.train,data.train$response.train)
  
#Fit the model to the training data
#history <- fit(
#  model,
#  data.train$predictors.train,
#  data.train$response.train,
#  epochs = 150,
#  batch_size = 1024,
#  validation_data = list(data.train$predictors.validate,data.train$response.validate),
#  verbose = 1,
#  callbacks = list(callback_early_stopping(monitor = 'val_loss', patience = 10)))
#history <- data.frame(history[[2]])
#write_delim(history,'data/history_beneficiaries.txt',delim='|')
#rm(history)
#save_model_hdf5(model,'models/model_beneficiaries.h5')
model <- load_model_hdf5('models/model_beneficiaries.h5')
rm(data.train)

#Load and transform the base data
data <- get.data(data.file,data.delimiter,time,beneficiary.id,base.period:(base.period + length(prediction.periods)))
data <- data[c(time,beneficiary.id,categorical.predictors,continuous.predictors,states)]
data <- left_join(data,rbind(0,state.map(data,time,beneficiary.id,states)),by=states)
data <- data[which(!names(data) %in% states)]
data.base <- training.set(data,time,beneficiary.id,base.period,prediction.periods)
exposure.actual <- one.hot(data.base['Future.State'],drop_last=FALSE)
categorical <- one.hot(data.base[,c(categorical.predictors,'Lag','State')],drop_last=TRUE)
continuous <- data.frame(scale(data.base[,c(continuous.predictors)]))
predictors.base <- as.matrix.data.frame(cbind(categorical,continuous))
rm(data,categorical,continuous)

#Generate predictions for the test data
exposure.predicted <- data.frame(predict(model, predictors.base))
names(exposure.predicted) <- names(exposure.actual)

#Evaluate the model fit for the test data
#metrics <- evaluate(model, predictors.base, as.matrix.data.frame(exposure.actual), verbose = 0)
#metrics <- data.frame(metrics)
#write_delim(metrics,'data/metrics_beneficiaries.txt',delim='|')
#rm(metrics)
rm(predictors.base)

#Add the predictions to the data and save the data for further use
exposure.actual <- cbind(data.base['Lag'] + base.period,data.base[c(beneficiary.id,categorical.predictors,continuous.predictors)],exposure.actual)
exposure.predicted <- cbind(data.base['Lag'] + base.period,data.base[c(beneficiary.id,categorical.predictors,continuous.predictors)],exposure.predicted)
names(exposure.actual) <- c(time,beneficiary.id,categorical.predictors,continuous.predictors,names(exposure.actual[which(!names(exposure.actual) %in% c('Lag',beneficiary.id,categorical.predictors,continuous.predictors))]))
names(exposure.predicted) <- c(time,beneficiary.id,categorical.predictors,continuous.predictors,names(exposure.predicted[which(!names(exposure.predicted) %in% c('Lag',beneficiary.id,categorical.predictors,continuous.predictors))]))
exposure.actual <- melt(exposure.actual,id = c(time,beneficiary.id,categorical.predictors,continuous.predictors))
exposure.predicted <- melt(exposure.predicted,id = c(time,beneficiary.id,categorical.predictors,continuous.predictors))
exposure.actual$variable <- as.character(exposure.actual$variable)
exposure.predicted$variable <- as.character(exposure.predicted$variable)
exposure.actual$variable <- as.double(sapply(strsplit(exposure.actual$variable,split=".",fixed = TRUE),function(x) x[3]))
exposure.predicted$variable <- as.double(sapply(strsplit(exposure.predicted$variable,split=".",fixed = TRUE),function(x) x[3]))
names(exposure.actual) <- c(time,beneficiary.id,categorical.predictors,continuous.predictors,'State','Exposure')
names(exposure.predicted) <- c(time,beneficiary.id,categorical.predictors,continuous.predictors,'State','Exposure')
exposure.actual$Output <- 'Actual'
exposure.predicted$Output <- 'Predicted'
output <- rbind(exposure.actual,exposure.predicted)
rm(data.base,exposure.actual,exposure.predicted)
output <- output[output['Exposure'] != 0,]
for (variable in ageing.variables) {output[,variable] <- output[,variable] + (1/12) * (output[,time] - base.period)}
rm(variable)
data <- get.data(data.file,data.delimiter,time,beneficiary.id,min(training.periods):base.period)
data <- data[c(time,beneficiary.id,categorical.predictors,continuous.predictors,states)]
state.mapping <- rbind(0,state.map(data,time,beneficiary.id,states))
data <- left_join(data,state.mapping,by=states)
data <- data[which(!names(data) %in% states)]
data$Exposure <- 1
data$Output <- 'Actual'
output <- left_join(rbind(output,data),state.mapping,by='State')
rm(data)
write_delim(state.mapping,'data/state_mapping.txt',delim='|')
write_delim(output,'data/output_beneficiaries.txt',delim='|')
rm(output,state.mapping)
