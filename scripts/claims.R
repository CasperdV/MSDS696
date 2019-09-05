rm(list = ls())

library(readr)
library(dplyr)
library(keras)
library(tibble)
library(reshape2)
use_session_with_seed(42, quiet = TRUE)

mapping <- read_delim("data/state_mapping.txt", 
                      "|", escape_double = FALSE, trim_ws = TRUE)
beneficiaries <- read_delim("data/beneficiaries.txt",
                            "|",
                            escape_double = FALSE,
                            col_types = cols(Dependant = col_skip(),Member = col_skip(),Month = col_skip(),Year = col_skip()),
                            trim_ws = TRUE)
beneficiaries <- left_join(beneficiaries,mapping,by=c('Plan','Income','Type','Chronic'))
beneficiaries <- beneficiaries[beneficiaries$Period %in% c(24,36),c('Period','Beneficiary','Gender','Age','Duration','Adults','Children','State')]

genders <- factor(beneficiaries$Gender,levels = sort(unique(beneficiaries$Gender)))
genders <- as.tibble(to_categorical(genders))
genders <- as.tibble(genders)[-1]
names(genders) <- rep(paste0('Gender.',1:length(genders)))
beneficiaries <- cbind(beneficiaries[which(!names(beneficiaries) %in% c('Gender'))],genders)
rm(genders)

states <- factor(beneficiaries$State,levels = sort(unique(beneficiaries$State)))
states <- as.tibble(to_categorical(states))
states <- as.tibble(states)[-1]
names(states) <- rep(paste0('State.',1:length(states)))
beneficiaries <- cbind(beneficiaries[which(!names(beneficiaries) %in% c('State'))],states)
rm(states,mapping)

claims <- read_delim("data/claims.txt",
                     "|",
                     escape_double = FALSE,
                     col_types = cols(),
                     trim_ws = TRUE)

train_labels <- data.frame(claims[claims$Period %in% 25:36,c('Period','Beneficiary','ClaimTotal')])
train_labels <- train_labels[order(train_labels[,'Period'],train_labels[,'Beneficiary']),]
train_labels <- as.tibble(reshape(train_labels,timevar='Period',idvar='Beneficiary',direction='wide'))
for (i in 2:length(train_labels)) {train_labels[is.na(train_labels[,i]),i] <- 0}
train_labels <- left_join(beneficiaries[beneficiaries$Period == 24,],train_labels,by='Beneficiary')[(length(beneficiaries)+1):(length(beneficiaries)+12)]
for (i in 1:12) {train_labels[is.na(train_labels[,i]),i] <- 0}
train_labels <- as.matrix(train_labels)
rm(i)

test_labels <- data.frame(claims[claims$Period %in% 37:48,c('Period','Beneficiary','ClaimTotal')])
test_labels <- test_labels[order(test_labels[,'Period'],test_labels[,'Beneficiary']),]
test_labels <- as.tibble(reshape(test_labels,timevar='Period',idvar='Beneficiary',direction='wide'))
for (i in 2:length(test_labels)) {test_labels[is.na(test_labels[,i]),i] <- 0}
test_labels <- left_join(beneficiaries[beneficiaries$Period == 36,],test_labels,by='Beneficiary')[(length(beneficiaries)+1):(length(beneficiaries)+12)]
for (i in 1:12) {test_labels[is.na(test_labels[,i]),i] <- 0}
test_labels <- as.matrix(test_labels)
rm(i)

#======================================================================>

col <- as.tibble(claims[claims$Period %in% 13:24,])
col <- aggregate(col[,c('ClaimTotal','ClaimHospital','ClaimChronic','ClaimOther')],by=list(Beneficiary = col$Beneficiary),FUN=sum)
train_data <- left_join(beneficiaries[beneficiaries$Period == 24,which(!names(beneficiaries) %in% ('Period'))],col,by='Beneficiary')
train_data[is.na(train_data$ClaimTotal),'ClaimTotal'] <- 0
train_data[is.na(train_data$ClaimHospital),'ClaimHospital'] <- 0
train_data[is.na(train_data$ClaimChronic),'ClaimChronic'] <- 0
train_data[is.na(train_data$ClaimOther),'ClaimOther'] <- 0

col <- as.tibble(claims[claims$Period %in% 19:24,])
col <- aggregate(col[,c('ClaimTotal')],by=list(Beneficiary = col$Beneficiary),FUN=sum)
names(col) <- c('Beneficiary','ClaimSix')
train_data <- left_join(train_data,col,by='Beneficiary')
train_data[is.na(train_data$ClaimSix),'ClaimSix'] <- 0

col <- as.tibble(claims[claims$Period %in% 22:24,])
col <- aggregate(col[,c('ClaimTotal')],by=list(Beneficiary = col$Beneficiary),FUN=sum)
names(col) <- c('Beneficiary','ClaimThree')
train_data <- left_join(train_data,col,by='Beneficiary')
train_data[is.na(train_data$ClaimThree),'ClaimThree'] <- 0

cols <- data.frame(claims[claims$Period %in% 13:24,c('Period','Beneficiary','ClaimTotal')])
cols <- cols[order(cols[,'Period'],cols[,'Beneficiary']),]
cols <- as.tibble(reshape(cols,timevar='Period',idvar='Beneficiary',direction='wide'))
for (i in 2:length(cols)) {cols[is.na(cols[,i]),i] <- 0}
train_data <- left_join(train_data,cols,by='Beneficiary')
rm(cols)
for (i in (length(train_data)-11):length(train_data)) {train_data[is.na(train_data[,i]),i] <- 0}
rm(i)

train_data$ClaimSlope <- apply(train_data[,(length(train_data)-11):length(train_data)],1,function(x) {
  t <- 1:12
  y <- as.numeric(x)
  lm(y~t)$coeff[[2]]
})

col <- as.tibble(claims[claims$Period %in% 13:24,])
col <- aggregate(col[,'ClaimTotal'],by=list(Beneficiary = col$Beneficiary),FUN=max)
names(col) <- c('Beneficiary','ClaimMax')
train_data <- left_join(train_data,col,by='Beneficiary')
train_data[is.na(train_data$ClaimMax),'ClaimMax'] <- 0

col <- as.tibble(claims[claims$Period %in% 13:24,])
col <- aggregate(col[,'ClaimTotal'],by=list(Beneficiary = col$Beneficiary),FUN=mean)
names(col) <- c('Beneficiary','ClaimAvg')
train_data <- left_join(train_data,col,by='Beneficiary')
train_data[is.na(train_data$ClaimAvg),'ClaimAvg'] <- 0

train_data$ClaimAcute <- 0
train_data[train_data$ClaimAvg != 0,'ClaimAcute'] <- train_data[train_data$ClaimAvg != 0,'ClaimMax'] / train_data[train_data$ClaimAvg != 0,'ClaimAvg']
train_data$ClaimAcute <- train_data$ClaimAcute/max(train_data$ClaimAcute)
train_data[train_data$ClaimAcute >= 0.5,'ClaimAcute'] <- 1
train_data[train_data$ClaimAcute != 1,'ClaimAcute'] <- 0

col <- apply(train_data[which(names(train_data) %in% paste0('ClaimTotal.',13:24))] > train_data[,'ClaimAvg'],1,function(x) sum(x))
train_data$ClaimHigher <- col

train_data <- as.tibble(train_data[which(!names(train_data) %in% c('Beneficiary'))])

#======================================================================>

col <- as.tibble(claims[claims$Period %in% 25:36,])
col <- aggregate(col[,c('ClaimTotal','ClaimHospital','ClaimChronic','ClaimOther')],by=list(Beneficiary = col$Beneficiary),FUN=sum)
test_data <- left_join(beneficiaries[beneficiaries$Period == 36,which(!names(beneficiaries) %in% ('Period'))],col,by='Beneficiary')
test_data[is.na(test_data$ClaimTotal),'ClaimTotal'] <- 0
test_data[is.na(test_data$ClaimHospital),'ClaimHospital'] <- 0
test_data[is.na(test_data$ClaimChronic),'ClaimChronic'] <- 0
test_data[is.na(test_data$ClaimOther),'ClaimOther'] <- 0

col <- as.tibble(claims[claims$Period %in% 31:36,])
col <- aggregate(col[,c('ClaimTotal')],by=list(Beneficiary = col$Beneficiary),FUN=sum)
names(col) <- c('Beneficiary','ClaimSix')
test_data <- left_join(test_data,col,by='Beneficiary')
test_data[is.na(test_data$ClaimSix),'ClaimSix'] <- 0

col <- as.tibble(claims[claims$Period %in% 34:36,])
col <- aggregate(col[,c('ClaimTotal')],by=list(Beneficiary = col$Beneficiary),FUN=sum)
names(col) <- c('Beneficiary','ClaimThree')
test_data <- left_join(test_data,col,by='Beneficiary')
test_data[is.na(test_data$ClaimThree),'ClaimThree'] <- 0

cols <- data.frame(claims[claims$Period %in% 25:36,c('Period','Beneficiary','ClaimTotal')])
cols <- cols[order(cols[,'Period'],cols[,'Beneficiary']),]
cols <- as.tibble(reshape(cols,timevar='Period',idvar='Beneficiary',direction='wide'))
for (i in 2:length(cols)) {cols[is.na(cols[,i]),i] <- 0}
test_data <- left_join(test_data,cols,by='Beneficiary')
rm(cols)
for (i in (length(test_data)-11):length(test_data)) {test_data[is.na(test_data[,i]),i] <- 0}
rm(i)

test_data$ClaimSlope <- apply(test_data[,(length(test_data)-11):length(test_data)],1,function(x) {
  t <- 1:12
  y <- as.numeric(x)
  lm(y~t)$coeff[[2]]
})

col <- as.tibble(claims[claims$Period %in% 25:36,])
col <- aggregate(col[,'ClaimTotal'],by=list(Beneficiary = col$Beneficiary),FUN=max)
names(col) <- c('Beneficiary','ClaimMax')
test_data <- left_join(test_data,col,by='Beneficiary')
test_data[is.na(test_data$ClaimMax),'ClaimMax'] <- 0

col <- as.tibble(claims[claims$Period %in% 25:36,])
col <- aggregate(col[,'ClaimTotal'],by=list(Beneficiary = col$Beneficiary),FUN=mean)
names(col) <- c('Beneficiary','ClaimAvg')
test_data <- left_join(test_data,col,by='Beneficiary')
test_data[is.na(test_data$ClaimAvg),'ClaimAvg'] <- 0

test_data$ClaimAcute <- 0
test_data[test_data$ClaimAvg != 0,'ClaimAcute'] <- test_data[test_data$ClaimAvg != 0,'ClaimMax'] / test_data[test_data$ClaimAvg != 0,'ClaimAvg']
test_data$ClaimAcute <- test_data$ClaimAcute/max(test_data$ClaimAcute)
test_data[test_data$ClaimAcute >= 0.5,'ClaimAcute'] <- 1
test_data[test_data$ClaimAcute != 1,'ClaimAcute'] <- 0

col <- apply(test_data[which(names(test_data) %in% paste0('ClaimTotal.',25:36))] > test_data[,'ClaimAvg'],1,function(x) sum(x))
test_data$ClaimHigher <- col

test_data <- as.tibble(test_data[which(!names(test_data) %in% c('Beneficiary'))])

rm(col)

#======================================================================>

rm(claims)

continuous.train <- scale(train_data[which(!names(train_data) %in% c(paste0('State.',1:30),paste0('Gender.',1:2)))])
col_means_train <- attr(continuous.train, "scaled:center") 
col_stddevs_train <- attr(continuous.train, "scaled:scale")
continuous.test <- scale(test_data[which(!names(test_data) %in% c(paste0('State.',1:30),paste0('Gender.',1:2)))], center = col_means_train, scale = col_stddevs_train)
train_data <- as.matrix(cbind(as.tibble(continuous.train),as.tibble(train_data[which(names(train_data) %in% c(paste0('State.',1:30),paste0('Gender.',1:2)))])))
test_data <- as.matrix(cbind(as.tibble(continuous.test),as.tibble(test_data[which(names(test_data) %in% c(paste0('State.',1:30),paste0('Gender.',1:2)))])))
rm(continuous.train,continuous.test,col_means_train,col_stddevs_train)

build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = dim(train_labels)[2])
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  model
}

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

model <- build_model()

history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 200,
  batch_size = 1024,
  validation_split = 0.3,
  verbose = 1,
  callbacks = list(early_stop)
)
save_model_hdf5(model,'models/model_claims.h5')

c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

test_predictions <- as.tibble(predict(model,test_data))
names(test_predictions) <- paste0('ClaimTotal.',37:48)

rm(train_data,train_labels,test_data,early_stop,build_model)

test_predictions <- cbind(beneficiaries[beneficiaries$Period == 36,'Beneficiary'],test_predictions)
names(test_predictions) <- c('Beneficiary',paste0('ClaimTotal.',37:48))
test_predictions <- melt(test_predictions,measure.vars = paste0('ClaimTotal.',37:48), id.vars='Beneficiary')
test_predictions$variable <- as.character(test_predictions$variable)
test_predictions$variable <- sapply(strsplit(test_predictions$variable,'.',fixed = TRUE),function(x) x[2])
test_predictions$variable <- as.numeric(test_predictions$variable)
names(test_predictions) <- c('Beneficiary','Period','ClaimTotal')
test_predictions$Output <- 'Predicted'

test_labels <- cbind(beneficiaries[beneficiaries$Period == 36,'Beneficiary'],as.tibble(test_labels))
names(test_labels) <- c('Beneficiary',paste0('ClaimTotal.',37:48))
test_labels <- melt(test_labels,measure.vars = paste0('ClaimTotal.',37:48), id.vars='Beneficiary')
test_labels$variable <- as.character(test_labels$variable)
test_labels$variable <- sapply(strsplit(test_labels$variable,'.',fixed = TRUE),function(x) x[2])
test_labels$variable <- as.numeric(test_labels$variable)
names(test_labels) <- c('Beneficiary','Period','ClaimTotal')
test_labels$Output <- 'Actual'

claims_output <- rbind(test_predictions,test_labels)

rm(beneficiaries,test_predictions,test_labels)

output <- read_delim("data/output_beneficiaries.txt", "|", 
                     escape_double = FALSE, trim_ws = TRUE)

output <- left_join(output,claims_output,by=c('Period','Beneficiary','Output'))
output[is.na(output$ClaimTotal),'ClaimTotal'] <- 0

rm(claims_output)

write_delim(output,'data/output_claims.txt',delim='|')
write_delim(data.frame(history),'data/history_claims.txt',delim='|')
write_delim(data.frame(mae = mae,loss = loss),'data/metrics_claims.txt',delim='|')

