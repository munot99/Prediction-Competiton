rm(list=ls())

if (!require('DEoptim', character.only = TRUE)) {
  install.packages('DEoptim', dependencies = TRUE)
  require('DEoptim')
}
library(dplyr)




getwd()
setwd("D:\\BSE\\BSE Material\\sem 2\\Behaviorial Econ\\Codes and Model\\Prediction competition")
dataset <- read.csv('dataset_censored.csv')

block_1 <-dataset[dataset$choiceprobA == -1000 & dataset$block_id == 1 ,]
block_2 <-dataset[dataset$choiceprobA == -1000 & dataset$block_id == 2 ,]
block_3 <-dataset[dataset$choiceprobA == -1000 & dataset$block_id == 3 ,]
block_4 <-dataset[dataset$choiceprobA == -1000 & dataset$block_id == 4 ,]

subj_block1 <- unique(block_1$subject_id )
subj_block2 <- unique(block_2$subject_id) #softmax
subj_block3 <- unique(block_3$subject_id) #baseline
subj_block4<- unique(block_4$subject_id)#baseline

softmax <- append(subj_block1,subj_block2)
base <- append(subj_block3,subj_block4)

baseline<- dataset %>%
  filter(subject_id %in% base)

str(dataset)
apply(dataset, 2, unique)

str(baseline)
apply(baseline, 2, unique)

model_simple <- function(X, fit_data, block_to_predict, return_predictions){
  #fixed parameter
  Q0               <- 0
  #free parameters
  alpha            <- X[1] #learning parameter for RPE > 0
  epsilon          <- X[2] #steady hand 
  greediness       <- 1 - epsilon #greediness
  # some data pretreatment
  orig_columns <- colnames(fit_data)
  fit_data[c('QA', 'QB')] <- NA
  
  for (block in unique(fit_data[['block_id']])) {
    block_data <- fit_data[fit_data[['block_id']] == block, ]
    n_trials <- nrow(block_data)
    rewards <- block_data[, c('outcomeA', 'outcomeB')]
    Q <- matrix(NA, nrow=n_trials, ncol=2) #just creating 2 columns
    Q[1,] <- Q0
    for (trial in 2:n_trials) {
      RPE <- as.numeric(rewards[trial-1, ]) - Q[trial-1,] # reward Prediction Error #diff between reward achieved after trial 1 and predicting future rewards
      Q[trial,] <- (1-alpha)*Q[trial-1,] + alpha* RPE 
      
      #they might influence my probability 
      #Q values are reward estimates
      
      
      #we add a extra 1-alpha term to include reinforcement learning ( if alpha is learning rate )
      #Recency depends on the influence of previous prediction error
    }
    fit_data[fit_data[['block_id']] == block, c('QA', 'QB')] <- Q
  }
  
  # get from Q values to choice probabilities
  decision_variable <- fit_data[c('QA', 'QB')]
  choice_probs <- matrix(1/2*epsilon, nrow=nrow(decision_variable), ncol=ncol(decision_variable)) 
  #the parameters with taken in account all epsilon values 
  dummy_max <- decision_variable == apply(decision_variable, 1, max) #finds which of value between A & B is higher
  locator <- apply(dummy_max, 1, sum) == 1 #if one is higher then we choose that variable to change our choice
  #TRUE + FALSE  = 1
  choice_probs[locator, ] <- choice_probs[locator, ] + dummy_max[locator, ] * greediness
  #our steady hand is influenced by our greediness to select that variable more 
  #can we try influencing loss aversion here
  locator <- apply(dummy_max, 1, sum) == 2
  #if both values are the equal to each other then our choice is distributed by our greediness
  choice_probs[locator, ] <- choice_probs[locator, ] + dummy_max[locator, ] * greediness/2
  colnames(choice_probs) <- c('QA', 'QB') # NOT Q, BUT PR
  # verification:
  # all(apply(choice_probs, 1, sum) == 1)
  choice_probs[apply(choice_probs, 1, sum) != 1] <- 1/2
  
  if (return_predictions) {
    predictions <- round(choice_probs[fit_data['block_id'] == block_to_predict, ], 2)
    fit_data[fit_data['block_id'] == block_to_predict, c('choiceprobA', 'choiceprobB')] <- predictions
    return(fit_data[orig_columns])
  }
  else {
    log_choice_probs <- log(as.data.frame(choice_probs))[fit_data['block_id'] != block_to_predict, ]
    deviance_data <- fit_data[fit_data['block_id'] != block_to_predict, ]
    deviance <- -2*sum(c(log_choice_probs[deviance_data[['choiceprobA']]==1, 'QA'],
                         log_choice_probs[deviance_data[['choiceprobB']]==1, 'QB']))
    return(deviance)
  }
}

participants <- unique(baseline[['subject_id']])
subj <- 1
for (subj in subj:length(participants)){
  print(c(subj, length(participants)))
  subj_data <- baseline[baseline[['subject_id']] == participants[subj], ]
  prediction_block_id <- unique(subj_data[subj_data$choiceprobA == -1000, 'block_id'])
  
  # test purposes
  # X <- runif(n=2)
  # fit_data <- subj_data
  # block_to_predict <- prediction_block_id
  # return_predictions <- FALSE
  
  fit_DE <- DEoptim(fn=model_simple, lower=c(0, 0), upper=c(1, 1), fit_data=subj_data, block_to_predict=prediction_block_id, return_predictions=FALSE, control=list(NP = 100 ,itermax = 200))
  MLE_deviance <- fit_DE$optim$bestval
  MLE_params <- as.vector(fit_DE$optim$bestmem) #why do we use this for X ? what does it mean 
  MLE_predictions <- model_simple(X=MLE_params, fit_data=subj_data, block_to_predict=prediction_block_id, return_predictions=TRUE)
  baseline[baseline[['subject_id']] == participants[subj], c('choiceprobA', 'choiceprobB')] <- MLE_predictions[, c('choiceprobA', 'choiceprobB')]
  AIC <- MLE_deviance + 2*length(MLE_params)
  BIC <- MLE_deviance + log(90)*length(MLE_params) # 90 = 3 blocks with 30 trials each
  random_model <- log(1/2) * 90 * -2
}

#softmax
library(DMwR2)

softmax_data<- dataset %>%
  filter(subject_id %in% softmax)

str(dataset)
apply(dataset, 2, unique)

str(softmax_data)
apply(softmax_data, 2, unique)

model_simple <- function(X, fit_data, block_to_predict, return_predictions){
  #fixed parameter
  Q0               <- 0
  #free parameters
  alpha            <- X[1] #learning parameter for RPE > 0
  epsilon          <- X[2] #steady hand 
  greediness       <- 1 - epsilon #greediness
  
  # some data pretreatment
  orig_columns <- colnames(fit_data)
  fit_data[c('QA', 'QB')] <- NA
  
  for (block in unique(fit_data[['block_id']])) {
    block_data <- fit_data[fit_data[['block_id']] == block, ]
    n_trials <- nrow(block_data)
    rewards <- block_data[, c('outcomeA', 'outcomeB')]
    Q <- matrix(NA, nrow=n_trials, ncol=2) #just creating 2 columns
    Q[1,] <- Q0
    for (trial in 2:n_trials) {
      RPE <- as.numeric(rewards[trial-1, ]) - Q[trial-1,] # reward Prediction Error #diff between reward achieved after trial 1 and predicting future rewards
      Q[trial,] <- Q[trial-1,] + alpha* RPE 
      
      #they might influence my probability 
      #Q values are reward estimates
      
      
      #we add a extra 1-alpha term to include reinforcement learning ( if alpha is learning rate )
      #Recency depends on the influence of previous prediction error
    }
    fit_data[fit_data[['block_id']] == block, c('QA', 'QB')] <- Q
  }
  
  # get from Q values to choice probabilities
  decision_variable <- fit_data[c('QA', 'QB')]
  choice_probs <- matrix(SoftMax(decision_variable), nrow=nrow(decision_variable), ncol=ncol(decision_variable)) 
  #the parameters with taken in account all epsilon values 
  dummy_max <- decision_variable == apply(decision_variable, 1, max) #finds which of value between A & B is higher
  locator <- apply(dummy_max, 1, sum) == 1 #if one is higher then we choose that variable to change our choice
  #TRUE + FALSE  = 1
  choice_probs[locator, ] <- choice_probs[locator, ] + dummy_max[locator, ] * greediness
  #our steady hand is influenced by our greediness to select that variable more 
  #can we try influencing loss aversion here
  locator <- apply(dummy_max, 1, sum) == 2
  #if both values are the equal to each other then our choice is distributed by our greediness
  choice_probs[locator, ] <- choice_probs[locator, ] + dummy_max[locator, ] * greediness/2
  colnames(choice_probs) <- c('QA', 'QB') # NOT Q, BUT PR
  # verification:
  # all(apply(choice_probs, 1, sum) == 1)
  choice_probs[apply(choice_probs, 1, sum) != 1] <- 1/2
  
  if (return_predictions) {
    predictions <- round(choice_probs[fit_data['block_id'] == block_to_predict, ], 2)
    fit_data[fit_data['block_id'] == block_to_predict, c('choiceprobA', 'choiceprobB')] <- predictions
    return(fit_data[orig_columns])
  }
  else {
    log_choice_probs <- log(as.data.frame(choice_probs))[fit_data['block_id'] != block_to_predict, ]
    deviance_data <- fit_data[fit_data['block_id'] != block_to_predict, ]
    deviance <- -2*sum(c(log_choice_probs[deviance_data[['choiceprobA']]==1, 'QA'],
                         log_choice_probs[deviance_data[['choiceprobB']]==1, 'QB']))
    return(deviance)
  }
}


participants <- unique(softmax_data[['subject_id']])
subj <- 1
for (subj in subj:length(participants)){
  print(c(subj, length(participants)))
  subj_data <- softmax_data[softmax_data[['subject_id']] == participants[subj], ]
  prediction_block_id <- unique(subj_data[subj_data$choiceprobA == -1000, 'block_id'])
  
  # test purposes
  #X <- runif(n=2)
  # fit_data <- subj_data
  # block_to_predict <- prediction_block_id
  # return_predictions <- FALSE
  
  fit_DE <- DEoptim(fn=model_simple, lower=c(0, 0), upper=c(1, 1), fit_data=subj_data, block_to_predict=prediction_block_id, return_predictions=FALSE, control=list(NP = 100 ,itermax = 200))
  MLE_deviance <- fit_DE$optim$bestval
  MLE_params <- as.vector(fit_DE$optim$bestmem) #why do we use this for X ? what does it mean 
  MLE_predictions <- model_simple(X=MLE_params, fit_data=subj_data, block_to_predict=prediction_block_id, return_predictions=TRUE)
  softmax_data[softmax_data[['subject_id']] == participants[subj], c('choiceprobA', 'choiceprobB')] <- MLE_predictions[, c('choiceprobA', 'choiceprobB')]
  AIC <- MLE_deviance + 2*length(MLE_params)
  BIC <- MLE_deviance + log(90)*length(MLE_params) # 90 = 3 blocks with 30 trials each
  random_model <- log(1/2) * 90 * -2
}

dataset <- rbind(baseline,softmax_data)

write.csv(x=dataset, file='predictions_MUNOT_Session18b.csv', row.names=FALSE)



