library(dplyr)
getwd()
setwd("D:\\BSE\\BSE Material\\sem 2\\Behaviorial Econ\\Codes and Model\\Prediction competition")
dataset1 <- read.csv ("predictions_softmax.csv")


dataset1["choice"] <- NA
dataset1["choice"] <- ifelse(dataset1$chosen_reward == dataset1$outcomeB,dataset1$choiceprobB, 
                             dataset1$choiceprobA)
probsoftmax<- dataset1 %>%
  filter(choice < 1)%>%
  select(subject_id,choice)%>%
  group_by(subject_id)%>%
  summarise(ll_value = -sum(log(choice)))

  


