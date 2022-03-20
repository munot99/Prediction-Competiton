library(dplyr)
setwd("D:\\BSE\\BSE Material\\sem 2\\Behaviorial Econ\\Codes and Model\\Prediction competition")
dataset1 <- read.csv ("predictions_recency.csv")


dataset1["choice"] <- NA
dataset1["choice"] <- ifelse(dataset1$chosen_reward == dataset1$outcomeB,dataset1$choiceprobB, 
                             dataset1$choiceprobA)
probrecency<- dataset1 %>%
  filter(choice < 1 )%>%
  select(subject_id,choice)%>%
  group_by(subject_id)%>%
  summarise(ll_value = -sum(log(choice)))



Alpha <- probalpha[probalpha$ll_value <=  max(probsoftmax$ll_value),]
Base <- probbase[probbase$ll_value <= max(probsoftmax$ll_value),]
Recency <-probrecency[probrecency$ll_value <= max(probsoftmax$ll_value),]

compare <- inner_join(Base, Alpha, by="subject_id")

compare <- inner_join(compare, Recency, by="subject_id")

compare <- inner_join(compare,probsoftmax , by ="subject_id")

colnames(compare) <- c("subj_id","Base","Pa","Recency","Softmax")

base_test <- compare %>%
  filter(compare$Base<=compare$Pa & compare$Base<=compare$Recency & compare$Base <= compare$Softmax)

base <- base_test$subj_id

alpha_test <- compare %>%
  filter(compare$Pa<compare$Recency
         & compare$Pa<compare$Base & compare$Pa< compare$Softmax)

alpha <- alpha_test$subj_id

recency_test <- compare %>%
  filter(compare$Recency<=compare$Pa& compare$Recency<compare$Base & compare$Recency < compare$Softmax)

recency <- recency_test$subj_id

softmax_test<- compare %>%
  filter(compare$Softmax<compare$Pa & compare$Softmax < compare$Recency & compare$Softmax <= compare$Base)
softmax <- softmax_test$subj_id

x <- setdiff(Base$subject_id,base)
x <- setdiff(x,recency)
x <- setdiff(x,alpha)
x<- setdiff(x,softmax)


base <- append(base,x)

y <- setdiff(Alpha$subject_id,alpha)
y <- setdiff(y,base)
y <- setdiff(y,recency)
y <- setdiff(y,softmax)

alpha <- append(alpha,y)

z <- setdiff(Recency$subject_id,recency)
z <- setdiff(z,base)
z <- setdiff(z,alpha)
z <-setdiff(z,softmax)

recency <- append(recency,z)


k <- setdiff(probsoftmax$subject_id,softmax)
k <- setdiff(k,base)
k <- setdiff(k,alpha)
k <-setdiff(k,recency)

softmax <- append(softmax,k)


softmax <- append(base,alpha)
softmax <- append(softmax,recency)
softmax <- setdiff(unique(probalpha$subject_id),softmax)

length(softmax)+length(alpha)+length(base)+length(recency)
