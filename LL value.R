
dataset1 <- read.csv ("predictions_Pranav.csv")

dataset1["choice"] <- NA
dataset1["choice"] <- ifelse(dataset1$chosen_reward == dataset1$outcomeB,dataset1$choiceprobB, 
                             dataset1$choiceprobA)
ll_val = sum(log(dataset1$choice))*-1
average = exp(ll_val/nrow(dataset1)*-1)

prob_cal <- as.data.frame(dataset1$choice[dataset1["choice"]<1])
