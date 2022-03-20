library(dplyr)

dataset <- ()

block_1 <-dataset[dataset$choiceprobA == -1000 & dataset$block_id == 1 ,]
block_2 <-dataset[dataset$choiceprobA == -1000 & dataset$block_id == 2 ,]
block_3 <-dataset[dataset$choiceprobA == -1000 & dataset$block_id == 3 ,]
block_4 <-dataset[dataset$choiceprobA == -1000 & dataset$block_id == 4 ,]

subj_block1 <- block_1$subject_id 
subj_block2 <- block_2$subject_id #softmax
subj_block3 <- block_3$subject_id #baseline
subj_block4<- block_4$subject_id #baseline


colMeans(block_1[,c("outcomeA","outcomeB")])

colMeans(block_2[,c("outcomeA","outcomeB")])

colMeans(block_3[,c("outcomeA","outcomeB")])

colMeans(block_4[,c("outcomeA","outcomeB")])

mean(block_4$subject_id == 49)