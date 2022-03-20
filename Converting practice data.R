dataset<- read.csv('dataset_practice.csv')

participants <- unique(dataset[['subject_id']])
subj <- 1
block <- c(1,2,3,4)
set.seed(1234)
for (subj in participants){
  dataset$choiceprobA[dataset$subject_id == subj & dataset$block_id == sample(block,1)] <- -1000
}

dataset$choiceprobB[dataset$choiceprobA == -1000] <- -1000
