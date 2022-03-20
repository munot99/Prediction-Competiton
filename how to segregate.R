PA <- probalpha[probalpha$ll_value <=  max(probgamma$ll_value),]
Base <- probbase[probbase$ll_value <= max(probgamma$ll_value),]
compare <- inner_join(Base, PA, by="subject_id")

colnames(compare) <- c("subj_id","Base","Pa")


base <- compare$subj_id[compare$Base>=compare$Pa]
AS <- setdiff(compare$subj_id,base)


x <- setdiff(Base$subject_id,base)
base <- append(base,setdiff(x,AS))
y <- setdiff(PA$subject_id,AS)
AS <- append(AS,setdiff(y,base))


softmax <- append(AS,base)
softmax <- setdiff(participants,softmax)

