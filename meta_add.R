cl<-makeCluster(11)
registerDoParallel(cl)

res <- foreach(i = 1:nrow(amer_novels), .combine=rbind)  %dopar% {
  
  
  res <- rep(NA, 6)
  
  res[1] <- novels[which(as.character(novels[,1])==as.character(amer_novels[i,2]) & novels[,9] == 1),7][1] == 0 # white
  res[2] <- novels[which(as.character(novels[,1])==as.character(amer_novels[i,2]) & novels[,9] == 1),7][1] == 1 #black
  res[3] <- novels[which(as.character(novels[,1])==as.character(amer_novels[i,2]) & novels[,9] == 1),7][1] == 2# poc
  res[4] <- novels[which(as.character(novels[,1])==as.character(amer_novels[i,3]) & novels[,9] == 1),7][1] == 0 # white
  res[5] <- novels[which(as.character(novels[,1])==as.character(amer_novels[i,3]) & novels[,9] == 1),7][1] == 1 #black
  res[6] <- novels[which(as.character(novels[,1])==as.character(amer_novels[i,3]) & novels[,9] == 1),7][1] == 2# poc

  res}
names(res) = c("WHITE1", "BLACK1", "POC1",
                "WHITE2", "BLACK2", "POC2")


meta_2$GENDER = ifelse(meta_2$GENDER==1, 'Female',
                       ifelse(meta_2$GENDER==0, 'Male',NA))

meta_2$RACE = ifelse(meta_2$RACE==0, 'White',
                     ifelse(meta_2$RACE==1, 'Black',
                            ifelse(meta_2$RACE==2,'POC','NA')))


res <- foreach(i = 1:nrow(amer_novels), .combine=rbind)  %dopar% {
  
  
  res <- rep(NA, 6)
  
  res[1] = meta_2[which(as.character(meta_2[,1])==as.character(amer_novels[i,2])),4]
  res[2] = meta_2[which(as.character(meta_2[,1])==as.character(amer_novels[i,3])),4]
  
  res[3] = meta_2[which(as.character(meta_2[,1])==as.character(amer_novels[i,3])),5]
  res[4] = meta_2[which(as.character(meta_2[,1])==as.character(amer_novels[i,3])),5]

  res[5] = meta_2[which(as.character(meta_2[,1])==as.character(amer_novels[i,3])),7]
  res[6] = meta_2[which(as.character(meta_2[,1])==as.character(amer_novels[i,3])),7]

  
  
  res}


names(res) = c("Gender1", "Gender2",
               'Race1', 'Race2',
               'P1', 'P2')

amer_novels = cbind(amer_novels, res)

amer_novels$Female1 = ifelse(amer_novels$`1`=='Female',1,0)
amer_novels$Female2 = ifelse(amer_novels$`2`=='Female',1,0)

amer_novels$Male1 = ifelse(amer_novels$`1`=='Male',1,0)
amer_novels$Male2 = ifelse(amer_novels$`2`=='Male',1,0)

amer_novels$WHITE1 = ifelse(amer_novels$`3`=='White',1,0)
amer_novels$WHITE2 = ifelse(amer_novels$`4`=='White',1,0)

amer_novels$BLACK1 = ifelse(amer_novels$`3`=='Black',1,0)
amer_novels$BLACK2 = ifelse(amer_novels$`4`=='Black',1,0)

amer_novels$POC1 = ifelse(amer_novels$`3`=='POC',1,0)
amer_novels$POC2 = ifelse(amer_novels$`4`=='POC',1,0)

amer_novels$PUB2_0_1 = ifelse(amer_novels$`5`=='0',1,0)
amer_novels$PUB2_1_1 = ifelse(amer_novels$`5`=='1',1,0)
amer_novels$PUB2_2_1 = ifelse(amer_novels$`5`=='2',1,0)
amer_novels$PUB2_3_1 = ifelse(amer_novels$`5`=='3',1,0)
amer_novels$PUB2_4_1 = ifelse(amer_novels$`5`=='4',1,0)
amer_novels$PUB2_5_1 = ifelse(amer_novels$`5`=='5',1,0)
amer_novels$PUB2_6_1 = ifelse(amer_novels$`5`=='6',1,0)
amer_novels$PUB2_7_1 = ifelse(amer_novels$`5`=='7',1,0)
amer_novels$PUB2_8_1 = ifelse(amer_novels$`5`=='8',1,0)
amer_novels$PUB2_9_1 = ifelse(amer_novels$`5`=='9',1,0)
amer_novels$PUB2_10_1 = ifelse(amer_novels$`5`=='10',1,0)

amer_novels$PUB2_0_2 = ifelse(amer_novels$`6`=='0',1,0)
amer_novels$PUB2_1_2 = ifelse(amer_novels$`6`=='1',1,0)
amer_novels$PUB2_2_2 = ifelse(amer_novels$`6`=='2',1,0)
amer_novels$PUB2_3_2 = ifelse(amer_novels$`6`=='3',1,0)
amer_novels$PUB2_4_2 = ifelse(amer_novels$`6`=='4',1,0)
amer_novels$PUB2_5_2 = ifelse(amer_novels$`6`=='5',1,0)
amer_novels$PUB2_6_2 = ifelse(amer_novels$`6`=='6',1,0)
amer_novels$PUB2_7_2 = ifelse(amer_novels$`6`=='7',1,0)
amer_novels$PUB2_8_2 = ifelse(amer_novels$`6`=='8',1,0)
amer_novels$PUB2_9_2 = ifelse(amer_novels$`6`=='9',1,0)
amer_novels$PUB2_10_2 = ifelse(amer_novels$`6`=='10',1,0)


