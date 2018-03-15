############ Data Cleaning CRC Script ##########

library(stringr)
library(dplyr)
library(reshape2)
library(foreach)
library(doParallel)
library(parallel)

# cluster me mum    
cl<-makeCluster(11)
registerDoParallel(cl)

novels <- read.csv("EDGE_LIST_FINAL2.csv")

#novels <- read.csv("C:/Users/Administrator/Desktop/Notre Dame/Center for Social Research/work/EDGE_LIST_FINAL2.csv")

novels[,c(1,2,3,4)] = apply(novels[,c(1,2,3,4)],2,as.character)

# find all unique combinations
# date ranges: 1965-1970, 1971-1975, 1976-1980, 1981-1985, 1986-1990, 1991-1995, and 1996-2000  

names(novels)[9] <- "connect"
names(novels)[11] <- "PUB2"

nov = subset(novels, YEAR >= 65 & YEAR <= 70) 

combos = (combn(unique(nov$TITLE),2))
all_combs = paste(combos[1,],combos[2,])

combos_df = unique(expand.grid(unique(nov$TITLE),unique(nov$TITLE)))
combos_df$a = paste(combos_df$Var1,combos_df$Var2)

uniq_combos = combos_df[(combos_df$a %in% all_combs),]
colnames(uniq_combos) = c('TITLE','TITLE2','Comb')

# store the year range first
uniq_combos$Years = rep('65-70',nrow(uniq_combos))    

# number of connections
res <- foreach(i = 1:nrow(uniq_combos), .combine=rbind)  %dopar% {
  res <- rep(NA, 53)
  res[1] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),2] %in%
                  nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),2])
  res[2] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1])
  res[3] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1])
  
  res[4] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 1
  res[5] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 0
  res[6] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 1
  res[7] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 0
  
  res[8] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 1
  res[9] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 2
  res[10] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 0
  
  res[11] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 1
  res[12] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 2
  res[13] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 0
  
  res[14] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ACAD']")
  res[15] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['BLACK']")
  res[16] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ELITE']")
  res[17] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['GENRE']")
  res[18] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['LIT']")
  res[19] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MAIN']")
  res[20] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MISC']")
  res[21] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['TRADE']")
  res[22] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "[nan]")
  
  res[23] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ACAD']")
  res[24] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['BLACK']")
  res[25] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ELITE']")
  res[26] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['GENRE']")
  res[27] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['LIT']")
  res[28] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MAIN']")
  res[29] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MISC']")
  res[30] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['TRADE']")
  res[31] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "[nan]")
  
  for(j in 0:10){
    res[32 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),11] == j)
    res[43 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),11] == j)
  }
  res
}

names(res) <- c("connections_books", "RaceMatch", "GenderMatch", "Female1", "Male1",
                "Female2", "Male2", "BLACK1", "POC1", "WHITE1", "BLACK2", "POC2", "WHITE2",
                "J2_ACAD_1", "J2_BLACK_1", "J2_ELITE_1", "J2_GENRE_1", "J2_LIT_1", "J2_MAIN_1",
                "J2_MISC_1", "J2_TRADE_1" , "J2_nan_1", "J2_ACAD_2", "J2_BLACK_2", "J2_ELITE_2", 
                "J2_GENRE_2", "J2_LIT_2", "J2_MAIN_2", "J2_MISC_2", "J2_TRADE_2" , "J2_nan_2", "PUB2_0_1", 
                "PUB2_1_1", "PUB2_2_1", "PUB2_3_1", "PUB2_4_1", "PUB2_5_1", "PUB2_6_1", "PUB2_7_1",
                "PUB2_8_1", "PUB2_9_1", "PUB2_10_1", "PUB2_0_2", "PUB2_1_2", "PUB2_2_2", "PUB2_3_2",
                "PUB2_4_2", "PUB2_5_2", "PUB2_6_2", "PUB2_7_2", "PUB2_8_2", "PUB2_9_2", "PUB2_10_2")

uniq_combos <- cbind(uniq_combos, res)

save(uniq_combos, file="text_econ_1.rda")  


nov = subset(novels, YEAR >= 71 & YEAR <= 75) 

combos = (combn(unique(nov$TITLE),2))
all_combs = paste(combos[1,],combos[2,])

combos_df = unique(expand.grid(unique(nov$TITLE),unique(nov$TITLE)))
combos_df$a = paste(combos_df$Var1,combos_df$Var2)

uniq_combos = combos_df[(combos_df$a %in% all_combs),]
colnames(uniq_combos) = c('TITLE','TITLE2','Comb')

# store the year range first
uniq_combos$Years = rep('71-75',nrow(uniq_combos))    

# number of connections
res <- foreach(i = 1:nrow(uniq_combos), .combine=rbind)  %dopar% {
  res <- rep(NA, 53)
  res[1] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),2] %in%
                  nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),2])
  res[2] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1])
  res[3] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1])
  
  res[4] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 1
  res[5] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 0
  res[6] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 1
  res[7] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 0
  
  res[8] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 1
  res[9] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 2
  res[10] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 0
  
  res[11] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 1
  res[12] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 2
  res[13] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 0
  
  res[14] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ACAD']")
  res[15] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['BLACK']")
  res[16] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ELITE']")
  res[17] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['GENRE']")
  res[18] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['LIT']")
  res[19] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MAIN']")
  res[20] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MISC']")
  res[21] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['TRADE']")
  res[22] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "[nan]")
  
  res[23] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ACAD']")
  res[24] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['BLACK']")
  res[25] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ELITE']")
  res[26] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['GENRE']")
  res[27] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['LIT']")
  res[28] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MAIN']")
  res[29] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MISC']")
  res[30] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['TRADE']")
  res[31] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "[nan]")
  
  for(j in 0:10){
    res[32 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),11] == j)
    res[43 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),11] == j)
  }
  res
}

names(res) <- c("connections_books", "RaceMatch", "GenderMatch", "Female1", "Male1",
                "Female2", "Male2", "BLACK1", "POC1", "WHITE1", "BLACK2", "POC2", "WHITE2",
                "J2_ACAD_1", "J2_BLACK_1", "J2_ELITE_1", "J2_GENRE_1", "J2_LIT_1", "J2_MAIN_1",
                "J2_MISC_1", "J2_TRADE_1" , "J2_nan_1", "J2_ACAD_2", "J2_BLACK_2", "J2_ELITE_2", 
                "J2_GENRE_2", "J2_LIT_2", "J2_MAIN_2", "J2_MISC_2", "J2_TRADE_2" , "J2_nan_2", "PUB2_0_1", 
                "PUB2_1_1", "PUB2_2_1", "PUB2_3_1", "PUB2_4_1", "PUB2_5_1", "PUB2_6_1", "PUB2_7_1",
                "PUB2_8_1", "PUB2_9_1", "PUB2_10_1", "PUB2_0_2", "PUB2_1_2", "PUB2_2_2", "PUB2_3_2",
                "PUB2_4_2", "PUB2_5_2", "PUB2_6_2", "PUB2_7_2", "PUB2_8_2", "PUB2_9_2", "PUB2_10_2")

uniq_combos <- cbind(uniq_combos, res)

save(uniq_combos, file="text_econ_2.rda")  



nov = subset(novels, YEAR >= 76 & YEAR <= 80) 

combos = (combn(unique(nov$TITLE),2))
all_combs = paste(combos[1,],combos[2,])

combos_df = unique(expand.grid(unique(nov$TITLE),unique(nov$TITLE)))
combos_df$a = paste(combos_df$Var1,combos_df$Var2)

uniq_combos = combos_df[(combos_df$a %in% all_combs),]
colnames(uniq_combos) = c('TITLE','TITLE2','Comb')

# store the year range first
uniq_combos$Years = rep('76-80',nrow(uniq_combos))    

# number of connections
res <- foreach(i = 1:nrow(uniq_combos), .combine=rbind)  %dopar% {
  res <- rep(NA, 53)
  res[1] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),2] %in%
                  nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),2])
  res[2] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1])
  res[3] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1])
  
  res[4] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 1
  res[5] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 0
  res[6] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 1
  res[7] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 0
  
  res[8] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 1
  res[9] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 2
  res[10] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 0
  
  res[11] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 1
  res[12] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 2
  res[13] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 0
  
  res[14] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ACAD']")
  res[15] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['BLACK']")
  res[16] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ELITE']")
  res[17] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['GENRE']")
  res[18] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['LIT']")
  res[19] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MAIN']")
  res[20] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MISC']")
  res[21] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['TRADE']")
  res[22] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "[nan]")
  
  res[23] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ACAD']")
  res[24] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['BLACK']")
  res[25] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ELITE']")
  res[26] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['GENRE']")
  res[27] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['LIT']")
  res[28] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MAIN']")
  res[29] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MISC']")
  res[30] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['TRADE']")
  res[31] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "[nan]")
  
  for(j in 0:10){
    res[32 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),11] == j)
    res[43 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),11] == j)
  }
  res
}

names(res) <- c("connections_books", "RaceMatch", "GenderMatch", "Female1", "Male1",
                "Female2", "Male2", "BLACK1", "POC1", "WHITE1", "BLACK2", "POC2", "WHITE2",
                "J2_ACAD_1", "J2_BLACK_1", "J2_ELITE_1", "J2_GENRE_1", "J2_LIT_1", "J2_MAIN_1",
                "J2_MISC_1", "J2_TRADE_1" , "J2_nan_1", "J2_ACAD_2", "J2_BLACK_2", "J2_ELITE_2", 
                "J2_GENRE_2", "J2_LIT_2", "J2_MAIN_2", "J2_MISC_2", "J2_TRADE_2" , "J2_nan_2", "PUB2_0_1", 
                "PUB2_1_1", "PUB2_2_1", "PUB2_3_1", "PUB2_4_1", "PUB2_5_1", "PUB2_6_1", "PUB2_7_1",
                "PUB2_8_1", "PUB2_9_1", "PUB2_10_1", "PUB2_0_2", "PUB2_1_2", "PUB2_2_2", "PUB2_3_2",
                "PUB2_4_2", "PUB2_5_2", "PUB2_6_2", "PUB2_7_2", "PUB2_8_2", "PUB2_9_2", "PUB2_10_2")

uniq_combos <- cbind(uniq_combos, res)

save(uniq_combos, file="text_econ_3.rda")  


nov = subset(novels, YEAR >= 81 & YEAR <= 85) 

combos = (combn(unique(nov$TITLE),2))
all_combs = paste(combos[1,],combos[2,])

combos_df = unique(expand.grid(unique(nov$TITLE),unique(nov$TITLE)))
combos_df$a = paste(combos_df$Var1,combos_df$Var2)

uniq_combos = combos_df[(combos_df$a %in% all_combs),]
colnames(uniq_combos) = c('TITLE','TITLE2','Comb')

# store the year range first
uniq_combos$Years = rep('81-85',nrow(uniq_combos))    

# number of connections
res <- foreach(i = 1:nrow(uniq_combos), .combine=rbind)  %dopar% {
  res <- rep(NA, 53)
  res[1] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),2] %in%
                  nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),2])
  res[2] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1])
  res[3] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1])
  
  res[4] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 1
  res[5] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 0
  res[6] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 1
  res[7] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 0
  
  res[8] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 1
  res[9] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 2
  res[10] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 0
  
  res[11] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 1
  res[12] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 2
  res[13] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 0
  
  res[14] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ACAD']")
  res[15] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['BLACK']")
  res[16] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ELITE']")
  res[17] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['GENRE']")
  res[18] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['LIT']")
  res[19] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MAIN']")
  res[20] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MISC']")
  res[21] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['TRADE']")
  res[22] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "[nan]")
  
  res[23] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ACAD']")
  res[24] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['BLACK']")
  res[25] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ELITE']")
  res[26] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['GENRE']")
  res[27] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['LIT']")
  res[28] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MAIN']")
  res[29] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MISC']")
  res[30] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['TRADE']")
  res[31] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "[nan]")
  
  for(j in 0:10){
    res[32 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),11] == j)
    res[43 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),11] == j)
  }
  res
}

names(res) <- c("connections_books", "RaceMatch", "GenderMatch", "Female1", "Male1",
                "Female2", "Male2", "BLACK1", "POC1", "WHITE1", "BLACK2", "POC2", "WHITE2",
                "J2_ACAD_1", "J2_BLACK_1", "J2_ELITE_1", "J2_GENRE_1", "J2_LIT_1", "J2_MAIN_1",
                "J2_MISC_1", "J2_TRADE_1" , "J2_nan_1", "J2_ACAD_2", "J2_BLACK_2", "J2_ELITE_2", 
                "J2_GENRE_2", "J2_LIT_2", "J2_MAIN_2", "J2_MISC_2", "J2_TRADE_2" , "J2_nan_2", "PUB2_0_1", 
                "PUB2_1_1", "PUB2_2_1", "PUB2_3_1", "PUB2_4_1", "PUB2_5_1", "PUB2_6_1", "PUB2_7_1",
                "PUB2_8_1", "PUB2_9_1", "PUB2_10_1", "PUB2_0_2", "PUB2_1_2", "PUB2_2_2", "PUB2_3_2",
                "PUB2_4_2", "PUB2_5_2", "PUB2_6_2", "PUB2_7_2", "PUB2_8_2", "PUB2_9_2", "PUB2_10_2")

uniq_combos <- cbind(uniq_combos, res)

save(uniq_combos, file="text_econ_4.rda")  


nov = subset(novels, YEAR >= 86 & YEAR <= 90) 

combos = (combn(unique(nov$TITLE),2))
all_combs = paste(combos[1,],combos[2,])

combos_df = unique(expand.grid(unique(nov$TITLE),unique(nov$TITLE)))
combos_df$a = paste(combos_df$Var1,combos_df$Var2)

uniq_combos = combos_df[(combos_df$a %in% all_combs),]
colnames(uniq_combos) = c('TITLE','TITLE2','Comb')

# store the year range first
uniq_combos$Years = rep('86-90',nrow(uniq_combos))    

# number of connections
res <- foreach(i = 1:nrow(uniq_combos), .combine=rbind)  %dopar% {
  res <- rep(NA, 53)
  res[1] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),2] %in%
                  nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),2])
  res[2] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1])
  res[3] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1])
  
  res[4] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 1
  res[5] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 0
  res[6] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 1
  res[7] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 0
  
  res[8] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 1
  res[9] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 2
  res[10] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 0
  
  res[11] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 1
  res[12] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 2
  res[13] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 0
  
  res[14] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ACAD']")
  res[15] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['BLACK']")
  res[16] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ELITE']")
  res[17] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['GENRE']")
  res[18] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['LIT']")
  res[19] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MAIN']")
  res[20] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MISC']")
  res[21] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['TRADE']")
  res[22] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "[nan]")
  
  res[23] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ACAD']")
  res[24] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['BLACK']")
  res[25] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ELITE']")
  res[26] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['GENRE']")
  res[27] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['LIT']")
  res[28] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MAIN']")
  res[29] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MISC']")
  res[30] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['TRADE']")
  res[31] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "[nan]")
  
  for(j in 0:10){
    res[32 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),11] == j)
    res[43 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),11] == j)
  }
  res
}

names(res) <- c("connections_books", "RaceMatch", "GenderMatch", "Female1", "Male1",
                "Female2", "Male2", "BLACK1", "POC1", "WHITE1", "BLACK2", "POC2", "WHITE2",
                "J2_ACAD_1", "J2_BLACK_1", "J2_ELITE_1", "J2_GENRE_1", "J2_LIT_1", "J2_MAIN_1",
                "J2_MISC_1", "J2_TRADE_1" , "J2_nan_1", "J2_ACAD_2", "J2_BLACK_2", "J2_ELITE_2", 
                "J2_GENRE_2", "J2_LIT_2", "J2_MAIN_2", "J2_MISC_2", "J2_TRADE_2" , "J2_nan_2", "PUB2_0_1", 
                "PUB2_1_1", "PUB2_2_1", "PUB2_3_1", "PUB2_4_1", "PUB2_5_1", "PUB2_6_1", "PUB2_7_1",
                "PUB2_8_1", "PUB2_9_1", "PUB2_10_1", "PUB2_0_2", "PUB2_1_2", "PUB2_2_2", "PUB2_3_2",
                "PUB2_4_2", "PUB2_5_2", "PUB2_6_2", "PUB2_7_2", "PUB2_8_2", "PUB2_9_2", "PUB2_10_2")

uniq_combos <- cbind(uniq_combos, res)

save(uniq_combos, file="text_econ_5.rda")  

  

nov = subset(novels, YEAR >= 91 & YEAR <= 95) 

combos = (combn(unique(nov$TITLE),2))
all_combs = paste(combos[1,],combos[2,])

combos_df = unique(expand.grid(unique(nov$TITLE),unique(nov$TITLE)))
combos_df$a = paste(combos_df$Var1,combos_df$Var2)

uniq_combos = combos_df[(combos_df$a %in% all_combs),]
colnames(uniq_combos) = c('TITLE','TITLE2','Comb')

# store the year range first
uniq_combos$Years = rep('91-95',nrow(uniq_combos))    

# number of connections
res <- foreach(i = 1:nrow(uniq_combos), .combine=rbind)  %dopar% {
  res <- rep(NA, 53)
  res[1] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),2] %in%
                  nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),2])
  res[2] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1])
  res[3] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1])
  
  res[4] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 1
  res[5] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 0
  res[6] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 1
  res[7] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 0
  
  res[8] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 1
  res[9] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 2
  res[10] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 0
  
  res[11] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 1
  res[12] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 2
  res[13] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 0
  
  res[14] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ACAD']")
  res[15] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['BLACK']")
  res[16] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ELITE']")
  res[17] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['GENRE']")
  res[18] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['LIT']")
  res[19] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MAIN']")
  res[20] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MISC']")
  res[21] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['TRADE']")
  res[22] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "[nan]")
  
  res[23] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ACAD']")
  res[24] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['BLACK']")
  res[25] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ELITE']")
  res[26] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['GENRE']")
  res[27] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['LIT']")
  res[28] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MAIN']")
  res[29] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MISC']")
  res[30] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['TRADE']")
  res[31] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "[nan]")
  
  for(j in 0:10){
    res[32 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),11] == j)
    res[43 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),11] == j)
  }
  res
}

names(res) <- c("connections_books", "RaceMatch", "GenderMatch", "Female1", "Male1",
                "Female2", "Male2", "BLACK1", "POC1", "WHITE1", "BLACK2", "POC2", "WHITE2",
                "J2_ACAD_1", "J2_BLACK_1", "J2_ELITE_1", "J2_GENRE_1", "J2_LIT_1", "J2_MAIN_1",
                "J2_MISC_1", "J2_TRADE_1" , "J2_nan_1", "J2_ACAD_2", "J2_BLACK_2", "J2_ELITE_2", 
                "J2_GENRE_2", "J2_LIT_2", "J2_MAIN_2", "J2_MISC_2", "J2_TRADE_2" , "J2_nan_2", "PUB2_0_1", 
                "PUB2_1_1", "PUB2_2_1", "PUB2_3_1", "PUB2_4_1", "PUB2_5_1", "PUB2_6_1", "PUB2_7_1",
                "PUB2_8_1", "PUB2_9_1", "PUB2_10_1", "PUB2_0_2", "PUB2_1_2", "PUB2_2_2", "PUB2_3_2",
                "PUB2_4_2", "PUB2_5_2", "PUB2_6_2", "PUB2_7_2", "PUB2_8_2", "PUB2_9_2", "PUB2_10_2")

uniq_combos <- cbind(uniq_combos, res)

save(uniq_combos, file="text_econ_6.rda")  


nov = subset(novels, YEAR >= 96 & YEAR <= 100) 

combos = (combn(unique(nov$TITLE),2))
all_combs = paste(combos[1,],combos[2,])

combos_df = unique(expand.grid(unique(nov$TITLE),unique(nov$TITLE)))
combos_df$a = paste(combos_df$Var1,combos_df$Var2)

uniq_combos = combos_df[(combos_df$a %in% all_combs),]
colnames(uniq_combos) = c('TITLE','TITLE2','Comb')

# store the year range first
uniq_combos$Years = rep('96-00',nrow(uniq_combos))    

# number of connections
res <- foreach(i = 1:nrow(uniq_combos), .combine=rbind)  %dopar% {
  res <- rep(NA, 53)
  res[1] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),2] %in%
                  nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),2])
  res[2] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1])
  res[3] <- (nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] ==
               nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1])
  
  res[4] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 1
  res[5] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),6][1] == 0
  res[6] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 1
  res[7] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),6][1] == 0
  
  res[8] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 1
  res[9] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 2
  res[10] <- nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),7][1] == 0
  
  res[11] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 1
  res[12] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 2
  res[13] <- nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),7][1] == 0
  
  res[14] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ACAD']")
  res[15] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['BLACK']")
  res[16] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['ELITE']")
  res[17] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['GENRE']")
  res[18] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['LIT']")
  res[19] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MAIN']")
  res[20] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['MISC']")
  res[21] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "['TRADE']")
  res[22] <- sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),10] == "[nan]")
  
  res[23] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ACAD']")
  res[24] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['BLACK']")
  res[25] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['ELITE']")
  res[26] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['GENRE']")
  res[27] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['LIT']")
  res[28] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MAIN']")
  res[29] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['MISC']")
  res[30] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "['TRADE']")
  res[31] <- sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),10] == "[nan]")
  
  for(j in 0:10){
    res[32 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,1] & nov[,9] == 1),11] == j)
    res[43 + j] <-  sum(nov[which(nov[,1]==uniq_combos[i,2] & nov[,9] == 1),11] == j)
  }
  res
}

names(res) <- c("connections_books", "RaceMatch", "GenderMatch", "Female1", "Male1",
                "Female2", "Male2", "BLACK1", "POC1", "WHITE1", "BLACK2", "POC2", "WHITE2",
                "J2_ACAD_1", "J2_BLACK_1", "J2_ELITE_1", "J2_GENRE_1", "J2_LIT_1", "J2_MAIN_1",
                "J2_MISC_1", "J2_TRADE_1" , "J2_nan_1", "J2_ACAD_2", "J2_BLACK_2", "J2_ELITE_2", 
                "J2_GENRE_2", "J2_LIT_2", "J2_MAIN_2", "J2_MISC_2", "J2_TRADE_2" , "J2_nan_2", "PUB2_0_1", 
                "PUB2_1_1", "PUB2_2_1", "PUB2_3_1", "PUB2_4_1", "PUB2_5_1", "PUB2_6_1", "PUB2_7_1",
                "PUB2_8_1", "PUB2_9_1", "PUB2_10_1", "PUB2_0_2", "PUB2_1_2", "PUB2_2_2", "PUB2_3_2",
                "PUB2_4_2", "PUB2_5_2", "PUB2_6_2", "PUB2_7_2", "PUB2_8_2", "PUB2_9_2", "PUB2_10_2")

uniq_combos <- cbind(uniq_combos, res)

save(uniq_combos, file="text_econ_7.rda")  