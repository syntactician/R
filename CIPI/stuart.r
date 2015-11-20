# using data.table for the fread() command
# i will also include code that works with only base commands
# just in case
require(data.table)

# change this if you're not me, obviously
setwd("~/.R/CIPI/")

# get the file list(s)
FILES <- list.files(path = "./Stuart/", pattern = "[0-9]{5}\\.txt", full.names=T)

processeddata <- data.frame(matrix(NA,length(FILES),2))

# loop
for (i in (length(F14.FILES)+1):length(FILES)) {
  
  # pariticpant id from filename
  procdata <- partid <- substr(FILES[i], nchar(FILES[i])-8, nchar(FILES[i])-4)
  
  file <- fread(FILES[i])
  # fread adds a column
  file$V5 <- NULL
  
  # file <- read.table(FILES[i])
  
  names(file) <- c("ECG", "EDA", "Indicator", "HR")
  
  # finding key points based on Indicator
  # only first instances in bursts of 5 matter
  indvector <- as.numeric(rownames(file)[which(file$Indicator==5)])
  indtest <- append(0,indvector)
  indvector <- append(indvector, indvector[length(indvector)]+1)
  keyvec <- ifelse(indvector==(indtest+1),0,1)
  keylist[[i]] <- keypoint <- indvector[keyvec==1]
  
  # if there are not 14, the file is bad somehow
  # if(length(keypoint)!=14) {
    
  #   print(paste("ERROR", partid, sep="_"))
    
  # } else {
    
  #   keypoint -> k#eypoint
  #   60000 -> d#uration
    
  #   # these are the start and end times for meaningful data
  #   start <- c(k[1]-d, k[1], k[3]-d, k[3], k[5]-d, k[5], k[6]+d, k[7]-3*d, k[7]-d, k[7], k[9]-d, k[9], k[11]-d, k[11], k[12]+d, k[13]-d*3, k[13]-d, k[13], k[13]+d, k[13]+d*2)
    
  #   end <- c(k[1], k[2], k[3], k[4], k[5], k[6], k[6]+d*2, k[7]-d*2, k[7], k[8], k[9], k[10], k[11], k[12], k[12]+d*2, k[13]-d*2, k[13], k[13]+ d, k[13]+d*2, k[14])
    
  #   for (j in 1:length(start)){
  #     # procdata <- append(procdata, 0) # mean ECG
  #     procdata <- append(procdata, mean(file$EDA[start[j]:end[j]], na.rm=T)) # mean EDA
  #     procdata <- append(procdata, mean(file$HR[start[j]:end[j]], na.rm=T)) # mean HR
  #   }
    
  # }
  
  procdata <- append(procdata,length(keypoint))
  
  print(partid)
  print(length(keypoint))
  
  processeddata[i,] <- procdata
  remove(procdata)
  
}

# the data coming out has some quirks I don't understand yet
# this fixes them
unlisteddata <- unlist(processeddata)
unlist2 <- as.numeric(unlisteddata)
# pdata2 <- as.data.frame(matrix(data=unlist2, nrow=dim(processeddata)[1], ncol=dim(processeddata)[2]))

# datatype <- rep(c("meanEDA", "meanHR"), 20)
# stimtype <- c(rep("blank1.1", 2), rep("video1.1", 2), rep("blank1.2", 2), rep("video1.2", 2), rep("blank1.3", 2), rep("video1.3", 2), rep("blank1.4", 2), rep("relax1", 2), rep("blank2.1", 2), rep("video2.1", 2), rep("blank2.2", 2), rep("video2.2", 2), rep("blank2.3", 2), rep("video2.3", 2), rep("blank2.4", 2), rep("relax2", 2), rep("blank3.1", 2), rep("discussion", 2), rep("discussioninfo", 2), rep("discussiontopic", 2))
# varname <- paste(datatype, stimtype, sep="_")
# names(pdata2) <- c("partid", varname)
