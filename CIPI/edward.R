# using data.table for the fread() command
# i will also include code that works with only base commands
# just in case
require(data.table)

# change this if you're not me, obviously
setwd("~/.R/CIPI/")

# get the file list(s)
F14.FILES <- list.files(path = "./F14/", pattern = "[0-9]{5}\\.txt", full.names=T)
F15.FILES <- list.files(path = "./F15/", pattern = "[0-9]{6}\\.txt", full.names=T)
FILES <- append(F14.FILES,F15.FILES)

# this is where all the data will eventually go
processeddata <- data.frame(matrix(NA, length(FILES),41))

# this object is of questionable importance
# i may remove it
keylist <- vector(mode="list", length(FILES))

# F14 setup

# in F14, timing depends on condition
conditions <- fread("conditions.csv")
# conditions <- read.csv("conditions.csv")

conditions[!duplicated(conditions),] -> conditions
names(conditions) <- c("ID", "Condition")
conditions <- conditions[order(ID)]
row.names(conditions) <- 1:length(row.names(conditions))

# Timing durations
# 20 time periods
# 19 are 30s long, 1 is 150s
# 1s = 1000 lines
seconds <- c(rep(30,19),150)
duration <- seconds*1000

# Political timings (1 in the conditions file)
seconds <- c(30, 60, 131, 161, 224, 254, 320, 350, 410, 440, 503, 533, 597, 627, 680, 710, 770, 800, 830, 860)
lines <- seconds*1000
start.pol <- lines+1
end.pol <- start.pol+duration

# Apoltical timings (2 in the conditions file)
seconds <-c(30, 60, 123, 153, 217, 247, 300, 330, 390, 420, 491, 521, 584, 614, 680, 710, 770, 800, 830, 860)
lines <- seconds*1000
start.apol <- lines+1
end.apol <- start.apol+duration

# F14 loop

for(i in 1:length(F14.FILES)) {
  # grab the participant ID from the file name
  # and declare the object for all this participant's data
  procdata <- partid <- substr(FILES[i], nchar(FILES[i])-8, nchar(FILES[i])-4)
  
  # for convenience
  condition <- conditions[which(ID==partid)]$Condition
  
  file <- fread(FILES[i])
  # fread adds an extra column
  file$V4 <- NULL
  
  # file <- read.table(FILES[i])
  
  names(file) <- c("PPG", "EDA", "HR")
  
  if (condition==1) {
  # political condition
    
    start <- start.pol
    end <- end.pol
  
  } else if (condition==2) {
  # apolitical condition
    
    start <- start.apol
    end <- end.apol
    
  } else {
  # error
    
    start <- c(rep(0,20))
    end <- c(rep(0,20))
    
  }
  
  # this for loop will go away as soon as i figure out how to replace it
  for (j in 1:length(start)){
    # procdata <- append(procdata, 0) # mean ECG
    procdata <- append(procdata, mean(file$EDA[start[j]:end[j]], na.rm=T)) # mean EDA
    procdata <- append(procdata, mean(file$HR[start[j]:end[j]], na.rm=T)) # mean HR
  }
  
  processeddata[i,] <- procdata
  remove(procdata)
  
}

# F15 loop
for (i in (length(F14.FILES)+1):length(FILES)) {
  
  # pariticpant id from filename
  procdata <- partid <- substr(FILES[i], nchar(FILES[i])-9, nchar(FILES[i])-4)
  
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
  if(length(keypoint)!=14) {
    
    print(paste("ERROR", partid, sep="_"))
    
  } else {
    
    keypoint -> k#eypoint
    60000 -> d#uration
    
    # these are the start and end times for meaningful data
    start <- c(k[1]-d, k[1], k[3]-d, k[3], k[5]-d, k[5], k[6]+d, k[7]-3*d, k[7]-d, k[7], k[9]-d, k[9], k[11]-d, k[11], k[12]+d, k[13]-d*3, k[13]-d, k[13], k[13]+d, k[13]+d*2)
    
    end <- c(k[1], k[2], k[3], k[4], k[5], k[6], k[6]+d*2, k[7]-d*2, k[7], k[8], k[9], k[10], k[11], k[12], k[12]+d*2, k[13]-d*2, k[13], k[13]+ d, k[13]+d*2, k[14])
    
    for (j in 1:length(start)){
      # procdata <- append(procdata, 0) # mean ECG
      procdata <- append(procdata, mean(file$EDA[start[j]:end[j]], na.rm=T)) # mean EDA
      procdata <- append(procdata, mean(file$HR[start[j]:end[j]], na.rm=T)) # mean HR
    }
    
  }
  
  processeddata[i,] <- procdata
  remove(procdata)
  
}

# the data coming out has some quirks I don't understand yet
# this fixes them
unlisteddata <- unlist(processeddata)
unlist2 <- as.numeric(unlisteddata)
pdata2 <- as.data.frame(matrix(data=unlist2, nrow=dim(processeddata)[1], ncol=dim(processeddata)[2]))

datatype <- rep(c("meanEDA", "meanHR"), 20)
stimtype <- c(rep("blank1.1", 2), rep("video1.1", 2), rep("blank1.2", 2), rep("video1.2", 2), rep("blank1.3", 2), rep("video1.3", 2), rep("blank1.4", 2), rep("relax1", 2), rep("blank2.1", 2), rep("video2.1", 2), rep("blank2.2", 2), rep("video2.2", 2), rep("blank2.3", 2), rep("video2.3", 2), rep("blank2.4", 2), rep("relax2", 2), rep("blank3.1", 2), rep("discussion", 2), rep("discussioninfo", 2), rep("discussiontopic", 2))
varname <- paste(datatype, stimtype, sep="_")
names(pdata2) <- c("partid", varname)
