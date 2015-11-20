total <- proc.time()

###############################################################
###############################################################
#######--Calculating the Physiological Response Data--#########
###############################################################
###############################################################

#---------------Load in the File with the Self-Report Data from the Lab Session-------------#
#As far as I can tell, there is no need at the get go to match in any of the survey data




# ----- load data ---------- #

# setwd("~/Box Sync/Research/Active Projects/Contentious Social Interaction Projects/NSF Funded Projects - 2014-2015/Omnibus Data Fall 2014/F15 Data Collection/Subjects Data/StimTracker Timing 2")
setwd("./F15/")

# ----- define objects ---------- #
# FILES <- list.files() # files names should as participant ID number
FILES <- list.files(pattern = "[0-9]{5}\\.txt")
keyvec <- 0 #this is creating a placeholder for vector we will append for the key time periods
keylist <- vector(mode="list", length(FILES))
processeddata <- as.data.frame(matrix(NA, length(FILES) , 61)) ##making a placeholder to receive the processed data


for (i in 1:length(FILES)){ #this is setting up the initial loop to read in and process the files
  ptm <- proc.time()
  timingfile <- read.delim(file=FILES[i], header=F) #this reads in the ith file
  print(proc.time() - ptm)
procdata <- partid <- substr(FILES[i],4,9) #pulling out the participant number
indvector <- as.numeric(rownames(timingfile)[which(timingfile$V3==5)]) #this identifies the row numbers when the indicator variable is set to 5
indvector <- append(0, indvector) #this creates a dummy first value for the loop below

for (k in 2:length(indvector)){ #this sets up the loop to identify the key times for this particular subject from which we will pull the relevant data

indtest <- indvector[k-1] +1 #this sets up the comparison value for the ifelse test below
ifelse(indvector[k]==indtest, keyvec <- append(keyvec, 0), keyvec <- append(keyvec, 1)) #this is the test to see if this is the first appearance in the chain of 5's or not

}

keylist[[i]] <- keypoint <- indvector[keyvec==1] #this fills the ith list space in keylist with the key time points for each subject
keyvec <- 0 #this resets the keyvec to be filled
print(i) #this iterates so you can see how the code is looping

#------------------Dealing with the subjects who do not have 14 segments------------------------------------#

######THIS CODE IS  NOT CORRECT BUT COULD BE MODIFIED
#if(length(keylist[[i]])!=14) {

#print(paste("ERROR_", partid, sep=""))
#next
#}

#if(length(keylist[[i]]==14)) {


#------------------Grabbing the Portions of Data you Want------------------------------------#
#------------------Setting the parameters for how you want to calculate the data. Here, we have start and end times for each segment of our experiment------------------------------------#
keypoint <- keylist[[i]]

#startingtime.vector <- c(keylist[[i]][1]-60000, keylist[[i]][1], keylist[[i]][3]-60000, keylist[[i]][3], keylist[[i]][5]-60000, keylist[[i]][5], keylist[[i]][7]-60000, keylist[[i]][7], keylist[[i]][9]-60000, keylist[[i]][9], keylist[[i]][11]-60000, keylist[[i]][11],  keylist[[i]][13]-60000, keylist[[i]][13], keylist[[i]][13]+ 60000, keylist[[i]][13]+ 120000, keylist[[i]][13]+ 300000)

#I am not getting the "blank screen" starting point AFTER the third video quite right, but I don't think we use this data for anything. 
startingtime.vector <- c(keypoint[1]-60000, keypoint[1], keypoint[3]-60000, keypoint[3], keypoint[5]-60000, keypoint[5], keypoint[6] + 60000, keypoint[7]-180000, keypoint[7]-60000, keypoint[7], keypoint[9]-60000, keypoint[9], keypoint[11]-60000, keypoint[11], keypoint[12] + 60000, keypoint[13]-180000, keypoint[13]-60000, keypoint[13], keypoint[13]+ 60000, keypoint[13]+ 120000)

endingtime.vector <- c(keypoint[1], keypoint[2], keypoint[3], keypoint[4], keypoint[5], keypoint[6], keypoint[6] + 120000, keypoint[7]-120000, keypoint[7], keypoint[8], keypoint[9], keypoint[10], keypoint[11], keypoint[12], keypoint[12]+120000, keypoint[13]-120000, keypoint[13], keypoint[13]+ 60000, keypoint[13]+ 120000, keypoint[14])


##If you wanted to make alterations here--like only taking the first 10 seconds of each stimulus, or waiting five seconds after each stimulus event to begin gathering data, you could do arithmetic transformations to these vectors. 


# ----- outerloop k: process one rawtext file per iteration ---------- #

names(timingfile) <- c("ECG", "EDA", "Indicator", "HR") #naming the files
#cond <- conditions$condition.pol[which(partid==conditions$id)] #identifying which of the two conditions the participant was in

    ##we want to loop through 20 segments of the experiment taking the mean for each of three variables
    
        
for (j in 1:length(startingtime.vector)){
      procdata <- append(procdata, mean(timingfile$ECG[startingtime.vector[j]:endingtime.vector[j]], na.rm=T)) ##mean PPG 
      procdata <- append(procdata, mean(timingfile$EDA[startingtime.vector[j]:endingtime.vector[j]], na.rm=T)) ##mean EDA 
      procdata <- append(procdata, mean(timingfile$HR[startingtime.vector[j]:endingtime.vector[j]], na.rm=T)) ##mean HR 
    }

#print(procdata) #only put this in if you want to watch it
processeddata[i,] <- procdata
remove(procdata)

 

print(partid) #this prints each participant ID so you watch how it is processing

}

#-----------------This is to make the data the right kind of object----------------#
#I think append has the effect of making this data a list object, which is a pain
unlisteddata <- unlist(processeddata)
unlist2 <- as.numeric(unlisteddata)
pdata2 <- as.data.frame(matrix(data=unlist2, nrow=dim(processeddata)[1], ncol=dim(processeddata)[2]))

#this is some code to generate the names of the aggregated variables we just created
datatype <- rep(c("meanECG", "meanEDA", "meanHR"), 20)
stimtype <- c(rep("blank1.1", 3), rep("video1.1", 3), rep("blank1.2", 3), rep("video1.2", 3), rep("blank1.3", 3), rep("video1.3", 3), rep("blank1.4", 3), rep("relax1", 3), rep("blank2.1", 3), rep("video2.1", 3), rep("blank2.2", 3), rep("video2.2", 3), rep("blank2.3", 3), rep("video2.3", 3), rep("blank2.4", 3), rep("relax2", 3), rep("blank3.1", 3), rep("discussion", 3), rep("discussioninfo", 3), rep("discussiontopic", 3))
varname <- paste(datatype, stimtype, sep="")
names(pdata2) <- c("partid", varname)

septdata <- processeddata <- pdata2

# save(septdata, file="/Users/jaimesettle/Box Sync/Research/Active Projects/Contentious Social Interaction Projects/NSF Funded Projects - 2014-2015/Omnibus Data Fall 2014/F15 Data Collection/Subjects Data/septdata.rda")


#-----------------Checking out ----------------#
print(proc.time() - total)