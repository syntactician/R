

###############################################################
###############################################################
#######--Calculating the Physiological Response Data--#########
###############################################################
###############################################################

#---------------Load in the File with the Self-Report Data from the Lab Session-------------#
load("/Users/jaimesettle/Box Sync/Research/Active Projects/Contentious Social Interaction Projects/NSF Funded Projects - 2014-2015/Omnibus Data Fall 2014/Data Analysis/lab.rda")

 #-----------------Idiosyncratic Fixes for this particular data -- ONLY DO THE FIRST TIME ---------------#
#It appears that one of the IDs got entered incorrectly into Qualtrics. So, I am fixing it by hand. However, in the future, I've included a line of code in the loop that will let you know if something goes wrong.

lab$id[81] <- 69256
save(lab, file="/Users/jaimesettle/Box Sync/Research/Active Projects/Contentious Social Interaction Projects/NSF Funded Projects - 2014-2015/Omnibus Data Fall 2014/Data Analysis/lab.rda")

######Fixing the f14 file
f14[124,144:286] <- f14[125, 144:286]
f14[-which(f14$ID==69259),] -> f14

#Grabbing the relevant columns for which experimental conditions the subject was in, and removing the lab data file
conditions <- lab[,c(3,4,5,7)]
remove(lab)

# ----- load data ---------- #

setwd("/Users/jaimesettle/Box Sync/Research/Active Projects/Contentious Social Interaction Projects/NSF Funded Projects - 2014-2015/Omnibus Data Fall 2014/Psychophys Analysis/Cleaned CSV Graphs")

# ----- define objects ---------- #
FILES <- list.files() # files names should as participant ID number

processeddata <- as.data.frame(matrix(NA, length(FILES) , 61)) ##making a placeholder to receive the processed data

#------------------Setting the parameters for how you want to calculate the data. Here, we have start and end times for each segment of our experiment------------------------------------#

###Political Videos First Condition
#start <-c(60, 101, 161, 194, 254, 290, 350, 380, 440, 473, 533, 567, 627, 650, 710, 740, 800, 830, 860)
#Note that here, I am making the decision to only grab the last 30 seconds of each ISI. If you want to change that, you would need to adjust the start times  
start <- c(30, 60, 131, 161, 224, 254, 320, 350, 410, 440, 503, 533, 597, 627, 680, 710, 770, 800, 830, 860)
start2 <- start*50
start3 <- start2+1
startingtime.pol.vector <- c(start3)
endingtime.pol.vector <- c(start2[2:20])

##If you wanted to make alterations here--like only taking the first 10 seconds of each stimulus, or waiting five seconds after each stimulus event to begin gathering data, you could do arithmetic transformations to these vectors. 

###Apoltical Videos First Condition
#Note that here, I am making the decision to only grab the last 30 seconds of each ISI. If you want to change that, you would need to adjust the start times 
#start <-c(60, 93, 153, 187, 247, 270, 330, 360, 420, 461, 521, 554, 614, 650, 710, 740, 800, 830, 860)
start <-c(30, 60, 123, 153, 217, 247, 300, 330, 390, 420, 491, 521, 584, 614, 680, 710, 770, 800, 830, 860)
start2 <- start*50
start3 <- start2+1
startingtime.apol.vector <- c(start3)
endingtime.apol.vector <- c(start2[2:20])

##If you wanted to make alterations here--like only taking the first 10 seconds of each stimulus, or waiting five seconds after each stimulus event to begin gathering data, you could do arithmetic transformations to these vectors. 

# ----- outerloop k: process one rawtext file per iteration ---------- #
for(k in 1:length(FILES)){
    
timingfile <- read.csv(file=FILES[k]) #Reading in one file at a time from the folder of invididual level data
names(timingfile) <- c("time", "PPG", "EDA", "HR") #naming the files
partid <- substr(FILES[k],1,5) #pulling out the participant number
cond <- conditions$condition.pol[which(partid==conditions$id)] #identifying which of the two conditions the participant was in

 #----------------writing a quick check to see if there were any problems with ID numbers---------------------#
##The idea here is if the cond object was not created in the line above this, it will print an error. cond wouldn't be created if there is no match for the participant IDs. 
if(length(cond)<1) {

print(paste("ERROR_", partid, sep=""))
next
}

if(cond==1)

    ##we want to loop through 20 segments of the experiment taking the mean for each of three variables
    
    {

        procdata <- partid ##partID
        endingtime.pol.vector2 <- c(endingtime.pol.vector, dim(timingfile)[1]) #this is because each file is slightly different in length
        
for (i in 1:length(startingtime.pol.vector)){
      procdata <- append(procdata, mean(timingfile$PPG[startingtime.pol.vector[i]:endingtime.pol.vector2[i]], na.rm=T)) ##mean PPG 
      procdata <- append(procdata, mean(timingfile$EDA[startingtime.pol.vector[i]:endingtime.pol.vector2[i]], na.rm=T)) ##mean EDA 
      procdata <- append(procdata, mean(timingfile$HR[startingtime.pol.vector[i]:endingtime.pol.vector2[i]], na.rm=T)) ##mean HR 
    }

#print(procdata) #only put this in if you want to watch it
processeddata[k,] <- procdata
remove(procdata)
}
 
else( {


    procdata <- partid ##partID
    endingtime.apol.vector2 <- c(endingtime.apol.vector, dim(timingfile)[1])  #this is because each file is slightly different in length
        
for (i in 1:length(startingtime.apol.vector)){
      procdata <- append(procdata, mean(timingfile$PPG[startingtime.apol.vector[i]:endingtime.apol.vector2[i]], na.rm=T)) ##mean PPG 
      procdata <- append(procdata, mean(timingfile$EDA[startingtime.apol.vector[i]:endingtime.apol.vector2[i]], na.rm=T)) ##mean EDA 
      procdata <- append(procdata, mean(timingfile$HR[startingtime.apol.vector[i]:endingtime.apol.vector2[i]], na.rm=T)) ##mean HR 
    }


#print(procdata) #only put this in if you want to watch it
processeddata[k,] <- procdata
remove(procdata)
} )

print(partid) #this prints each participant ID so you watch how it is processing

}

#-----------------This is to make the data the right kind of object----------------#
#I think append has the effect of making this data a list object, which is a pain
unlisteddata <- unlist(processeddata)
unlist2 <- as.numeric(unlisteddata)
pdata2 <- as.data.frame(matrix(data=unlist2, nrow=dim(processeddata)[1], ncol=dim(processeddata)[2]))

#this is some code to generate the names of the aggregated variables we just created
datatype <- rep(c("meanPPG", "meanEDA", "meanHR"), 20)
stimtype <- c(rep("blank1.1", 3), rep("video1.1", 3), rep("blank1.2", 3), rep("video1.2", 3), rep("blank1.3", 3), rep("video1.3", 3), rep("blank1.4", 3), rep("relax1", 3), rep("blank2.1", 3), rep("video2.1", 3), rep("blank2.2", 3), rep("video2.2", 3), rep("blank2.3", 3), rep("video2.3", 3), rep("blank2.4", 3), rep("relax2", 3), rep("blank3.1", 3), rep("discussion", 3), rep("discussioninfo", 3), rep("discussiontopic", 3))
varname <- paste(datatype, stimtype, sep="")
names(pdata2) <- c("partid", varname)

processeddata <- pdata2

save(processeddata, file="/Users/jaimesettle/Box Sync/Research/Active Projects/Contentious Social Interaction Projects/NSF Funded Projects - 2014-2015/Omnibus Data Fall 2014/Psychophys Analysis/Phys Data/processeddata.rda")


#-----------------Checking out ----------------#
