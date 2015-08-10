#reading in the data
mturk.stress <- read.csv("mturkstress.csv",header=TRUE)

#1-------------------------

treatmenttable <- table(mturk.stress$treatmentgroup)
rownames(treatmenttable) <- c("Control","Stress","Politics","Content")
treatmenttable

#2-------------------------

mean(na.omit(mturk.stress$saisscale))

#3-------------------------

#creating a null variable, running a loop to write in values based on party answers
mturk.stress$partystrength <- NA
i <- 1926
while (i >= 1) {
	if(is.na(mturk.stress[i,23])==TRUE) {
		i <- i-1
	} else if(mturk.stress[i,23]==1) {
		if(is.na(mturk.stress[i,25])==TRUE) {
		} else if(mturk.stress[i,25]==1) {
			mturk.stress[i,137] <- 3
		} else if(mturk.stress[i,25]==2) {
			mturk.stress[i,137] <- 2
		}
		i <- i-1
	} else if(mturk.stress[i,23]==2) {
		if(is.na(mturk.stress[i,25])==TRUE) {
		} else if(mturk.stress[i,25]==1) {
			mturk.stress[i,137] <- 3
		} else if(mturk.stress[i,25]==2) {
			mturk.stress[i,137] <- 2
		}
		i <- i-1
	} else if(mturk.stress[i,23]==3) {
		if(is.na(mturk.stress[i,24])==TRUE) {
		} else if(mturk.stress[i,24]==1) {
			mturk.stress[i,137] <- 1
		} else if(mturk.stress[i,24]==2) {
			mturk.stress[i,137] <- 1
		} else if(mturk.stress[i,24]==3) {
			mturk.stress[i,137] <- 0
		}
		i <- i-1
	}
}
print(i)

partystrength.matrix <- matrix(table(mturk.stress$partystrength),ncol=1,nrow=4)
rownames(partystrength.matrix) <- c("True Independent","Leaning Independent","Weak Partisan", "Strong Partisan")
colnames(partystrength.matrix) <- c("n")

partystrength.matrix

#4-------------------------

#reverse coding polengage2, polengage4, and polengage5 with a while loop#
i <- 1926
while (i >= 1) {
      if(is.na(mturk.stress[i,28])==TRUE) {
	  }
	  else if(mturk.stress[i,28]==5){
	  mturk.stress[i,28] <- 1}
	  else if(mturk.stress[i,28]==4){
	  mturk.stress[i,28] <- 2}
	  else if(mturk.stress[i,28]==2){
	  mturk.stress[i,28] <- 4}
	  else if(mturk.stress[i,28]==1){
	  mturk.stress[i,28] <- 5}
	  
if(is.na(mturk.stress[i,30])==TRUE) {
	  }
	  else if(mturk.stress[i,30]==5){
	  mturk.stress[i,30] <- 1}
	  else if(mturk.stress[i,30]==4){
	  mturk.stress[i,30] <- 2}
	  else if(mturk.stress[i,30]==2){
	  mturk.stress[i,30] <- 4}
	  else if(mturk.stress[i,30]==1){
	  mturk.stress[i,30] <- 5}
	  
if(is.na(mturk.stress[i,31])==TRUE) {
	  }
	  else if(mturk.stress[i,31]==5){
	  mturk.stress[i,31] <- 1}
	  else if(mturk.stress[i,31]==4){
	  mturk.stress[i,31] <- 2}
	  else if(mturk.stress[i,31]==2){
	  mturk.stress[i,31] <- 4}
	  else if(mturk.stress[i,31]==1){
	  mturk.stress[i,31] <- 5}
i <- i-1
}

#checking iteration
print(i)



#summing a vector of the polengage variables, and saving it as polengage
mturk.stress$polengage <- NA
i <- 1926
while (i >=1) {
if(is.na(mturk.stress[i,27])==TRUE){
}
else if(is.na(mturk.stress[i,28])==TRUE){
}
else if(is.na(mturk.stress[i,29])==TRUE){
}
else if(is.na(mturk.stress[i,30])==TRUE){
}
else if(is.na(mturk.stress[i,31])==TRUE){
}
else{
mturk.stress[i,138] <- sum(mturk.stress[i,27:32])
}
i <- i-1
}

#checking iteration completed
print(i)

#5-------------------------

#creating and printing a table for born in the US
borninUS.table <- table(mturk.stress$borninUS)
rownames(borninUS.table) <- c("Yes", "No")
borninUS.table

marital.table <- table(mturk.stress$marital)
rownames(marital.table) <- c("Single","Married","Widowed","Divorced","Other")
marital.table

occupation.table <- table(mturk.stress$occupation)
rownames(occupation.table) <- c("Clerical","Professional","Self-Employed","Corporation","Other White Collar","Service","Custodial/Factory","Construction","Other Blue Collar","Homemaker","Student","Unemployed")
occupation.table

#7-------------------------

#t.test would be good, but the hypotheses are in the doc

#8-------------------------

length(mturk.stress$finished[mturk.stress$finished>0]) / nrow(mturk.stress)

#9-------------------------

length(na.omit(mturk.stress$marital[mturk.stress$marital %in% c(2)])) / nrow(mturk.stress)

length(na.omit(mturk.stress$marital[mturk.stress$marital %in% c(2) & mturk.stress$male %in% c(2)])) / length(na.omit(mturk.stress$male[mturk.stress$male %in% c(2)]))

#10------------------------

prop.test(table(mturk.stress$male))

#11------------------------

#establishing 
mturk.stressdemstrong <- length(mturk.stress$polengage[mturk.stress$part1 %in% c(1) & mturk.stress$part3 %in% c(1)]) / nrow(mturk.stress)
mturk.stressdemweak <- length(mturk.stress$polengage[mturk.stress$part1 %in% c(1) & mturk.stress$part3 %in% c(2)]) / nrow(mturk.stress)
mturk.stressdemlean <- length(mturk.stress$polengage[mturk.stress$part1 %in% c(3) & mturk.stress$part2 %in% c(1)]) / nrow(mturk.stress)


mturk.stressdem.matrix <- matrix(c(mturk.stressdemstrong,mturk.stressdemweak,mturk.stressdemlean),ncol=1,nrow=3)
anesdem.matrix <- matrix(c(0.19,0.15,0.17),ncol=1,nrow=3)

demtestmatrix <- cbind(mturk.stressdem.matrix,anesdem.matrix)

prop.test(demtestmatrix)

#12------------------------

#running a t-test to rule out the null hypothesis
t.test(mturk.stress$votelikely~mturk.stress$pol)

#13------------------------

#creating proportion variables
barack <- length(mturk.stress$votechoice[mturk.stress$votechoice %in% c(1)])/ nrow(mturk.stress) * 100
mitt <- length(mturk.stress$votechoice[mturk.stress$votechoice %in% c(2)]) / nrow(mturk.stress) * 100
other <- length(mturk.stress$votechoice[mturk.stress$votechoice %in% c(3)]) / nrow(mturk.stress) * 100

#creating a matrix of the variables
votechoice <- matrix(c(barack, mitt, other),nrow=1,ncol=3)

#plotting the matrix
barplot(votechoice,names.arg=c("Obama","Romney","Other"),beside=TRUE,col=c("dodgerblue4", "firebrick1", "darkgoldenrod4"), ylim=c(0,75),xlab="Candidates",font.lab="2",ylab="Proportion of Respondents",main="Vote Choice Proportions in the MTurk Dataset")
box()

#14------------------------

#creating a histogram of age
hist(mturk.stress$age,xlab="Age",font.lab="2",main="Histogram of MTurk Participant Age",col=c("chartreuse4"))
box()

#15-------------------------

#creating plot of age v response time and regression line
plot(na.omit(mturk.stress$age[mturk.stress$responselength>0]),na.omit(mturk.stress$responselength[mturk.stress$responselength>0]),xlab="Age",ylab="Response Length",yaxt="n",ylim=c(0,400),main="Correlation Between Respondent Age and Response Length")
abline(lm(na.omit(mturk.stress$responselength[mturk.stress$responselength>0])~na.omit(mturk.stress$age[mturk.stress$responselength>0])),col="red")

#running correlation test
cor.test(na.omit(mturk.stress$age[mturk.stress$responselength>0]),na.omit(mturk.stress$responselength[mturk.stress$responselength>0]))
