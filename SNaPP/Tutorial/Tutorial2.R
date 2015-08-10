#----------------------------------------------------------------------------------------------------#
# INTRODUCTION TO R Part II
#----------------------------------------------------------------------------------------------------#
#
# College of William & Mary, Department of Government
# INSTRUCTOR: Jaime Settle (and Chris Fariss in the original version of this class)
# Wednesday, February 19th 
# See http://polisci2.ucsd.edu/cfariss/Home/Christopher_J_Fariss.html for additional resources
#
#----------------------------------------------------------------------------------------------------#
# REVIEW FROM LAST WEEK: LOAD DATA INTO AN R SESSION AND TAKE A PEEK
#----------------------------------------------------------------------------------------------------#


# or read it in directly from the internet
macro <- read.csv("http://polisci2.ucsd.edu/cfariss/code/macro.csv", header=TRUE)

# list the variables in mydata
names(macro)

# dimensions of an object
dim(macro) 

# other commands to look at the dimensions of an object
nrow(macro)

ncol(macro)

dim(macro)[1]

dim(macro)[2]

# look at the first 6 rows in the data set
head(macro)

# look at the first 10 rows in the data set
head(macro, n=10)

# look at the last 6 rows in the data set
tail(macro)


# alternative syntax to look at rows in a data set
macro[1:10,]

macro[1:10, 1:2]


# look at the unemployment variable called unem that exists in the macro dataset 
macro$unem


# calculate the mean and standard deviation for each variable in the data set

sum(macro$unem)/length(macro$unem)
sqrt(sum((macro$unem-mean(macro$unem))^2)/(length(macro$unem)-1))

mean(macro$unem)
sd(macro$unem)

summary(macro)

# removing the dataset from your workspace

remove(macro)


#----------------------------------------------------------------------------------------------------#
# CREATING, RECODING AND RENAMING VARIABLES
#----------------------------------------------------------------------------------------------------#

#Loading in the occupational prestige data

prestige <- read.table("http://socserv.mcmaster.ca/jfox/Books/Companion/data/Duncan.txt")

#let's say that you wanted to create a variable that was the sum of education and prestige

#best practice is first to create a new variable that is full of NAs

prestige$sum <- NA
prestige$sum <- prestige$education + prestige$prestige


#then you decide that particular variable is meaningless so you want to remove it. These commands do the same thing

prestige <- prestige[,-5]
prestige <- prestige[,1:4]


###perhaps you want to make a dummy variable for all the cases where prestige is 80 or greater

prestige$high <- NA
prestige$high[which(prestige$prestige >= 80)] <- 1
prestige$high[which(prestige$prestige < 80)] <- 0


#and then in hindsight you decide you want to rename the variable

names(prestige)[5] <- c("magic.number")



#----------------------------------------------------------------------------------------------------#
#  SUBSET A DATAFRAME  
#----------------------------------------------------------------------------------------------------#

# reference the subset command 
?subset

# select the numeric variables (columns) from the full macro data set
macro.variables <- subset(macro, select=c(gdp, unem, capmob, trade))
dim(macro.variables)
summary(macro.variables)

# select only the observations (rows) that occured in the year 1990 from the full data set
macro.1990 <- subset(macro, year==1990)
dim(macro.1990)
summary(macro.1990)

# select the numeric variables (columns) AND the observations (rows) that occured in the year 1990 from the full macro data set
macro.variables.1990 <- subset(macro,  year==1990, select=c(gdp, unem, capmob, trade))
dim(macro.variables.1990)
summary(macro.variables.1990)

                               



#----------------------------------------------------------------------------------------------------#
#  First Pass Analysis
#----------------------------------------------------------------------------------------------------#

# Look at the "Math" section on the reference card. This will give you many of the commands you need for getting a basic overview of your data. You may need to use "na.rm=T" ... look it up with ?mean

min(x)
max(x)
range(x)
mean(x)
median(x)

#We've already talked about looking at the table function.

table(macro$country)

#You can look at a table for only cases meeting specific criteria

table(macro$country[which(macro$year<1980)])

#You can create a table for two ordinal variables. I will first make an ordinal variable

macro$gdppos <- NA
macro$gdppos[which(macro$gdp>=0)] <- 1
macro$gdppos[which(macro$gdp<0)] <- 0

table(macro$country, macro$gdppos)



# calculate correlation coefficients for each pair of variables in the macro.variables dataset

cor(macro)

cor(macro.variables)

cor(macro.variables$unem, macro.variables$trade)

cor.test(macro.variables$unem, macro.variables$trade)



#----------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------#
# COMPLETE DATA EXERCISE 3, PART 1
#----------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------------------------#
# LOOPS
#----------------------------------------------------------------------------------------------------#

for(i in 1:10)
{
	print(paste("i =", i));
}



x <- 10

if(x > 9)

{
	# Execute this only if x > 9
	print("x is larger than 9");
}

###Try setting X to 7 and see what happens in the above example

###You can do a simple test using this format: ifelse(test, yes, no)

x <- 1:10
numbertest <- ifelse(x>5, print("yes"), print("no"))


###You can also separate "if" and "else" for more complicated conditions

x <- 10

if(x > 9) {
	# Execute this only if x > 9
	print("x is larger than 9");
} else if(x > 7) {
	# Execute this only if x > 7 and x <= 9
	print("x is larger than 7, but not larger than 9");
} else {
	# Execute this in all other cases
	print("x is not larger than 7")
}


###You can also use the "while" command: while(cond) expr   ####See the example below. You are telling R that "while a certain condition hold, you should do this expression"

#----------------------------------------------------------------------------------------------------#
# LOOP THROUGH A DATASET AND USE AN IF STATEMENT
#----------------------------------------------------------------------------------------------------#

# add in a new variable that will measure years in which the US was engaged in the Vietnam War
# set all values to 0 and then change values to 1 using the loop and if statements below
macro$us_conflict <- 0

# visually inspect the new variable
macro$us_conflict

# check the data set and new variable before running the loop
head(macro, n=25)
summary(macro, n=25)

i <- 1

while(i <= nrow(macro)){

if(macro$year[i] <=1975){
macro$us_conflict[i] <- 1
}

i <- i + 1
}


# check the data set and new variable after running the loop
head(macro, n=25)
summary(macro, n=25)

# visually inspect the modified variable
macro$us_conflict



#----------------------------------------------------------------------------------------------------#
# MAKING A TABLE TO SUMMARIZE YOUR DATA
#----------------------------------------------------------------------------------------------------#

#The idea here is that you want to make a table summarizing the statistics for your numerical or count variables. 

#Here, you identify all the variables of interest, and then make a table with that many rows, and the number of columns for the stats of interset
vec <- c(3,4,5,6,8)
summarystat <- as.data.frame(matrix(NA, nrow=length(vec),ncol=5))
names(summarystat) <- c("Mean/Prop", "SD", "Min", "Max", "N")
row.names(summarystat) <- names(macro)[vec]
summarystat


#Now, we are going to fill it in with the values for our statistics
for (i in 1:length(vec)){

summarystat[i, 1] <- mean(macro[,vec[i]], na.rm=T)
summarystat[i, 2] <- sd(macro[,vec[i]], na.rm=T)
summarystat[i, 3] <- min(macro[,vec[i]], na.rm=T)
summarystat[i, 4] <- max(macro[,vec[i]], na.rm=T)
summarystat[i, 5] <- length(which(!is.na(macro[,vec[i]])))

}

summarystat

##Now, we'd like to round those values to be more appropriate

sumstat2 <- round(summarystat, digits=3)


###Now, how do we get our nice table out of R?

#The first option is to copy and paste into Excel, and use the "Text to Columns" feature

#The second option is to use a command that formats the table for LaTeX
xtable(sumstat2)



#----------------------------------------------------------------------------------------------------#
# T.Tests and Crosstabs
#----------------------------------------------------------------------------------------------------#


#One of the most commonly used statistical analyses is a t test. A t test allows you to test whether two values are statistically significantly different from one another. #

#There are two main types of t tests:

	#The first is an independent samples t test. This is used if you want to compare the means of the SAME variable but for two different groups. For example, if you wanted to compare GDP in years when the US was engaged in the Vietnam War to years when it was not, you would use this type of test. 

	#The following code will demonstrate this

	#t.test(macro$gdp ~ macro$us_conflict)

		#here, you use the command t.test(DV ~ IV)
		#The IVs here are basically telling R to sort the data based on their value for that variable (war or no war)
		#To use this t test your IV MUST be a dummy variable or have only 2 value options

	#To best answer this question, we need to limit the analysis to the United States only:

t.test(macro$gdp[which(macro$country=="United States")] ~ macro$us_conflict[which(macro$country=="United States")])

	#Analyzing this t test: When you look at the output from this test, you should see values for t, df (degrees of freedom) and p-value. P-value is the most important piece to look at. If the p-value is less than .05, you can reject the null hypothesis that there is no relationship between being at war and GDP and say that there is a statistically significant difference. But, because p>.05, we cannot reject the null hypothesis and we have no evidence to suggest that there is a significant relationship between being involved in the Vietnam War and GDP. 

	#At the bottom of your output, you can see the mean GDPs for the two groups (when the US is engaged in the Vietnam War and not) where it says 'mean group 0' (not) and 'mean group 1' (engaged)

	#The second type of t test allows you to compare the means of two different numeric variables


		t.test(prestige$education[which(prestige$type=="prof")], prestige$prestige[which(prestige$type=="prof")])

	#The output will look almost the same as the first independent samples t-test we did before. You'll see that the p-value here is again >.05, so we can fail to reject the null hypothesis that there is no relationship between the 2 variables.  

	#By looking down at the bottom of your output, you'll see that instead of saying 'mean group 0' and 'mean group 1' it says 'mean of x mean of y.' This is because you're looking at 2 different variables here (x=education and y=prestige). 


#The third type, a one sample t-test, allows you to test whether the value of the mean is different than a hypothesized value. 

		t.test(prestige$education, mu=70)



	
#You should notice that the syntax for these tests are very similar. If you want more information, see ?t.test

##################

#Crosstabs#

##################

###To get you in the practice of seeking out resource online to help you learn more, check out this website, and make some notes about crosstabs. 

#http://www.statmethods.net/stats/frequencies.html


#----------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------#
# COMPLETE DATA EXERCISE 3, PART 2
#----------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------#





#----------------------------------------------------------------------------------------------------#
# ESTIMATE A SIMPLE LINEAR MODEL: BIVARIATE REGRESSION
#----------------------------------------------------------------------------------------------------#

# linear algrebra for the linear model 
# for more information on the syntax for matrix 
#algebra in R see http://www.statmethods.net/advstats/matrix.html

X <- cbind(rep(1, 350), macro$trade)
Y <- macro$unem

model.01 <- solve(t(X)%*%X) %*% (t(X)%*%Y)

model.01

# linear model function lm()
model.01 <- lm(macro$unem ~ macro$trade)

model.01

model.01 <- lm(unem ~ trade, data = macro)

model.01

summary(model.01)

#----------------------------------------------------------------------------------------------------#
# ESTIMATE A SIMPLE LINEAR MODEL: MULTIPLE REGRESSION
#----------------------------------------------------------------------------------------------------#

model.01 <- lm(unem ~ gdp + capmob + trade, data = macro)

model.01

summary(model.01)



#----------------------------------------------------------------------------------------------------#
# ESTIMATE SUBSTANTIVE EFFECTS/QUANTITIES OF INTEREST 
# USING THE MODEL ESTIMATES
#----------------------------------------------------------------------------------------------------#

# generate a sequence from the lowest value of the trade variable to the highest value
e <- seq(from=min(macro$trade), to=max(macro$trade), by=1)

# visually inspect the sequence e
e

# create a new data set that is the length of the sequence created above that contains the variables gdp, unem, capmob, trade 
new.macro <- macro[1:length(e), 3:6]

# loop through the new data set and change the values of the four variables
for(i in 1:length(e)){

# change unem to NA (i.e., missing).  this is the y variable that will be predicted below using the model estimates contained in the model.01 object
new.macro$unem[i] <- NA

# change every value of the variable gdp and capmob to the mean value of these variables from the original data
new.macro$gdp[i] <- mean(macro$gdp)
new.macro$capmob[i] <- mean(macro$capmob)

# change each value of trade to the values in the sequence e
new.macro$trade[i] <- e[i]


}

# see the syntax to predict
?predict

# predict the value unem in the new.macro data object using the coefficients contained in the model.01 object
predict.model.01 <- predict.lm(model.01, new.macro, se.fit=TRUE)

# view the values contained in the new prediction object
predict.model.01

# view the predicted values only
predict.model.01$fit


#----------------------------------------------------------------------------------------------------#
# PLOT THE SUBSTANTIVE EFFECTS/QUANTITIES OF INTEREST
#----------------------------------------------------------------------------------------------------#

plot(e, predict.model.01$fit, type="n", ylim=c(0,max(macro$unem)), ylab="Predicted Value of Unemployment", xlab="Amount of Trade", main="SWANKY TITLE HERE")

lines(e, predict.model.01$fit, lty=1, lwd=3, col=1)
lines(e, predict.model.01$fit-1.96*predict.model.01$se.fit, lty=2, lwd=3, col=2)
lines(e, predict.model.01$fit+1.96*predict.model.01$se.fit, lty=2, lwd=3, col=2)





#----------------------------------------------------------------------------------------------------#
# WHERE CAN I FIND MORE INFO ABOUT RUNNING THE MODELS I WANT?
#----------------------------------------------------------------------------------------------------#

#http://www.statmethods.net/advstats/glm.html
#http://www.statmethods.net/advstats/timeseries.html
#Just google "generalized linear models" or the specific model you are interested in running and "R"

