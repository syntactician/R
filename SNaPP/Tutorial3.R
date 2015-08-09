#----------------------------------------------------------------------------------------------------#
# INTRODUCTION TO R Part III
#----------------------------------------------------------------------------------------------------#
#
# College of William & Mary, Department of Government
# INSTRUCTOR: Jaime Settle (and Chris Fariss in the original version of this class)
# Wednesday, February 26th 
#


#----------------------------------------------------------------------------------------------------#
# Reading in the datasets
#----------------------------------------------------------------------------------------------------#
macro <- read.csv("http://polisci2.ucsd.edu/cfariss/code/macro.csv", header=TRUE)
prestige<-read.table("http://socserv.mcmaster.ca/jfox/Books/Companion/data/Prestige.txt")




##For the purposes of the t-tests described below, run the following code. We will explain what it does later in the lesson. 


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


















































######Cross Tab Code##############










#----------------------------------------------------------------------------------------------------#
# LOOPS
#----------------------------------------------------------------------------------------------------#




###
##Explanation: a loop is a sequence of instructions that is continually repeated until a certain condition is met. Normally, some process is done--generating or changing data, calculating a statistic, or matching rows of datasets together, for example--and after each iteration, a condition is checked so the program knows to keep going with another iteration through the loop, or whether to stop and move on to the next part of the code. 


##Understanding what a loop does is really fundamental to basic computer programming, and once you get the hang of it, using loops can save you a lot of time. 


##It is a good practice to use the print() function when you are running a loop so that R will print to the screen something each time the loop is run. While this is unnecessary with simple small loops, if you are doing something really complicated, it is helpful. If you want to string a phrase together to show up on screen, use the paste() command


#Let's walk through a really straightforward example. We want to know the square root of the numbers 1 through 10. We are going to tell R to iterate through this calculation by setting an index ("i") where each trip through the loop, "i" will take on a different value, moving from the value of 1 through the value of 10. This is the code using the for() command before the squiggly bracket {


for(i in 1:10)
{
        print(paste("i =", i))
        print(paste("square root of i =", sqrt(i)))
}


#Now let's focus on what is going on inside the squiggly brackets. First, we are telling R to print "i =    " each time it runs through the loop, so that we know what i equals in that loop. Then we are telling R to print the square root of "i."  i is a different sequential value between 1 and 10 on each subsequent iteration of the loop.


###Another way to do this is with the ifelse() command. This tells R what test to conduct, what to do if the test is passed, and what to do if the test is failed. 


x <- 1:10
numbertest <- ifelse(x>5, print("yes"), print("no"))
numbertest


###You can tell R to do a variety of different things, not just printing words. You could have it recode a variable, or only match data if certain conditions are met. 




## A third way to do this is the while() command. Here, you are telling R what to do "while" certain conditions apply. As soon as those conditions no longer apply, the loop stops running. For example, in the loop below, we want to create a new variable to capture the years in which the US was engaged in the Vietnam War. In this case it would be possible to do this in other ways without a loop, but sometimes, the only efficient way is to loop. 




                # set all values to 0 and then change values to 1 using the loop and if statements below
                macro$us_conflict <- 0


                # visually inspect the new variable
                macro$us_conflict


                # check the data set and new variable before running the loop
                head(macro, n=25)
                summary(macro, n=25)


# in this loop, we first set our indicator variable i to i. Then we set up the "while conditions" to specify that we only want R to run this as long as the indicator variable is less than or equal to the number of rows in the dataset. Then, within the first set of brackets, we use the if command to tell R that if the i-th value of the year variable is less than or equal to 1975, the conflict variable should be replaced with a 1. If that condition is not met, nothing happens inside that loop. R then moves on to the next line in the code, where it adds a 1 to the value of i. It then proceeds through another loop, as long as i still meets the condition. 


                i <- 1


                while(i <= nrow(macro)){


                if(macro$year[i] <=1975){
                macro$us_conflict[i] <- 1
                }


                i <- i + 1
                }








#----------------------------------------------------------------------------------------------------#
# MAKING A TABLE TO SUMMARIZE YOUR DATA
#----------------------------------------------------------------------------------------------------#


#The idea here is that you want to make a table summarizing the statistics for your numerical or count variables. Instead of calculating these all by hand, you are going to run a loop to run a set of statistics over each of the variables of interest. 


#Here, you identify all the variables of interest, and then make a table with that many rows, and the number of columns for the stats of interest in your dataset. In this case, we are interested in four variables: columns 3-6 in our dataset, or gdp, unemployment, capital mobility, and trade. 


        #First, we set up a placeholder matrix full of NAs to receive the statistics we calculate
        summarystat <- as.data.frame(matrix(NA, nrow=4,ncol=5))
        
        #Next we name our columns with the statistics we want to calculate
        names(summarystat) <- c("Mean/Prop", "SD", "Min", "Max", "N")


        #Then, we name the rows in our placeholder matrix with the names of the variables from the macro dataset
        row.names(summarystat) <- names(macro)[3:6]


        #Let’s look at our placeholder table. 
        summarystat




#Now, we are going to fill it in with the values for our statistics. Let’s walk through how we would do this if we weren’t using a loop. The first variable we care about is gdp. we want to fill the first row of our placeholder table with the statistics for that variable


summarystat[1, 1] <- mean(macro$gdp, na.rm=T)
summarystat[1, 2] <- sd(macro$gdp, na.rm=T)
summarystat[1, 3] <- min(macro$gdp ,na.rm=T)
summarystat[1, 4] <- max(macro$gdp, na.rm=T)
summarystat[1, 5] <- length(which(!is.na(macro$gdp)))


#We could do this exact same thing, but this time instead of calling the variable by its name, we could call it by its column number




summarystat[1, 1] <- mean(macro[,3], na.rm=T)
summarystat[1, 2] <- sd(macro[,3], na.rm=T)
summarystat[1, 3] <- min(macro[,3], na.rm=T)
summarystat[1, 4] <- max(macro[,3], na.rm=T)
summarystat[1, 5] <- length(which(!is.na(macro[,3])))


###Ok. Now, you could cut and paste this code for each of your five variables, changing the row number in summarystat into which you are assigning the values, and changing the column number of your dataset that you want to analyze. Or, you could write a loop! So, what is ripe for looping in this data? You want to loop through the variables you analyze, and assign each statistics to a different row in the data. 


##Each iteration of the loop will be performed on a different variable, but we want the same statistics calculated each time, the mean, standard deviation, minimum value, maximum value, and the total number of complete cases for that variable. In the code, you want to replace every thing that will vary between loops (the column number in the dataset and the row number in the placeholder table) with the index “i” so that it will change with each iteration of the loop. You are setting i to loop through columns 3 through 6 in your dataset, so your initial line creates a for loop with i looping through the values of 3,4, 5, 6. You then need to assign those to the proper rows in your placeholder table. On the first loop, “i” will take on a value of 3, but you want to fill in the first row. So, if you fill in “i-2” you will get the value of 1, and the first row will be filled in. This shoudl work on every iteration. Walk through it in your head. 




for (i in 3:6){


summarystat[i-2, 1] <- mean(macro[,i], na.rm=T)
summarystat[i-2, 2] <- sd(macro[,i], na.rm=T)
summarystat[i-2, 3] <- min(macro[,i], na.rm=T)
summarystat[i-2, 4] <- max(macro[,i], na.rm=T)
summarystat[i-2, 5] <- length(which(!is.na(macro[,i])))


}


summarystat


##Now, we'd like to round those values to be more appropriate


sumstat2 <- round(summarystat, digits=3)
sumstat2


###Now, how do we get our nice table out of R?


#The first option is to copy and paste into Excel, and use the "Text to Columns" feature


#The second option is to use a command that formats the table for LaTeX
library(xtable)
xtable(sumstat2)




###EXTENSION. What happens if you want to summarize variables that are not in a nice clean order in your dataset? In this case, you first generate a vector of the variable numbers of interest. Then, you set i to loop through the positions in that vector. Compare the code below to the code for our loop above. It is very similar in many ways. The key difference is that you are using two different indices. “i” is looping through the vector called “vec” in order to know which column numbers to use. In the code within the loop, we are still calling column numbers in macro, but this time we are calling the “i-th” element in the vector “vec” so that we can call non-sequential columns. Defining “vec” is also useful for is determining the number of rows we want to make in our summarystat placeholding table. 


vec <- c(2,4,6)
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








#----------------------------------------------------------------------------------------------------#
# PLOTTING
#----------------------------------------------------------------------------------------------------#


###One of the major advantages of using R is the incredible flexibility it gives you for the visualization of your data. I am only showing you the very tip of the iceberg: ways to show visualizations of the descriptive statistics you would run on your data. Again, I am not going to go through when it is apporpriate to visualize your data in each of these ways—that is something you will or have learned in GOVT 301. 


####################
#####Histogram######
####################


###here, I am generating a vector of data that might look like the distribution of midterm exam scores.
scores <- rnorm(70, mean=84, sd=4)


###Here, I am assigning my variable of interest to x. This step is not strictly necessary, but doing this means that use can use this code as a template, and just assign whatever variable you want to be x. 
x <- scores


####the main command you are using is hist(). All of the other parameters you are setting are additional. type in ?hist to get a sense of the different options
h<-hist(x, col="red", xlab="Grade", ylab="Count", main="Histogram with Normal Curve", axes=T) 


##If you want to add a line to show what a normal distribution would look like, do that like this
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)




####################
#####Bar Graph ######
####################


##Sometimes, the best way to show an effect is simply to plot the difference in proportions, or the means, between two groups. In that case, a bar graph might be the most straightforward way to show your results. Meg provided the example below. She ran an experiment where she foudn that in one group, 27% of people showed up to volunteer and in another group, only 3% did. She had separately calculated the 95% confidence interval of the difference in proportions and found it to be .018 to .443


# First, make a placeholder table for your variables of interest
logindiff <- matrix(NA,5,1)


#Next, fill in the placeholder with your values
logindiff[1,1] <- .27027027
logindiff[2,1] <- .03225806
logindiff[3,1] <- logindiff[1,1] - logindiff[2,1]
logindiff[4,1] <- 0.4434611
logindiff[5,1] <- 0.01833858


#Assign the object COLOR with the colors that you want to use. See the RColor PDF for options. 
COLOR <- c("lightblue", "cornflowerblue")


##Now, convert the proportions into a percent, and plot the first two rows. See ?barplot for the options
difference <- logindiff[3,1] * 100
barplot(logindiff[c(2,1),], beside=TRUE, col=COLOR, font=2, font.lab=2, ylim=c(-.1,.5), ylab='Percent volunteering', yaxt='n',)


###Now, add in details on the axises. 
axis(side=2, at=seq(from=-.1, to=.5, by = .1), label=c("-10", "0", "10","20", "30", "40", "50"), las=2, cex=.5)
axis(side=1, at=c(.7, 1.9), label=c("Campaign Recruitment", "Social Recruitment"), las=1, cex=.5, font = 2, line=1, lwd=0)
axis(side=2, at= -0.25, label=c("Number of Volunteers"), las=0, cex=.7, line=3, font=2, tick=F)
mtext(c("Difference in Average Volunteer Turnout - Field Experiment"), las=0, cex=1.1, line=1, font=2, side=3, at=1.3)


#Now add some text inside the plot window
text(1.3, .4, paste(round((logindiff[3,1] * 100), 2), "%", sep="") , col="black", cex=1.5)
text(1.3, .35, paste("(", round((logindiff[5,1]*100), 2), "%", " - ", round((logindiff[4,1]*100), 2), "%", ")", sep=""), col="black", cex=1)


# Finally, putting in Confidence intervals
segments(0,0,2,0)
segments(.7, -0.03362169, .7, .09813782)
segments(1.9, 0.1164764, 1.9,  0.4098394)
segments(.6,  -0.03362169, .8, -0.03362169)
segments(.6,  .09813782, .8, .09813782)
segments(1.8, 0.1164764, 2.0, 0.1164764)
segments(1.8, 0.4098394, 2.0, 0.4098394)




###EXTENSION. When you run a t.test, if you save the output to an object, you can then directly call elements of that object to plot


biebs <- t.test(classdata$bieber ~ classdata$bieber_deport==1)
names(biebs)
biebs$conf.int
biebs[4]
biebs[4]$conf.int[1]


#######################
#####Correlation Plot ######
#######################


par(mar=c(4,4,5,.5), cex=.75, font=2, font.lab=2)
plot(classdata$caf_food, classdata$sadler, ylim=c(0, 100), xlim=c(0,100),  yaxt="n", xaxt="n",
     main="", 
     ylab="",
     xlab="", col=c("black"), pch=19, cex=.5)
mtext("W & M Feeling Thermometer Scores\nSadler vs. Caf Food", side=3, at=55, las=0, cex=1.5, line=1, font=2)
  axis(side=2, at=seq(0, 100,10), label=c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100"), las=2, cex.axis=1.1)
axis(side=2, at=50, label=c("Sadler Rating"), las=0, cex.axis=1.2, line=1.5, font=2, tick=F)
axis(side=1, at=c(seq(0,100, by=10)), label=c(seq(0,100, by=10)), las=0, line= 0, font=1, cex.axis=1.1, tick=T)
axis(side=1, at=55, label=c("Caf Food Rating"), las=0, cex.axis=1.2, line=1.5, font=2, tick=F)


gendernormative <- ifelse(classdata$gender==2, "blue", "red")


for (i in 1:dim(classdata)[1]){
    text(classdata$caf_food[i], classdata$sadler[i], label=classdata$name[i], cex=.75, adj=c(.3,.3), col=gendernormative[i])
}


abline(lm(sadler~caf_food, data=classdata), col="black")






#######################
#####Dot Plot ######
#######################


stateprop <- table(classdata$state)/length(!is.na(classdata$state))
rank <- order(stateprop)
stateprop <-  stateprop[rank]


COLOR <- c("darkseagreen4")


dotchart(as.numeric(stateprop),labels=names(stateprop), cex=1.2, xlim=c(.0,.4), xlab="Proportion of R Class Students from State", pch=15, color=COLOR, main="Geographic Distribution of Students")


###Notice that DC is on there twice because of the different formats in which people entered it. This is a data management issue. 


####Whoa. Wait a minute. Virginia appears to be on there twice. Why is that? If you call stateprop to the screen and look very carefully, you can tell that there is a space behind the first "Virginia" entry. When you use the table() command to create stateprop, because data$state is a character vector, R thinks those are different values for the variable. Annoying, right? But this can actually be useful in other situations. 






#######################
#####Saving Your Plot ######
#######################


#Just make sure you've clicked into it and then use the menu bar for "File ————> Save"




##########################################
#####There are so many more things to learn! ######
##########################################


##Sadly, we are stopping here. But since plotting is one of the coolest features in R, I encourage you to learn more. 


To learn more about the control you have on different features of the plot:
http://www.statmethods.net/advgraphs/parameters.html



#Plotting in more than one window
http://www.statmethods.net/advgraphs/layout.html



#Plotting multiple things in the same window
par(new=TRUE)


#Adding legends
http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/legend.html



#Brainstorming Ideas
http://www.sr.bham.ac.uk/~ajrs/R/r-gallery.html
http://romainfrancois.blog.free.fr/index.php?tag/graphgallery






##########################################
#####Time Series #################### ######
##########################################


http://www.statmethods.net/advstats/timeseries.html



http://www.statmethods.net/management/reshape.html