# Code for the mixed effect logistic regression of decoding accuracies reported in a la Emberson, Zinszer, Raizada and Aslin

library(R.matlab)
library(ggplot2)
library(reshape2)
library(lme4)

# manually toggle between data sets, to start select the first dataset, run through to line 113 and follow the instructions in the comments for when to come back and toggle to the other dataset
data <- readMat('dataSet1_setsizes1through10_WDIGcontrol_cond2vs3.mat')
#data <- readMat('dataSet2_setsizes1through10_multiRScond2vs4.mat')

# set up the data for each set size into a dataframe with relevant subject information
# set size 2
foo <- (data$setsize2.results[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep(2,nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
# set up the data frame
setsize_data <- goo

# set size 3
foo <- (data$setsize3.results[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep(3,nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
setsize_data <- rbind(setsize_data,goo)

# set size 4
foo <- (data$setsize4.results[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep(4,nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
setsize_data <- rbind(setsize_data,goo)

# set size 5
foo <- (data$setsize5.results[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep(5,nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
setsize_data <- rbind(setsize_data,goo)

# set size 6
foo <- (data$setsize6.results[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep(6,nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
setsize_data <- rbind(setsize_data,goo)

# set size 7
foo <- (data$setsize7.results[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep(7,nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
setsize_data <- rbind(setsize_data,goo)

# set size 8
foo <- (data$setsize8.results[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep(8,nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
setsize_data <- rbind(setsize_data,goo)

# set size 9
foo <- (data$setsize9.results[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep(9,nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
setsize_data <- rbind(setsize_data,goo)


# set size 10
foo <- (data$setsize10.results[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep(10,nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
setsize_data <- rbind(setsize_data,goo)

# top 3 channels 
foo <- (data$setsize3.results.mostinformativechannels[11])  # get the accuracy per set
doo <- foo[[1]]
goo <- melt(doo)
colnames(goo) <- c("set","channel","Ss","acc")
goo$setsize <- rep('Top 3\nChannels',nrow(goo))
goo$channel <- as.factor(goo$channel)
goo$Ss <- as.factor(goo$Ss)
setsize_data <- rbind(setsize_data,goo)

# save current data set and then add another 
WDIG_data <- setsize_data
WDIG_data$experiment <- 'Dataset #1'

####
# now go back to the top, change the data inputted to another data set (e.g., MultiRS2 and run the code up to setsize_data <- rbind(setsize_data,goo) line 113 again)
setsize_data$experiment <- 'Dataset #2'
setsize_merged <- rbind(WDIG_data,setsize_data)
setsize_merged$experiment <- as.factor(setsize_merged$experiment)

# implementation of the models as reported (in detail) in the Supplementary Materials of Emberson, Zinszer, Raizada & Aslin

# first comparing decoding accuracy from set sizes 2 to 10 

# set up data frame to include set sizes 2 and 10 (without top 3 channels)
setsize_2to10 <- subset(setsize_merged,setsize!='Top 3\nChannels')
setsize_2to10$setsize <- as.numeric(setsize_2to10$setsize)

model1 <- glmer(acc ~ experiment + (1|Ss), setsize_2to10,family = "binomial")
summary(model1) 

model2 <- glmer(acc ~ experiment*channel + (1|Ss), setsize_2to10, family="binomial")
summary(model2) 

anova(model1,model2, test="Chisq")

model3 <- glmer(acc ~ experiment*channel*setsize + (1|Ss), setsize_2to10, family="binomial")
summary(model3)

anova(model2,model3, test="Chisq")

# second, compare channels between set size 3, top 3 channels and set size 10

# set up this new dataframe with these 3 set sizes only
foo <- subset(setsize_merged,setsize=='Top 3\nChannels')
foo$setsize <- "Top3"
foo <- rbind(foo,subset(setsize_merged,setsize=='3'))
setsize_310Top3 <- rbind(foo,subset(setsize_merged,setsize=='10'))
setsize_310Top3$setsize <- as.factor(setsize_310Top3$setsize)
setsize_310Top3$setsize <- relevel(setsize_310Top3$setsize,"Top3")

Top3_model1 <- glmer(acc~experiment+(1|Ss) +(1|channel), setsize_310Top3,family="binomial")
summary(Top3_model1)

Top3_model2 <- glmer(acc~experiment*setsize+(1|Ss)+(1|channel), setsize_310Top3,family="binomial")
summary(Top3_model2)

anova(Top3_model1, Top3_model2,test="Chisq")