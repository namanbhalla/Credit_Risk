##load data

## read the dataset
library(readr)
train <- read_csv("~/Desktop/Sberbank Russia/Credit Risk/train.csv")

# change column names to something more readable
colnames(train) <- c("customer_code", "limit_balance","sex","education","marital_status","birthdate","late_december","late_november","late_october","late_september","late_august","late_july","december_bill","november_bill","october_bill","september_bill","august_bill","july_bill","paid_december","paid_november","paid_october","paid_september","paid_august","paid_july","Default")

#clean quotes and data - fix education, sex and default
#calculate age from birthdate

library(plyr)
library(stringr)

del <- colwise(function(x) str_replace_all(x, '\"', "")) #function to remove quotes
del_hash <- colwise(function(x) str_replace_all(x, '#', ""))
train <- del(train) #apply to train
train[train == "#N/A"] <- NA

## lets look for missing values
missing <- unlist(lapply(train, function(x) any(is.na(x)))) #list all columns with missing values
missing[missing == "TRUE"]

#so age, education, marital_status and sex have missing values. Let's drop the variables

train_f <- train[complete.cases(train),]

#note here I have created a new copy of dataset for train_f. I will train a model first with outliers, then conduct analysis on it without outliers and see whether to keep the outliers in or remove it. 
#Some people remove outliers completely, I do not recommend it.

#function to calculate Age from Birthday
library(eeptools)
colnames(train_f)[6] <- "Age" #get age from birthday
train_f$Age <- as.Date(train_f$Age,format='%d/%m/%Y') #convert charachter to date

#now we can extract age from birthdate, eeptools provides a well optimized function which can take care of leap years etc
train_f$Age <- floor(age_calc(train_f$Age, enddate = Sys.Date(), units = "years"))


#our data-set is cleaner, we have extracted age, removed NA and quotes. Next we assign right classes to the variables
str(train_f)  #check structures

#change few to factors
train_f$sex <- as.factor(train_f$sex)    
train_f$education <- as.factor(train_f$education)
train_f$marital_status <- as.factor(train_f$marital_status)
train_f$Default <- as.factor(train_f$Default)

#change rest to numeric
for (i in 6:24) {
  train_f[i] <- as.numeric(unlist(train_f[i]))
}

train_f[2] <- as.numeric(unlist(train_f[2]))

train_f[6] <- as.numeric(unlist(train_f[6]))
train_f[7] <- as.factor(unlist(train_f[7]))
train_f[8] <- as.factor(unlist(train_f[8]))
train_f[9] <- as.factor(unlist(train_f[9]))
train_f[10] <- as.factor(unlist(train_f[10]))
train_f[11] <- as.factor(unlist(train_f[11]))
train_f[12] <- as.factor(unlist(train_f[12]))
# we also don't want customer_code  #people from some cultures might think that they are bankrupt because of an unlucky customer code, but we do not really want to study that correlation right now :p

train_data <- train_f[,2:25] #agin I make a new copy, I want to come back to it later


train_data[,6:11] <- abs(train_data[,6:11])


##############################################
# Let's Vizualize and transform the data-set #
##############################################

summary(train_data)

#lets make another copy of data-frame, that I can standardize

standard_train <- train_data

#let's reorginize standard_train. 

standard_train <- cbind(train_data[,1], train_data[,5], train_data[,12:23],train_data[,2:4],train_data[,6:11],train_data[,24])

colnames(standard_train)[1] <- "Limit_Balance"

colnames(standard_train)[2] <- "Age"
colnames(standard_train)[24] <- "Default"
#I like putting all quant variables together. Now let's do some work. Cleaning completed.



standard_train[,1:14] <-scale(standard_train[,1:14], center = TRUE, scale = TRUE)  # calculate z-score of variables



################################
# Let us explore CARET library #
################################













#############################
###     Model Building   ####
#############################
#lets study distribution of classes

table(standard_train$Default)

#highly imbalanced. #first thing to do is balance the classes and then take 

#standard train needs to split into 80 (training) -20 (validation) - (5115 rows for validation set)
#23% defaulters in 5115 #implies 1175 defaulters and 3940 non defaulters

## let's sample

standard_train_defaulters <- standard_train[standard_train$Default==1,]  #out of this 5724 a random sample of 1175 goes to validation set
standard_train_nondefaulters <- standard_train[standard_train$Default==0,] #3940 random rows go to validation set

#then I row-bind this into a clean data-frame and row-bind others into another clean dataset
indices <- sample(1:nrow(standard_train_defaulters), 1175)
train_this_part_one <- standard_train_defaulters[-indices,]
validate_this_part_one <- standard_train_defaulters[indices,]


indices_two <- sample(1:nrow(standard_train_nondefaulters), 3940)
train_this_part_two <- standard_train_nondefaulters[-indices_two,]
validate_this_part_two <- standard_train_nondefaulters[indices_two,]

###
train_this <- rbind(train_this_part_one,train_this_part_two)
validate_this <- rbind(validate_this_part_one,validate_this_part_two)

###I could have used a standard library for partitioning such as Caret, I like it better when I can see what library does. It's not memory efficient though


### Now let's get our first model. 

## train models then validate and then F-Selector.
