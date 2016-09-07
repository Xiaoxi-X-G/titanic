rm(list = ls())

Data.path <- "C:/gxx/Database/titanic"
Column.type <- c('integer',   # PassengerId
                 'factor',    # Survived 
                 'factor',    # Pclass
                 'character', # Name
                 'factor',    # Sex
                 'numeric',   # Age
                 'integer',   # SibSp
                 'integer',   # Parch
                 'character', # Ticket
                 'numeric',   # Fare
                 'character', # Cabin
                 'factor'     # Embarked
) 

train.raw <- read.csv(paste(Data.path, "/train.csv", sep=""),
                      colClasses = Column.type,
                      na.strings = c("", "NA")) 
# empty string is identified as "" or "NA", can replaced by real NA

Test.column.type <- Column.type[-2]
test.raw <- read.csv(paste(Data.path, "/test.csv", sep=""),
                     colClasses = Test.column.type,
                     na.strings = c("", "NA")) 


########## visually check - 1  #######
require(Amelia)
df.train <- train.raw

# plot miss infor
par(new=TRUE)

missmap(df.train, main ="Training - Missing Map",
        col = c("red", "black"), legend = F)

# Roughly 20 percent of the Age data is missing, and well above 
# 70 percent of the passengers cannot be linked to a specific cabin 
# number. While the proportion of Age "missings" is likely small 
# enough for reasonable replacement with some form of imputation, 
# the cabin missings seem too extensive to make reliable imputation 
# possible. Nevertheless, some data could be better than zero data, 
# so I'll look at cabin numbers later to see how we can put them to use.

barplot(table(df.train$Survived), names.arg = c("Perished", "Survived"))

barplot(table(df.train$Pclass), names.arg = c("1st", "2nd", "3rd"), main="Pclass")

barplot(table(df.train$Sex), main = "gender")

hist(df.train$Age, main = "Age")

barplot(table(df.train$SibSp), main="Siblings")

barplot(table(df.train$Parch), main = "parent+childen")

hist(df.train$Fare, main="Fare")

barplot(table(df.train$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main = "Embarked")


# Note the dominant categories in the first three graphs:
# -more passengers perished than survived
# -about twice as many passengers in 3rd class than in either 1st or 2nd
# -male passengers far outnumbered females



########## visually check - 2  #######
require(vcd)

mosaicplot(df.train$Pclass ~ df.train$Survived, main = "Survived in Pclass",
           color = T)

mosaicplot(df.train$Sex ~ df.train$Survived, main = "Survived in Gender",
           color = T)

boxplot(df.train$Age ~ df.train$Survived, main = "Survived in Age")

mosaicplot(df.train$Embarked ~ df.train$Survived, main = "Survived in Embarked",
           color = T)


########## visually check - 3  #######
# Check pair-wise cross-correlation
require(corrgram)

corrgram.data <- df.train
# change data type
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$SibSp <- as.numeric(corrgram.data$SibSp)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked,
                                  c("C"=1,"Q"=2,"S"=3))
corrgram.data$Parch <- as.numeric(corrgram.data$Parch)

corrgam.vars <- c("Survived", "Pclass", "Sex", "Age", 
                  "SibSp", "Parch", "Fare", "Embarked")

corrgram(corrgram.data[,corrgam.vars], order = F,
         upper.panel = panel.pie,
         lower.panel = panel.ellipse)


#### Summary of each varialbe #########
summary(df.train)

# Time to tackle those missing ages. A common approach to this type of 
# situation is to replacing the missings with the average of the available 
# values. In this case, that would mean replacing 177 missing Age values with 29.7.

# Taking that approach would be fine if only a small fraction of the ages were missing. 
# However, with missings accounting for 20 percent of all Age data in a relatively small 
# data set (<900 records), one could justify a search for a more refined method of imputation.


boxplot(df.train$Age ~ df.train$Survived, main = "Survived in Age")
# splitting the ages solely by Survived doesn't reveal much

boxplot(df.train$Age ~ df.train$Pclass, main = "Pclass in Age")
# Passengers in the upper classes (first and second) would tend to be wealthier, and in 
# that period of U.S. history, acquiring wealth usually required a good deal of time 
# (no dot-com kings in their 20s were aboard the Titanic on her maiden voyage). 

# There are no missing values in Pclass, so we could replace the missing age for, 
# say, a third class passenger with the average or median of the available ages for 
# those in Pclass="3". Doing so would be an improvement over assigning 29.7 to all Age missings.



##### Inspect Name #######
head(df.train$Name, n=10)
# The format "Surname, Title. Firstname..." is consistent in Name across all records

## extract the title from each name
getTitle <- function(data){
  starts <- regexpr("\\,[ A-Z]{1,20}\\.", data$Name, ignore.case = T)
  ends <- starts + attr(starts, "match.length")
  data$Title <- substr(data$Name, starts+2, ends-2)
  return (data$Title)
}



df.train$Title <- getTitle(df.train)

unique(df.train$Title)


### check missing Age against Title, and replace with median ########
require(Hmisc)

options(digits = 2)
aa <- bystats(df.train$Age, df.train$Title, fun = function(x)c(Mean=mean(x), 
                                                         Median = median(x)))

# Titles where Age requiring imputation
title.na.train <- setdiff(names(which(aa[,2] > 0)), "ALL")

imputeMedian <- function(impute.var, filter.var, var.level){
  for (v in var.level){
    impute.var[which(filter.var == v)] <- 
      impute(impute.var[which(filter.var==v)])
  }
  return(impute.var)
}

df.train$Age <- imputeMedian(df.train$Age, df.train$Title, title.na.train)


##### impute Embarked ####
df.train$Embarked <- impute(df.train$Embarked)
summary(df.train$Embarked)

df.train$Embarked <- as.factor(df.train$Embarked)

mosaicplot(df.train$Embarked ~ df.train$Survived, color = T)


summary(df.train)
# No missing data 
# but Fare shows 0 price tikets ????


#### checking Fare ####

# A zero fare might have been assigned to a baby. 
# However, a closer look at records where Fare = 0 suggests 
# otherwise...
subset(df.train, Fare <7)[order(subset(df.train, Fare <7)$Fare,
                                subset(df.train, Fare<7)$Pclass,
                                subset(df.train, Fare <7)$Age),
                          c("Age", "Title", "Pclass", "Fare")]
# The jump in fares from 0 to the 4-7 range suggests errors.

# impute 0 in Fare
df.train$Fare[which(df.train$Fare == 0)] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass,
                              as.factor(levels(df.train$Pclass)))


##### consolidate title ######
# A passenger's title can reflect gender, his/her position on the ship (officers & royalty),
# and access to a lifeboat (where "Master" superceded "Mr"). Making the effort to get the Title 
# feature model-ready seems worthwhile.


boxplot(df.train$Age ~ df.train$Title, xlab="title", ylab = "age ")

# Reclassified Title to a factor type
changeTitles <- function(dataT, old.titles, new.titles){
  for (nam in old.titles){
    dataT$Title[which(dataT$Title == nam)] <- new.titles
  }
  return(dataT$Title)
}

## Title consolidation
df.train$Title <- changeTitles(df.train, 
                               c("Capt", "Col", "Don", "Dr", 
                                 "Jonkheer", "Lady", "Major", 
                                 "Rev", "Sir"),
                               "Noble")

df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), 
                               "Mrs")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")

df.train$Title <- as.factor(df.train$Title)

boxplot(df.train$Age ~ df.train$Title, xlab="Title", ylab="Age")

# Title column can be considered a part of feature engineering

#### Other feature engineering ####
### Some Insights:
# 1. women and children first
# 2. some tickets were purchased in groups, adjust group purchases by size of family
# 3. First character in Cabin number represents the Deck 
# 4.  Odd-numbered cabins were reportedly on the port side of the ship
#     Even-numbered cabins assigned Side="starboard"
require(plyr)
require(stringr)

#### Test Even or Odd number
isEven <- function(x) x %in% c("0","2","4","6","8") 
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## Add feature engineering 
featureEngrg <- function(Data){
  ## Fate
  Data$Fate <- Data$Survived
  Data$Fate <- revalue(Data$Fate, c("1" = "Survived","0" = "Perished"))
  
  #1. women and children (Age < 15) first
  Data$Boat.dibs <- "No" # dibs inidcate on/off boat
  Data$Boat.dibs[which( (Data$Sex=="female") | (Data$Age < 15) )] <- "Yes"
  Data$Boat.dibs <- as.factor(Data$Boat.dibs)
  
  # 2. some tickets were purchased in groups, adjust group purchases by size of family
  Data$Family <- as.integer(Data$SibSp) + as.integer(Data$Parch)
  Data$Fare.pp <- Data$Fare / (Data$Family + 1)
  
  # Revalue Class
  Data$Class <- Data$Pclass
  Data$Class <- revalue(Data$Class, c("1"="First", "2"="Second", "3"="Third"))
  
  # 3. First character in Cabin number represents the Deck 
  Data$Deck <- substring(Data$Cabin, 1, 1)
  Data$Deck[which(is.na(Data$Deck))] <- "UNK"
  Data$Deck <- as.factor(Data$Deck)
  
  # 4.  Odd-numbered cabins were reportedly on the port side of the ship
  #     Even-numbered cabins assigned Side="starboard"
  Data$cabin.last.digit <- str_sub(Data$Cabin, -1)
  Data$Side <- "UNK"
  Data$Side[which( isEven(Data$cabin.last.digit)  )] <- "port"
  Data$Side[which( isOdd(Data$cabin.last.digit)  )] <- "starboard"
  Data$Side <- as.factor(Data$Side)
  
  Data$cabin.last.digit <- NULL # Delete tempere var
  
  return(Data)
}

df.train <- featureEngrg(df.train)


# Some color on the features I've added:

# Boat.dibs - assumes all females plus males under 15 get "dibs' on access
# to a lifeboat. Filtering by Title="Master" was considered, but the highest 
# age in the training data for males addressed as "Master" was just 12, and 
# I wanted to account for male teens with Title="Mr" who could pass for a child.

# Deck - levels are as shown in the Titanic cross-section displayed previously. 
# Cabin data provided for just 23 percent of training data records, so it's tough 
# to give this one much emphasis.

# Side - subject to the same concern (dearth of data) expressed for Deck


##### filter out un-useful info ####
# finish the data munging process by paring down the data frame to the 
# columns I will use in model building.
train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                 "Class", "Deck", "Side", "Fare", "Fare.pp", 
                 "Embarked", "Family")
df.train.munged <- df.train[train.keeps]

head(df.train.munged)
summary(df.train.munged)




###### Fitting a Model 1 ##############
## check correlations

corrgram.data2 <- df.train.munged

corrgram.data2$Fate <- as.numeric(corrgram.data2$Fate)
corrgram.data2$Sex <- as.numeric(corrgram.data2$Sex)
corrgram.data2$Boat.dibs <- as.numeric(corrgram.data2$Boat.dibs)
corrgram.data2$Title <- as.numeric(corrgram.data2$Title)
corrgram.data2$Class <- as.numeric(corrgram.data2$Class)
corrgram.data2$Deck <- as.numeric(corrgram.data2$Deck)
corrgram.data2$Side <- as.numeric(corrgram.data2$Side)
corrgram.data2$Embarked <- as.numeric(corrgram.data2$Embarked)

ToCheck <- c("Fate",
             "Sex",
             "Boat.dibs",
             "Age",
             "Title",
             "Class",
             "Deck",
             "Side",
             "Fare",
             "Fare.pp",
             "Embarked",
             "Family")


corrgram(corrgram.data2[,ToCheck], order=F, lower.panel = panel.ellipse,
           upper.panel = panel.pie)


require(caret)

## Split 80/20 for training and testing: preserve the distribution of the outcomes in the training and test sets 
set.seed(23)
train.rows <-createDataPartition(df.train.munged$Fate, p =0.8, list = F)
train.batch <- df.train.munged[train.rows,]
test.batch <- df.train.munged[-train.rows,]

### Model 1: Logistic regression y ~ f(X), f() is sigmoid function with parameter \theta
Titanic.logit.1 <- glm(Fate ~ Sex + Boat.dibs + Class + Age + Deck + Side + Fare.pp + Embarked + Family,
                       data = train.batch, family = binomial("logit"))

## Check corrgram to figure out Model2
Titanic.logit.2 <- glm(Fate ~ Sex + Boat.dibs + Class + Deck + Side + Fare.pp + Embarked, 
                       data = train.batch, family = binomial("logit"))
# The null deviance shows how well passenger survival is 
# predicted by a "null" model using only a constant (grand mean). 

anova(Titanic.logit.1, test = "Chisq")
anova(Titanic.logit.2, test = "Chisq")
Titanic.logit.3 <- glm(Fate ~ Sex + Boat.dibs + Class + Embarked + Age + Fare.pp + Embarked + Family, 
                       data = train.batch, family = binomial("logit"))



#### Train the Model ####
## Use the train function in Kuhn's caret package to fit binary logistic regression models