rm(list = ls())

Data.path <- "C:/Users/Xiaoxi/Dropbox/work/2016-2017OtherDAProject/DataSet/titanic"
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




###### Fitting a Model 1
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


###### Logistic models #####

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



#### Train the Model 
## Use the train function in Kuhn's caret package to fit binary logistic regression models

### K-fold validation: Overcome overfitting problem.
# In K-fold validation, the original sample are randomed partitioned into K-equally subsamples.
# Of the K subsamples,  a single subsample is used as validation data for testing the model, the
# rest K-1 subsamples are used to training the model. The process then repeats K times, which later
# averaged to a single estimation.

##1. 10-fold validation, 3 times
##2. AUC (Area under ROC curve) is as performance metric to figure out the parameters

cv.ctrl <- trainControl(method = "repeatedcv",
                        repeats = 5,   # "repeatedcv" is used to specify repeated K-fold cross-validation 
                        #(and the argument repeats controls the number of repetitions). K
                        #is controlled by the number argument and defaults to 10.
                        classProbs = T,
                        #Since the ROC curve is based on the predicted class probabilities 
                        #(as to define the threshold, which are not computed automatically), 
                        # another option is required. The classProbs = TRUE option is 
                        #used to include these calculations.
                        summaryFunction = twoClassSummary
                        # https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
                        
                        # summaryFunction is used to measure the performance of the model by taking
                        # the observed and predicted value
                        
                        #The twoClassSummary will compute measures specific to two-class problems, 
                        #such as the area under the ROC curve, the sensitivity and specificity,
                        # which is the True Positive Rate VS True negative rate.
                        )

set.seed(35)
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
summary(glm.tune.1)


# class compression:
# Think of it as collapsing particular levels on a categorical variable. 
#  use Embarked and the I() function, which inhibits interpretation & conversion of R objects, 
# to create a new 2-level factor within the model formula. This factor is valued TRUE if a 
# passenger's port of origin was Southampton ("S"), or FALSE otherwise.

set.seed(35)
glm.tune.2 <- train(Fate ~ Sex + Class + Age + Family + I(Embarked == "S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
summary(glm.tune.2)


####
set.seed(35)
glm.tune.3 <- train(Fate ~ Sex + Class + Age + Family + Title + I(Embarked == "S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

summary(glm.tune.3)
# Drop Sexmale, add Title=Mr, Noble

### 
set.seed(35)
glm.tune.4 <- train(Fate ~  Class + Age + Family + I(Embarked == "S")
                    + I(Title == "Mr") + I(Title=="Noble"),
                    method = "glm",
                    metric = "ROC",
                    data = train.batch,
                    trControl = cv.ctrl)
summary(glm.tune.4)


###
#Remember that there were a lot of male passengers in third class. Given the 
# "women and children first" policy already mentioned plus  third-class men might make a 
# further dent in that residual deviance

set.seed(35)
glm.tune.5 <-train(Fate ~ Class + Age + Family + I(Embarked == "S") +  
                     I(Title == "Mr") + I(Title=="Noble") +
                     I(Title=="Mr" & Class=="Third"),
                   data = train.batch,
                   method = "glm",
                   metric = "ROC",
                   trControl = cv.ctrl)
summary(glm.tune.5)

#### Turns out, glm.tune.5 gives the best results


#### Other models

#### Boosting #####
require(ada)
## First up is adaptive boosting. I can instruct train to fit a stochastic boosting model for 
# the binary response Fate using the adapackage and a range of values for each of three 
# tuning parameters. Concretely, when fitting a model using train with method="ada", 
# one has three levers to tweak: iter (number of boosting iterations, default=50), 
# maxdepth (depth of trees), and nu (shrinkage parameter, default=1).

# In particular, boosting is to find stronger and more comprehesive rulse from many weaker rules.
# Step1: Use a weak/simple rule as a classfier, and then evolve to multiple version by panilying incorrect results
# step2: Combine all weak rules and generate a stronger rules. Which later can be used to generate more 
#        stronger rules with different version
# ..., continue, until .depth or accuracy met

ada.grid <- expand.grid(.iter = c(50, 100),
                        .maxdepth = c(4, 8),
                        .nu = c(0.1, 1))
set.seed(35)
ada.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)
ada.tune
plot.train(ada.tune)


#### Random forest #####
require(randomForest)
# The number of randomly pre-selected predictor variables for each node, 
# designated mtry, is the sole parameter available for tuning an RF with train.

# step1: Use bootstrap to generate many training dataset.
# step2: build predictive model for each training dataset.
# step3: Combine results (averaging or majority vote) and produce finial predicts.

set.seed(35)
rf.grid <- data.frame(.mtry = c(2,3)) # mtry =  the square root of the number of variables
rf.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                 data = train.batch,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)
rf.tune


### Support VEctor Machine #####
# fit a support vector machine (SVM) model to the Titanic data.
# Use Kernel to separate data one the observation feature space
# One of the population is Radial Kernel to meature the distance between observations,
# ie: exp(-gamma sum(Xi-Xj)^2)

# The cost parameter controls the size of margin: distance from margin to hyperplane
# The default grid of cost parameter C is 0.25, 0.5, and 1. If we set train argument tuneLength = 9, 
# the grid expands to c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64).

# As SVM is considered sensitive to the scale and magnitude of the presented features, 
# I'll use the preProcess argument to instruct train to make arrangements for normalizing 
# the data within resampling loops

set.seed(35)
svm.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                  data = train.batch,
                  method = "svmRadial",
                  tuneLength = 9,
                  preProcess = c("center", "scale"),
                  metric = "ROC",
                  trControl =  cv.ctrl)

svm.tune

# You may have noticed that the same random number seed was set prior to fitting each model. 
# This ensures that the same resampling sets are used for each model, enabling an "apple-to-apples" 
# comparison of the resampling profiles between models during model evaluation.


#### Model Evaluation 
library(e1071)
# With all four models in hand, I can begin to evaluate their performance by whipping together
# some cross-tabulations of the observed and predicted Fate for the passengers in the test.batch data. 
# caret makes this easy with the confusionMatrix function.

# Logistics
glm.pred <- predict(glm.tune.5, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)

# Boosted 
ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Fate)

# RF
rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Fate)

# SVM
svm.pred <- predict(svm.tune, test.batch)
confusionMatrix(svm.pred, test.batch$Fate)

####
library(pROC)
# We can also calculate, using each of the four fitted models, 
# the predicted probabilities for the test.batch, and use those 
# probabilities to plot the ROC curves.

# Logistic = 0.89
glm.probs <- predict(glm.tune.5, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Fate,
               predictor = glm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(glm.ROC, type = "S")


# Boost model = 0.88
ada.probs <- predict(ada.tune, test.batch, type = "prob")
ada.ROC <- roc(response = test.batch$Fate,
               predictor = ada.probs$Survived,
               levels = levels(test.batch$Fate))
plot(ada.ROC, add=T, col = "green")

# RF = 0.89
rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Fate,
              predictor = rf.probs$Survived,
              levels = levels(test.batch$Fate))
plot(rf.ROC, add = T, col = "red")

# SVM = 0.89
svm.probs <- predict(svm.tune, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Fate,
               predictor = svm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(svm.ROC, add = T, col = "blue")


## The following R script uses caret function resamples to collect the resampling results, 
# then calls the dotplot function to create a visualization of the resampling distributions.
# One graph which sums up the performance of the four models, this is it.
cv.values <- resamples(list(Logit = glm.tune.5,
                            Ada = ada.tune,
                            RF = rf.tune,
                            SVM = svm.tune))
cv.values 
# Number of resamples = K * repeats (K = K-fold cross validation, where repeat 'repeats' times)

dotplot(cv.values, metric = "ROC")



# The next graph (my last, scout's honor) compares the four models on the basis of ROC, 
# sensitivity, and specificity. Here, sensitivity ("Sens" on the graph) is the probability 
# that a model will predict a Titanic passenger's death, given that the passenger actually 
# did perish. Think of sensitivity in this case as the true perished rate. Specificity ("Spec"), 
# on the other hand, is the probability that a model will predict survival, given that the 
# passenger actually did survive. 
bwplot(cv.values)

# Simply put, all four models were better at predicting passenger fatalities than survivals,
# and none of them are significantly better or worse than the other three. Of the four, 
# if I had to pick one, I'd probably put my money on the logistic regression model. 