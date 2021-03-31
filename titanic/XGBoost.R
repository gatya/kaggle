# Load csv files
library(dplyr) 
train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test) # bind training & test data

## 2 Feature Engineering
# 2.1 What is in a name?

# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
# table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "royalty" and "officer" level
royal_title <- c('Dona', 'Lady', 'the Countess', 'Don', 'Sir', 'Jonkheer')

officer_title <- c('Capt', 'Col', 'Dr', 'Major', 'Rev')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% royal_title]  <- 'Royalty'
full$Title[full$Title %in% officer_title]  <- 'Officer'

# Show title counts by sex again
# table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])


# 2.2 Do families sink or swim together?
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# 2.3 Treat a few more variables
# This variable appears to have a lot of missing values
# full$Cabin[1:28]

# The first character is the deck. For example:
# strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

## 3 Missingness
library(mice)
# 3.1 Sensible value imputation
# Passengers 62 and 830 are missing Embarkment
# full[c(62, 830), 'Embarked']

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

# Show row 1044
# full[1044, ]

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# 3.2 Predictive imputation
# Show number of missing Age values
# sum(is.na(full$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(123)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
# sum(is.na(full$Age))

# 3.3 Feature Engineering: Round 2
# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
# table(full$Child, full$Survived)

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
# table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

# Engineer features based on all the passengers with the same ticket
ticket.unique <- rep(0, nrow(full))
tickets <- unique(full$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full$Ticket == current.ticket)
  
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}

full$ticket.unique <- ticket.unique

full$ticketS[full$ticket.unique == 1]   <- 'Single'
full$ticketS[full$ticket.unique < 5 & full$ticket.unique>= 2]   <- 'Small'
full$ticketS[full$ticket.unique >= 5]   <- 'Big'

# Cabin / Deck
full$Cabin[full$Cabin == ''] <- 'M' # Missing cabin as M
full$Cabin <- substr(full$Cabin, 0, 1) # Only first character of Cabin
full$Cabin[full$Cabin == 'T'] <- 'A' # Cabin T to A

full$Cabin[full$Cabin %in% c('A','B','C')]  <- 'ABC'
full$Cabin[full$Cabin %in% c('D','E')]  <- 'DE'
full$Cabin[full$Cabin %in% c('F','G')]  <- 'FG'

## 4 Prediction
# Encoding the target feature as factor
full$Pclass = as.numeric(full$Pclass)
full$Sex = as.numeric(factor(full$Sex,
                             levels = c('female', 'male'),
                             labels = c(1,2)))
full$Title = as.numeric(factor(full$Title,
                               levels = c('Master', 'Miss', 'Mr', 'Mrs', 'Officer', 'Royalty'),
                               labels = c(1,2,3,4,5,6)))
full$Embarked = as.numeric(factor(full$Embarked,
                                  levels = c('C', 'Q', 'S'),
                                  labels = c(1,2,3)))
full$FsizeD = as.numeric(factor(full$FsizeD,
                                levels = c('large', 'singleton', 'small'),
                                labels = c(1,2,3)))
full$Child = as.numeric(factor(full$Child,
                               levels = c('Adult', 'Child'),
                               labels = c(1,2)))
full$Mother = as.numeric(factor(full$Mother,
                                levels = c('Mother', 'Not Mother'),
                                labels = c(1,2)))
full$ticketS = as.numeric(factor(full$ticketS,
                                 levels = c('Single', 'Small', 'Big'),
                                 labels = c(1,2,3)))
full$Cabin = as.numeric(factor(full$Cabin,
                                 levels = c('ABC', 'DE', 'FG', 'M'),
                                 labels = c(1,2,3,4)))

# Encode Factor with > 2 levels as Dummy
library(dummies)
full = dummy.data.frame(full,
                        names = c("Title","Embarked","FsizeD","ticketS","Cabin"),
                        sep = ".")

# Remove unnecessary vars
full_set = full[ -c(1,4,9,24,26,30)]

# Split the data back into a train set and a test set
training_set <- full_set[1:891,]
test_set <- full_set[892:1309,]

# Feature Scaling
training_set[-1] = scale(training_set[-1])
test_set[-1] = scale(test_set[-1])

# Tuning xgbTree with caret
library(caret)
input_x <- as.matrix(select(training_set, -Survived))
input_y <- training_set$Survived

# Step 1: Number iterations and the learning rate
nrounds <- 1000

# note to start nrounds from 200, as smaller learning rates result in errors so
# big with lower starting points that they'll mess the scales
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE, # FALSE for reproducible results,
  classProbs = TRUE, # Tambahan bwt metric ROC
  summaryFunction = twoClassSummary # Tambahan bwt metric ROC
)

xgb_tune <- caret::train(
  x = input_x,
  y = factor(input_y, labels = make.names(c("notSurvived", "survived"))), # Diganti factor bwt metric ROC
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE,
  metric = "ROC" # Tambahan Metric ROC
)

# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

#tuneplot(xgb_tune)

#xgb_tune$bestTune

# Step 2: Maximum depth and minimum child weight
tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                     c(xgb_tune$bestTune$max_depth:4),
                     xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

xgb_tune2 <- caret::train(
  x = input_x,
  y = factor(input_y, labels = make.names(c("notSurvived", "survived"))),
  trControl = tune_control,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = TRUE,
  metric = "ROC" # Tambahan Metric ROC
)

#tuneplot(xgb_tune2)

#xgb_tune2$bestTune

# Step 3: Column and row sampling
tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

xgb_tune3 <- caret::train(
  x = input_x,
  y = factor(input_y, labels = make.names(c("notSurvived", "survived"))),
  trControl = tune_control,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE,
  metric = "ROC" # Tambahan Metric ROC
)

#tuneplot(xgb_tune3, probs = .95)

#xgb_tune3$bestTune

# Step 4: Gamma
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  x = input_x,
  y = factor(input_y, labels = make.names(c("notSurvived", "survived"))),
  trControl = tune_control,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE,
  metric = "ROC" # Tambahan Metric ROC
)

#tuneplot(xgb_tune4)

#xgb_tune4$bestTune

# Step 5: Reducing learning rate
tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  x = input_x,
  y = factor(input_y, labels = make.names(c("notSurvived", "survived"))),
  trControl = tune_control,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE,
  metric = "ROC" # Tambahan Metric ROC
)

#tuneplot(xgb_tune5)

#xgb_tune5$bestTune

# Fitting XGBoost to the Training set
# install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-1]),
                     label = training_set$Survived,
                     nrounds = xgb_tune5$bestTune$nrounds,
                     params = list(max_depth = xgb_tune5$bestTune$max_depth,
                                   eta = xgb_tune5$bestTune$eta,
                                   min_child_weight = xgb_tune5$bestTune$min_child_weight,
                                   gamma = xgb_tune5$bestTune$gamma,
                                   colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
                                   subsample = xgb_tune5$bestTune$subsample)
                     )

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-1]))
y_pred = (y_pred >= 0.5)

# Bring back PassengerId
test_set$PassengerId = test$PassengerId

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test_set$PassengerId, Survived = y_pred)
solution$Survived = as.numeric(solution$Survived)

# Write the solution to file
write.csv(solution, file = 'XGBoost_Solution.csv', row.names = F)