#extreme gradient boosting
#raw data
#https://www.r-bloggers.com/2020/11/r-xgboost-regression/
#REMOVE THIS ABOVE LINK
library(xgboost)
library(caret)
library(dummies)
library(DiagrammeR)
library('fastDummies')
# Store 1 -----------------------------------------------------------------
# 1.1 Hobbies -------------------------------------------------------------
#rm(Hobbies_CA1)

title <- "extreme gradient boosting"


#Dummy Trials
trial_df= Hobbies_CA1[, c(12:15)]
trial_df
trial_df = dummy_cols(trial_df, select_columns = 'event_name_1')
trial_df
trial_df[,c(5:28)]

tmp <- Hobbies_CA1[, c(12:15)] #change list to categorical variables
tmp1 <- dummy_cols(Hobbies_CA1[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))

d <- data.frame(Hobbies_CA1[, c(2,10,11,16:582)], tmp1[,c(5:28)]) #add the dummies to the dataframe but filter the initial 
                                                #one to drop the ones converted to dummies
#Check types 
str(d)

#sort based on date first 

m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

dim(train) #you will get (1941,x) 
train[, 2:594] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:594], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:594])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

#plot first 3 trees of model


importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")

#grid search
#create hyperparameter grid
hyper_grid <- expand.grid(max_depth = seq(3, 6, 1),
                          eta = seq(.2, .35, .01))

hyper_grid
xgb_train_rmse <- NULL
xgb_test_rmse <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    data = train[, 2:594],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j]
  )
  
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
}


which.min(xgb_test_rmse)
xgb_test_rmse[53]

hyper_grid[which.min(xgb_test_rmse), ]


# 1.2 Household_1 ---------------------------------------------------------

#Encoding Categorical Variables
col= ncol(HOUSEHOLD_1_CA1)
tmp <- HOUSEHOLD_1_CA1[, c(12:15)] #change list to categorical variables
tmp1 <- dummy_cols(HOUSEHOLD_1_CA1[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
d <- data.frame(HOUSEHOLD_1_CA1[, c(3,10,11,16:col)], tmp1[,c(5:41)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)

#sort based on date first 

m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] #you will get (1941,x) 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")

#grid search
#create hyperparameter grid
hyper_grid <- expand.grid(max_depth = seq(3, 6, 1),
                          eta = seq(.2, .35, .01))

hyper_grid
xgb_train_rmse <- NULL
xgb_test_rmse <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    data = train[, 2:col],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j]
  )
  
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
}

hyper_grid[which.min(xgb_test_rmse), ]


# 1.3 Household_2 -------------------------------------------------------------

#Encoding Categorical Variables
col= ncol(HOUSEHOLD_2_CA1)
tmp <- HOUSEHOLD_2_CA1[, c(12:15)] #change list to categorical variables

tmp1 <- dummy_cols(HOUSEHOLD_2_CA1[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(HOUSEHOLD_2_CA1[, c(4,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# 1.4 Foods_1 -----------------------------------------------------------------

#Encoding Categorical Variables
col= ncol(FOODS_1_CA1)
tmp1 <- dummy_cols(FOODS_1_CA1[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(FOODS_1_CA1[, c(5,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# 1.5 Foods_2 -----------------------------------------------------------------

#Encoding Categorical Variables
col= ncol(FOODS_2_CA1)
tmp1 <- dummy_cols(FOODS_2_CA1[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(FOODS_2_CA1[, c(6,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))
importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")
# 1.6 Foods_3 -------------------------------------------------------------


#Encoding Categorical Variables
col= ncol(FOODS_3_CA1)
tmp1 <- dummy_cols(FOODS_3_CA1[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(FOODS_3_CA1[, c(7,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# Store 2


# Store_2 -----------------------------------------------------------------


# 2.1 Hobbies -------------------------------------------------------------
#Encoding Categorical Variables
col= ncol(HOBBIES_CA2)
tmp1 <- dummy_cols(HOBBIES_CA2[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(HOBBIES_CA2[, c(2,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# 2.2 Household_1 ---------------------------------------------------------

#Encoding Categorical Variables
col= ncol(HOUSEHOLDS_1_CA2)
tmp1 <- dummy_cols(HOUSEHOLDS_1_CA2[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(HOUSEHOLDS_1_CA2[, c(3,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")



# 2.3 Household_2 -------------------------------------------------------------

#Encoding Categorical Variables
col= ncol(HOUSEHOLDS_2_CA2)
tmp1 <- dummy_cols(HOUSEHOLDS_2_CA2[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(HOUSEHOLDS_2_CA2[, c(4,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# 2.4 Foods_1 -------------------------------------------------------------

#Encoding Categorical Variables
col= ncol(FOOD_1_CA2)
tmp1 <- dummy_cols(FOOD_1_CA2[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(FOOD_1_CA2[, c(5,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")
# 2.5 Foods_2 -------------------------------------------------------------
#Encoding Categorical Variables
col= ncol(FOODS_2_CA2)
tmp1 <- dummy_cols(FOODS_2_CA2[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(FOODS_2_CA2[, c(6,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# 2.6 Foods_3 -------------------------------------------------------------

#Encoding Categorical Variables
col= ncol(FOODS_3_CA2)
tmp1 <- dummy_cols(FOODS_3_CA2[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(FOODS_3_CA2[, c(7,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# Store_3 -----------------------------------------------------------------
# 3.1 Hobbies -------------------------------------------------------------
#Encoding Categorical Variables
col= ncol(HOBBIES_CA3)
tmp1 <- dummy_cols(HOBBIES_CA3[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(HOBBIES_CA3[, c(2,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .29  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# 3.2 Household_1 ---------------------------------------------------------

#Encoding Categorical Variables
col= ncol(HOUSEHOLD_1_CA3)
tmp1 <- dummy_cols(HOUSEHOLD_1_CA3[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(HOUSEHOLD_1_CA3[, c(3,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")

# 3.3 Household_2 -------------------------------------------------------------

#Encoding Categorical Variables
col= ncol(HOUSEHOLD_2_CA3)
tmp1 <- dummy_cols(HOUSEHOLD_2_CA3[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(HOUSEHOLD_2_CA3[, c(4,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# 3.4 Foods_1 -------------------------------------------------------------

#Encoding Categorical Variables
col= ncol(FOODS_1_CA3)
tmp1 <- dummy_cols(FOODS_1_CA3[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(FOODS_1_CA3[, c(5,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")
# 3.5 Foods_2 -------------------------------------------------------------
#Encoding Categorical Variables
col= ncol(FOODS_2_CA3)
tmp1 <- dummy_cols(FOODS_2_CA3[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(FOODS_2_CA3[, c(6,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


# 3.6 Foods_3 -------------------------------------------------------------

#Encoding Categorical Variables
col= ncol(FOODS_3_CA3)
tmp1 <- dummy_cols(FOODS_3_CA3[, c(12:15)], select_columns = c('event_name_1','event_type_1', 'event_name_2','event_type_2'))
dum_col= ncol(tmp1)
d <- data.frame(FOODS_3_CA3[, c(7,10,11,16:col)], tmp1[,c(5:dum_col)]) 
#add the dummies to the dataframe but filter the initial 
#one to drop the ones converted to dummies
#Check types 
str(d)
#sort based on date first 
m <- as.matrix(d)
#set.seed(123)
#indices <- sample(1:nrow(cars_19), size = 0.75 * nrow(cars_19))  #change this to whatever indices you want
train <- m[1:1941,] #train on certain indices #validate
#train_evaluate <- m[1914:1941,] create this as train and then create validate as .:1913
test <- m[1942:1969,] #test on the rest

col2= dim(train)[2] 
train[, 2:col2] #put the second number in the following line like: train[,2:x]
m1_xgb <-
  xgboost(
    data = train[, 2:col2], #train on everything except the first column
    label = train[, 1], 
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #based on cross validation
    eta = .33  #based on cross validation
  )
rm(pred_xgb)

pred_xgb <- predict(m1_xgb, test[, 2:col2])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

mean((y - yhat)^2) #mse
caret::MAE(y, yhat) #mae
caret::RMSE(y, yhat) #rmse

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)

abline(lm(yhat ~ y))

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")
