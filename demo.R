library(tidyverse)
library(magrittr)
library(caret)
library(randomForest)

#load data
train <- read_csv("input/application_train.csv")
test <- read_csv("input/application_test.csv")

df <- bind_rows(train, test)

df$TARGET <- factor(df$TARGET)

#convert character variables to numeric
df %<>% 
  mutate_if(is.character, funs(factor(.) %>% as.numeric(.)))

#remove character variables
# df %<>% 
#   select_if(~!is.character(.))

#impute missing values by median
df[, 3:ncol(df)] <- sapply(df[, 3:ncol(df)], function(x) x[is.na(x)] <- median(x, na.rm = TRUE))

#remove low variance variables
nzv_cols <- nearZeroVar(df)
if (length(nzv_cols) > 0) df <- df[,-nzv_cols]

#split data to training and test set
train <- df[!is.na(df$TARGET), ]
test <- df[is.na(df$TARGET), ]

#logistic regression
model_glm <- glm(TARGET ~ ., data = train[, -1], family = "binomial")

preds <- predict(model_glm, test, type = "response")

data.frame(SK_ID_CURR = test$SK_ID_CURR, TARGET = preds) %>% 
  write_csv("demo_submission_lr.csv")

#random forest
model_rf <- randomForest(TARGET ~ ., data = train[, -1], ntree = 50)

preds <- predict(model_rf, test[, -1], type = "prob")[, 2]

data.frame(SK_ID_CURR = test$SK_ID_CURR, TARGET = preds) %>% 
  write_csv("demo_submission_rf.csv")
