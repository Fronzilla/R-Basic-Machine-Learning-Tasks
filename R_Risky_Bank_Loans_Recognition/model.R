library(C50)
library(gmodels)
library(caret)
library(ipred)
library(adabag)
library(vcd)
library(randomForest)
library(pROC)

# load the data
credit <- read.csv("credit.csv")

# examine some data
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)

# 70% loans returned, 30% not 
table(credit$default)

# random seed
set.seed(123)
# data shuffle 
train_sample <- sample(1000, 900)
str(train_sample)
# splitting data set
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

# checking proportion of 70 / 30
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

credit_train$default<-as.factor(credit_train$default)


# teaching the model
credit_model <- C5.0(credit_train[-17], credit_train$default)

summary(credit_model)

credit_pred <- predict(credit_model, credit_test)
CrossTable(
  credit_test$default, 
  credit_pred,
  prop.chisq = FALSE, 
  prop.c = FALSE, 
  prop.r = FALSE, 
  dnn = c('actual default', 'predicted default')
  )

# model optimization
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)

summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(
  credit_test$default, 
  credit_boost_pred10,
  prop.chisq = FALSE, 
  prop.c = FALSE, 
  prop.r = FALSE,   
  dnn = c('actual default', 'predicted default')
)

matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)

credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)

credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(
  credit_test$default, 
  credit_cost_pred,
  prop.chisq = FALSE, 
  prop.c = FALSE, 
  prop.r = FALSE, 
  dnn = c('actual default', 'predicted default')
  )

# applying caret 
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")
p <- predict(m, credit)
table(p, credit)

head(predict(m, credit, type = "prob"))

ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")

grid <- expand.grid(model = "tree",
                    trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    winnow = FALSE)

m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa", trControl = ctrl, tuneGrid = grid)
# baggind 
mybag <- bagging(default ~ ., data = credit, nbagg = 25)

credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# baggind from caret 
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)
# boosting 
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion

adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion
Kappa(adaboost_cv$confusion)

# random forest
rf <- randomForest(default ~ ., data = credit)
Kappa(rf$confusion[1:2,1:2])

ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10, selectionFunction = "best", savePredictions = TRUE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
grid_rf <- expand.grid(mtry = c(2, 4, 8, 16))

m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "ROC", trControl = ctrl, tuneGrid = grid_rf)

roc_rf <- roc(m_rf$pred$obs, m_rf$pred$yes)
plot(roc_rf, col = "red", legacy.axes = TRUE)

