library("rpart")
library("rpart.plot")
library(Cubist)

wine <- read.csv("whitewines.csv")
str(wine)
hist(wine$quality)
summary(wine)
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

m.rpart <- rpart(quality ~ ., data = wine_train)
summary(m.rpart)

rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)


MAE <- function(actual, predicted) {
  mean(abs(actual - predicted)) 
}

MAE(p.rpart, wine_test$quality)
mean(wine_train$quality)

m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)

p.cubist <- predict(m.cubist, wine_test)
summary(p.cubist)
cor(p.cubist, wine_test$quality)
MAE(wine_test$quality, p.cubist)
