library("kernlab")
# explore data 
letters <- read.csv("letterdata.csv")
str(letters)

# splitting data
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000,]

# classifier
letter_classifier <- ksvm(
  letter ~ ., 
  data = letters_train,
  kernel = "vanilladot"
  )
# predictions
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)

table(letter_predictions, letters_test$letter)

# total TRUE classifications
agreement <- letter_predictions == letters_test$letter
table(agreement)

# in %
prop.table(table(agreement))

# rbf kernel 
letter_classifier_rbf <- ksvm(
  letter ~ ., 
  data = letters_train, 
  kernel = "rbfdot"
  )
letter_predictions_rbf <- predict(
  letter_classifier_rbf, 
  letters_test
  )

agreement_rbf <- letter_predictions_rbf == letters_test$letter

table(agreement_rbf)

# generation C value
cost_values <- c(1, seq(from = 5, to = 40, by = 5))
accuracy_values <- sapply(cost_values, function(x) {
  set.seed(12345)
  m <- ksvm(letter ~ ., data = letters_train,
            kernel = "rbfdot", C = x) pred <- predict(m, letters_test)
  agree <- ifelse(pred == letters_test$letter, 1, 0) accuracy <- sum(agree) / nrow(letters_test)
  return (accuracy)})

plot(cost_values, accuracy_values, type = "b")