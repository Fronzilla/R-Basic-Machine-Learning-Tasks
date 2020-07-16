library(neuralnet)

concrete <- read.csv("concrete.csv")
str(concrete)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# confirm that the range is now between zero and one
summary(concrete_norm$strength)

# compared to the original minimum and maximum
summary(concrete$strength)

# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]


# simple ANN with only a single hidden neuron
set.seed(12345) # to guarantee repeatable results
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)

# visualize the network topology
plot(concrete_model)

model_results <- compute(concrete_model, concrete_test[1:8])
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

set.seed(12345) # to guarantee repeatable results
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

# plot the network
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

# create a custom softplus activation function
softplus <- function(x) { 
  log(1 + exp(x))
}

set.seed(12345)
concrete_model3 <- neuralnet(
  strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, 
  data = concrete_train, hidden = c(5, 5),
  act.fct = softplus)

# plot the network
plot(concrete_model3)

# evaluate the results as we did before
model_results3 <- compute(concrete_model3, concrete_test[1:8])
predicted_strength3 <- model_results3$net.result
cor(predicted_strength3, concrete_test$strength)

strengths <- data.frame(
  actual = concrete$strength[774:1030],
  pred = predicted_strength3 )

head(strengths, n = 3)

unnormalize <- function(x) {
  return((x * (max(concrete$strength)) -
          min(concrete$strength)) + min(concrete$strength))
}

strengths$pred_new <- unnormalize(strengths$pred)
strengths$error <- strengths$pred_new - strengths$actual

head(strengths, n = 3)
cor(strengths$pred_new, strengths$actual)
