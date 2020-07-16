library("psych")

# load the data 
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

summary(insurance$expenses)
hist(insurance$expenses)

table(insurance$region)

cor(insurance[c("age", "bmi", "children", "expenses")])

pairs(insurance[c("age", "bmi", "children", "expenses")])
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

ins_model <- lm(expenses ~ ., data = insurance)
summary(ins_model)
# feature engineering
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

expenses ~ bmi30*smoker


ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)

insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$expenses)

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)


predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1, 
                   smoker = "no", region = "northeast")
        )

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "female", bmi30 = 1, 
                   smoker = "no", region = "northeast")
        )

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 0,
                   bmi = 30, sex = "female", bmi30 = 1, 
                   smoker = "no", region = "northeast")
        )
