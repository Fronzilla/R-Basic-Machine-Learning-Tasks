library(stats)

# read data 
teens <- read.csv("snsdata.csv")
str(teens)

# looking for NA values 
table(teens$gender, useNA = "ifany")
summary(teens$age)

# dealing with 3 year or 106 year age values 
teens$age <- ifelse(
  teens$age >= 13 & teens$age < 20, 
  teens$age, 
  NA
)

summary(teens$age)
teens$female <- ifelse(
  teens$gender == "F" & !is.na(teens$gender), 
  1, 
  0
)

teens$no_gender <- ifelse(
  is.na(teens$gender), 
  1, 
  0
)

table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# dealing with missing age 
mean(teens$age, na.rm = TRUE)

# aggregate mean
aggregate(data = teens, 
          age ~ gradyear, 
          mean, 
          na.rm = TRUE
)
ave_age <- ave(
  teens$age, 
  teens$gradyear, 
  FUN = function(x) mean(x, na.rm = TRUE)
)

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)

# 36 features as k
interests <- teens[5:40]

# z standardization
interests_z <- as.data.frame(lapply(interests, scale))
summary(interests$basketball)
summary(interests_z$basketball)

# k means 
set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)

teen_clusters$size
teen_clusters$centers

# model tuning 
teens$cluster <- teen_clusters$cluster
teens[1:5, c("cluster", "gender", "age", "friends")]
aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)
