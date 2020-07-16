library("OneR")
library("RWeka")
# load the data
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)
# drop unnecessary data 
mushrooms$veil_type <- NULL
table(mushrooms$type)

# applying 1-R classifier 
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(actual = mushrooms$type, predicted = mushroom_1R_pred)

mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip_pred <- predict(mushroom_JRip, mushrooms)
table(mushrooms$type, predicted = mushroom_JRip_pred)


