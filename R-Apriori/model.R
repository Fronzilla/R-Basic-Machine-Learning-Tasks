library("arules")
# sparse matrix

groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)
inspect(groceries[1:5])
itemFrequency(groceries[, 1:3])

itemFrequencyPlot(groceries, support = 0.1)

itemFrequencyPlot(groceries, topN = 20)

image(groceries[1:5])

image(sample(groceries, 100))

apriori(groceries)

groceryrules <- apriori(
  groceries, 
  parameter = list(support = 0.006, confidence = 0.25, minlen = 2)
)

# ~ set of 463 rules 
groceryrules

summary(groceryrules)

inspect(groceryrules[1:10])

inspect(sort(groceryrules, by = "lift")[1:5])

# inspect berries
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# berries with yogurt 
berryrules_yogurt <- subset(groceryrules,  items %ain% c("berries", "yogurt"))
inspect(berryrules_yogurt)

# save results 
write(
  groceryrules, 
  file = "groceryrules.csv", 
  sep = ",", 
  quote = TRUE, 
  row.names = FALSE
)
# as data frame
groceryrules_df <- as(groceryrules, "data.frame")

str(groceryrules_df)
