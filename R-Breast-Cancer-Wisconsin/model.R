library(class)
library(gmodels)

# load the data
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors= FALSE)
str(wbcd)

# The first feature of data-set in Id - unique identifiers of a patient.
# iT does not provide useful information and we will need to exclude it from the model.

wbcd <- wbcd[-1]

table(wbcd$diagnosis)

# we need to encode the diagnosis variable.
wbcd$diagnosis <-factor(
  wbcd$diagnosis, levels = c("B", "M"),
  labels = c("Benign", "Malignant"))


round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)


summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# normalization of numerical data
normalize <- function(x){
  return ((x-min(x)) / (max(x) - min(x)))
}
# normalized
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)

# split data-set into training and testing
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# labels 
wbcd_train_labels <- wbcd[1:469, 1]

wbcd_test_labels <- wbcd[470:569, 1]

# k nears neighbors 
wbcd_test_pred <- knn(
  train = wbcd_train, 
  test = wbcd_test, 
  cl = wbcd_train_labels, 
  k = 21)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

# z - standardization
wbcd_z <- as.data.frame(scale(wbcd[-1]))

summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)
