library(dplyr)
# lade die daten
data(mtcars)
head(mtcars)
?mtcars # show data description

summary(mtcars)
# n = anzahl der reihen
n     <- nrow(mtcars) #number of rows

# split in trainings und test set
train <- mtcars[sample(n, n/2), ]
train <- sample_n(mtcars, n / 2)

test <- mtcars[setdiff(row.names(mtcars), row.names(train)), ]


# KNN - K- Nearest Neighbours
library(class)
# train model
(model.knn <- knn(
    train = subset(train, select = -gear),
    test  = subset(test, select = -gear),
    cl    = train$gear,
    k     = 5))
# show importance
plot(model.knn)

# calculate root mean squared error
sqrt(mean((test$gear- as.numeric(as.character(model.knn)))^2))

# confusion matrix
table(test$gear, as.numeric(as.character(model.knn)))

### classification tree
library(rpart)
model.tree <- rpart(factor(gear) ~ ., data = train, minsplit = 3)
summary(model.tree)
plot(model.tree);text(model.tree)

#predict test set
pred.tree <- predict(model.tree, newdata = test, type = 'class')# calculate root mean squared error
sqrt(mean((test$gear- as.numeric(as.character(model.tree)))^2))


# random forrest
library(randomForest)
(rf <- randomForest(factor(gear) ~ ., data = train, ntree = 250))
table(test$gear, predict(rf, test))

# plotting
plot(rf)
legend('topright', colnames(rf$err.rate), col = 1:4, fill = 1:4, bty = 'n')