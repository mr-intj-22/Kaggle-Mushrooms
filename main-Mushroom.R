# Analyzing Mushroom data
require('caret')


###################################################################     Globals&CONSTANTS      ###############################################################
#INPUT PATH
input_path <- "./../input"
#OUTPUT PATH
output_path <- "./../output"
# Final file of predictions
output_name <- "Predictions"
pathinput<-"./Mushroom/mushrooms.csv"
dataset<-read.csv2(pathinput , sep =",")
head(dataset)
str(dataset)

#summary(dataset)

dataset <- subset(dataset, select = - veil.type)
dataset <- subset(dataset, select = - stalk.root)

#summary(dataset)

set.seed(100)

split.data <- createDataPartition(dataset$class, p=0.8,list = FALSE) #
trainmushrooms<- dataset[ split.data, ] #
testmushrooms <- dataset[-split.data, ] #
#summary(testmushrooms)

library(rpart)
library(rpart.plot)
set.seed(100)

model_tree <- rpart(class ~ ., data = trainmushrooms,
                    method = "class", cp=0.0011)

rpart.plot(model_tree, extra = 104, box.palette = "GnBu", 
           branch.lty = 3, shadow.col = "gray", nn = TRUE)

require('e1071')

caret::confusionMatrix(data=predict(model_tree, type = "class"), 
                       reference = trainmushrooms$class, 
                       positive="e")

penalty_matrix <- matrix(c(0, 1, 10, 0), byrow = TRUE, nrow = 2)

model_tree_penalty <- rpart(class ~ ., data = trainmushrooms, method = "class", 
                            parms = list(loss = penalty_matrix))

rpart.plot(model_tree_penalty)

caret::confusionMatrix(data=predict(model_tree_penalty, type = "class"), 
                       reference = trainmushrooms$class, 
                       positive="e")

test_tree <- predict(model_tree, newdata = testmushrooms)
caret::confusionMatrix(data = predict(model_tree, newdata = testmushrooms, type = "class"), 
                       reference = testmushrooms$class, 
                       positive = "e")

