# 26.10.2021

library(ggplot2)
library(caret)
library(dplyr)

main_train_data <- read.csv(file = "data/train.csv")
main_test_data <- read.csv(file = "data/test.csv")

###### Preliminary analysis

ggplot(main_train_data) +
  geom_jitter(aes(x = Sex, y = Survived))

## Higher chance of surviving if you are female
## Var: Sex matters

ggplot(main_train_data) +
  geom_jitter(aes(x = Age, y = Survived))

## Don't see a pattern

ggplot(main_train_data) +
  geom_jitter(aes(x = Pclass, y = Survived))

## Higher chance of surviving if you are in first class
## Var: Pclass matters

ggplot(main_train_data) +
  geom_jitter(aes(x = SibSp, y = Survived))

## Looks like a pattern

ggplot(main_train_data) +
  geom_jitter(aes(x = Ticket, y = Survived))

## I think this variable is useful
## Var: Ticket

ggplot(main_train_data) +
  geom_jitter(aes(x = Parch, y = Survived))

main_train_data %>%
  group_by(Parch) %>%
  summarise(prob = sum(Survived)/891)

## Looks like a pattern

ggplot(main_train_data) +
  geom_jitter(aes(x = Embarked, y = Survived))

main_train_data %>%
  group_by(Embarked) %>%
  summarise(prob = sum(Survived)/891)

## Perhaps port matters
## Var: Embarked

#### Splitting the dataset
set.seed(1)
train_index <- createDataPartition(y = main_train_data$Survived, p = 0.7, list = FALSE)
training <- main_train_data[train_index,]
testing <- main_train_data[-train_index,]

#### Let's use a random forest model
modFit <- train(Survived ~ Sex + Pclass + Embarked + SibSp + Parch, data = training, method = "rf", prox = TRUE)

pred <- predict(modFit, testing[c("Sex", "Pclass", "Embarked", "SibSp", "Parch")])
table(pred, testing$Survived)

confusionMatrix(as.factor(round(pred)), as.factor(testing$Survived))



##### Let's use the test data

pred_test <- predict(modFit, main_test_data[c("Sex", "Pclass", "Embarked", "SibSp", "Parch")])

result <- data.frame(PassengerId = main_test_data$PassengerId, Survived = as.numeric(round(pred_test)))

write.csv(result, file = "result.csv", row.names = FALSE)
