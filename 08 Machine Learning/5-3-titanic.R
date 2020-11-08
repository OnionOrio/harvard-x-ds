library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

titanic_clean


# Q1 Split data into train_set and test_set
set.seed(42, sample.kind="Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list= FALSE)

train_set <- titanic_clean[-test_index,]
test_set <- titanic_clean[test_index,]

count(train_set)

size <- count(test_set)

# Q2 accuracy by guessing
set.seed(3, sample.kind = "Rounding")
y_predicted <- sample(c(0,1), 179, replace = TRUE)
y_predicted

mean(test_set$Survived == y_predicted)


# Q3 predict survival rate by Sex
train_set %>% filter(Sex == "female") %>% count(Survived == "1")
p_female <- 182/(182+67)

train_set %>% filter(Sex == "male") %>% count(Survived == "1")
p_male <- 91/(372+91)

test_predicted <- test_set %>% mutate(predicted = ifelse(Sex == "male", 0, 1))
mean(test_predicted$predicted == test_set$Survived)

# Q4a,b Predict survival rate by passenger class
train_set %>% filter(Pclass == "1") %>% count(Survived == "1")
train_set %>% filter(Pclass == "2") %>% count(Survived == "1")
train_set %>% filter(Pclass == "3") %>% count(Survived == "1")

test_predicted <- test_set %>% mutate(predicted_2 = ifelse(Pclass == "1", 1, 0))
mean(test_predicted$predicted_2 == test_set$Survived)

# Q4c,d group by Sex, Passenger class
train_grouped <- train_set %>% select(Sex, Pclass, Survived) %>% group_by(Sex, Pclass) %>% summarise(count = n(), survival = sum(Survived=="1")/count)
train_grouped

test_predicted <- test_predicted %>% mutate(predicted_3 = ifelse((Pclass == "1" | Pclass == "2") & Sex == "female", 1, 0))  
mean(test_predicted$predicted_3 == test_set$Survived)


# Q5 Confusion Matrix
y_predicted <-  test_predicted$predicted %>% factor(levels = levels(test_predicted$Survived))
y_predicted
test_predicted$Survived

cSex <- confusionMatrix(data = y_predicted,  reference = test_predicted$Survived, positive = '0')
cSex

y_predicted_2 <- test_predicted$predicted_2 %>% factor(levels = levels(test_predicted$Survived))
cPclass <- confusionMatrix(data = y_predicted_2,  reference = test_predicted$Survived, positive = '0')

y_predicted_3 <- test_predicted$predicted_3 %>% factor(levels = levels(test_predicted$Survived))
cSPclass <- confusionMatrix(data = y_predicted_3, reference =  test_predicted$Survived, positive = '0')

cSex
cPclass
cSPclass

# Q6 F1 Score
fSex <- F_meas(data = y_predicted, reference = test_predicted$Survived, postive = '0')
fPclass <- F_meas(data = y_predicted_2, reference = test_predicted$Survived, postive = '0')
fSPclass <- F_meas(data = y_predicted_3, reference = test_predicted$Survived, postive = '0')

fSex
fPclass
fSPclass

# Q7 LDA and QDA
set.seed(1, sample.kind = "Rounding")
train_lda <- caret::train(Survived ~ Fare, method = 'lda', data = train_set)
y_hat <- predict(train_lda, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall[["Accuracy"]]

set.seed(1, sample.kind = "Rounding")
train_qda <- caret::train(Survived ~ Fare, method = 'qda', data = train_set)
y_hat <- predict(train_qda, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall[["Accuracy"]]


?caret::train

# Q8 Logistic regression models
set.seed(1, sample.kind = " Rounding")
train_glm <- caret::train(Survived ~ Age, method = 'glm', data = train_set)
y_hat <- predict(train_glm, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall[["Accuracy"]]

set.seed(1, sample.kind = " Rounding")
train_glm_2 <- caret::train(Survived ~ Sex + Pclass + Age + Fare, method = 'glm', data = train_set)
y_hat <- predict(train_glm_2, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall[["Accuracy"]]

set.seed(1, sample.kind = " Rounding")
train_glm_3 <- caret::train(Survived ~ ., method = 'glm', data = train_set)
y_hat <- predict(train_glm_3, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall[["Accuracy"]]

# Q9 kNN model
k <- seq(3, 51, 2)

set.seed(6, sample.kind = "Rounding")
fit <- caret::train(Survived ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k =  seq(3, 51, 2)))
fit
y_hat <- predict(fit, test_set, type = "raw") %>% factor(levels = levels(test_set$Survived))
cm_test <- confusionMatrix(data = y_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]


accuracy <- function(k)
{
  fit <- caret::train(Survived ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k =  seq(3, 51, 2)))
  y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = levels(test_set$Survived))
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$Survived)
  cm_test$overall["Accuracy"]
}

acc <- sapply(k, accuracy)
which.max(acc)
k[9]

acc
plot(k, acc)

# Q10 Cross-validation
set.seed(8, sample.kind = "Rounding")
tGrid <- expand.grid(k = seq(3, 51, 2))
train_ctrl <- trainControl(method = "cv", number = 10, p = 0.9)
set.seed(8, sample.kind = "Rounding")
fit <- caret::train(Survived ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k =  seq(3, 51, 2)), trControl = train_ctrl)
fit
y_hat <- predict(fit, test_set, type = "raw") %>% factor(levels = levels(test_set$Survived))
cm_test <- confusionMatrix(data = y_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]

# Q11 Rpart
tGrid <- expand.grid(cp = seq(0, 0.05, 0.002))
set.seed(10, sample.kind = "Rounding")
fit <- caret::train(Survived ~ ., method = "rpart", data = train_set, tuneGrid = tGrid)
fit
y_hat <- predict(fit, test_set, type = "raw") %>% factor(levels = levels(test_set$Survived))
cm_test <- confusionMatrix(data = y_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]

plot(fit$finalModel) + text(fit$finalModel)

# Q12 Random Forest Model
set.seed(14, sample.kind = "Rounding")
fit <- train(Survived ~ ., data = train_set, method="rf", ntree = 100, tuneGrid = expand.grid(mtry = seq(1:7)))
y_hat <- predict(fit, test_set, type = "raw") %>% factor(levels = levels(test_set$Survived))
cm_test <- confusionMatrix(data = y_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]

importance <- varImp(fit)
importance
