library("rpart")
library("dslabs")
library("tidyverse")
library("caret")
data("tissue_gene_expression")


cp <- seq(0, 0.1, 0.01)
set.seed(1991, sample.kind="rounding")

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
tGrid <- expand.grid(cp = seq(0, 0.1, 0.01))
train.ctrl <- trainControl(method = "cv", number = 10)
tGrid
set.seed(1991, sample.kind="rounding")
train_rpart <- caret::train(x, y, method ="rpart", tuneGrid = tGrid)

?caret::train

train_rpart

plot(train_rpart)

rpart.ctrl <- rpart.control(minsplit = 0, cp = cp)

set.seed(1991, sample.kind="Rounding")
fit_rpart <- train(x, y, method = "rpart", control = rpart.ctrl, tuneGrid = tGrid)

train_rpart_2
set.seed(1991, sample.kind="rounding")
y_hat_knn <- predict(train_rpart_2, x, type = "raw")
confusionMatrix(train_rpart_2)

plot(train_rpart_2$finalModel) + text(train_rpart_2$finalModel)

mtry <- seq(50, 200, 25)

set.seed(1991, sample.kind="Rounding")
fit <- train(x, y, method="rf", nodesize = 1, tuneGrid = expand.grid(mtry = mtry))
fit
plot(fit$finalModel)

?varImp

imp <- varImp(fit)
imp
set.seed(1991, sample.kind="Rounding")
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
