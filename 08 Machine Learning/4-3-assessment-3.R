library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

result <- train(x, y, method = 'lda')

t(result$finalModel$means)

plot(t(result$finalModel$means))

result <- train(x, y, method = 'qda')

result
plot(t(result$finalModel$means))

result <- train(x, y, method = 'lda', preProcess = 'center')

plot(t(result$finalModel$means))

?plot

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

result <- train(x, y, method = 'lda', preProcess = 'center')

plot(t(result$finalModel$means))

t(result$finalModel$means)

result
