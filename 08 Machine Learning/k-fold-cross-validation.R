library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

library(genefilter)
tt <- colttests(x, y)

names(tt)

pval <- tt$p.value
pval

ind <- pval < 0.01

length(ind)

length(pval[ind])

length(x)

ind <- which(pval < 0.01)
ind

set.seed(1996, sample.kind="Rounding")
tt
class(x)
x[1]
length(x[ind])
x2 <- x[ind]
y[ind]
length(y[ind])
y2 <- y[ind]

x_subset <- x[ ,ind]

fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)

which.max(fit)

library(dslabs)
data("tissue_gene_expression")

length(tissue_gene_expression$x)
length(tissue_gene_expression$y)

x <- tissue_gene_expression$x
x
y <- tissue_gene_expression$y
y
fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))

fit
