library(dslabs)
library(tidyverse)

data(tissue_gene_expression)

tissue_gene_expression

tissue_gene_expression %>% dist(1,2)

tissue_gene_expression$x

d <- dist(tissue_gene_expression$x)

as.matrix(d)[73, 74]

image(as.matrix(d))
