library(dslabs)
library(tidyverse)
library(caret)
library(tissuesGeneExpression)

data("tissue_gene_expression")
dim(tissue_gene_expression$x)
t1 <- tissue_gene_expression
t2x <- tissue_gene_expression$x

pca <- prcomp(t2x)

pcs <- data.frame(pca$rotation[,1:2], name = colnames(t2x))

pcs

t1$y

t10 <- cbind(pcs, data.frame(t1$y)) %>% select (PC1,PC2,t1.y) %>% set_names("PC1", "PC2", "y")

t10 %>% ggplot(aes(PC1, PC2 , color = y )) + geom_point()

x <- tissue_gene_expression$x

pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

row_avgs <- rowMeans(tissue_gene_expression$x)

temp <- data.frame(a=pc$x[,1], b=row_avgs, y=tissue_gene_expression$y)

temp %>% ggplot(aes(a, b, col=y)) + geom_point()

cor(row_avgs, pc$x[,1])

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
x

pc2 <- prcomp(x)
data.frame(pc_7 = pc$x[,7], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_7, tissue, color = tissue)) +
  geom_boxplot()

summary(pc)

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
d

plot(d)
hclust_avg <- hclust(d, method = 'average')
plot(hclust_avg)

h <- hclust(d)
plot(h)

library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]

heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(colors))
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
