set.seed(1987)
#if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead

#generate grade scores 100 students in 24 diff subjects
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#Q1 visualize data

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

y

#Q2 correlation
 
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)



#Qe SVD
s <- svd(y)
names(s)


y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- colSums(y*y)

V <- s$v

class(V)
class(y)

U <- s$u
D <- s$d

yv <- y %*% V

yv <- U %*% D
ss_yv <- colSums(yv*yv)
ss_yv

plot(1:ncol(y), ss_y)
plot(1:ncol(y), ss_yv)


ss_y
ss_yv

s_d <- s$d * s$d
s_d

plot(s$d, sqrt(ss_yv))

data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()

sqrt(ss_yv)
356.9+173.9+125.2

#variability
yv <- y %*% s$v

yv

pca <- prcomp(yv)
pca_sum <- summary(pca)

pca
pca_sum
sum(pca_sum$importance[2,1:3])

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

#Q8 
avg_subject <- colSums(y) / 100
avg_subject

ud <- s$u %*% diag(s$d)
ud
u1d11 <- ud[1,1:17]
u1d11
plot(avg_subject, u1d11)

yv1 <- s$v[1,1:17]
plot(avg_subject, yv1)

u1 <- s$u[1:100, 1]

s$v
vt1 <- t(s$v[1:24, 1])
vt1

my_image(y)

u1d11vt1 <- u1d11 %*% vt1
my_image(u1d11vt1)

plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)

resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
