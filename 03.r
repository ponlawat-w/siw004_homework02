# 3.a

data <- source('./chap7tibetskull.dat')$value
data <- data[, -6] # Eliminate `type` data

attach(data)

# pairs(data)
# cor(data)
# dev.off()

data.pc <- princomp(data[, -1], cor = TRUE)
summary(data.pc, loadings = TRUE)
out <- lm(Length ~ (data.pc$scores[,1] + data.pc$scores[,2] + data.pc$scores[,3]))

out

summary(out)

out <- lm(Length ~ (data.pc$scores[,1] + data.pc$scores[,2]))

out

summary(out)

# 3.b

data <- source('./chap7tibetskull.dat')$value
originalType <- data[, 6]
data <- data[, -6]

n <- length(data[, 1])
wss1 <- (n - 1) * sum(apply(data, 2, var))

wss <- numeric(0)
for (i in 2:10) {
  W <- sum(kmeans(data, i)$withinss)
  wss <- c(wss, W)
}
wss <- c(wss1, wss)

plot(1:10, wss, type = 'l',
  xlab = 'Number of groups',
  ylab = 'Within groups sum of squares',
  lwd = 2)

dev.off()

data.k <- kmeans(data, 3)
data.result <- data
data.result$oldType <- originalType
data.result$kmeans <- data.k$cluster
data.result

# life.k <- kmeans(life, 2)

# lapply(1:2, function(nc) {
#   apply(life[life.k$cluster == nc,], 2, mean)
# })

# We have selected 3

data

par(mfrow = c(1, 3))
hclust.single <- hclust(dist(data), method = 'single')
plclust(hclust.single, labels = row.names(data), ylab='Distance')
abline(h = 14, col = 'red')
title('Single Linkage')
hclust.complete <- hclust(dist(data), method = 'complete')
plclust(hclust.complete, labels = row.names(data), ylab='Distance')
abline(h = 30, col = 'red')
title('Complete Linkage')
hclust.average <- hclust(dist(data), method = 'average')
plclust(hclust.average, labels = row.names(data), ylab='Distance')
abline(h = 22, col = 'red')
title('Average Linkage')

dev.off()

# We have selected complete linkage

treecut2 <- cutree(hclust.complete, h = 30)
treecut2
data$oldType <- originalType
data$newType2 <- treecut2
data #yeiyei

# treecut3 <- cutree(hclust.average, h = 22)
# treecut3
# data$oldType <- originalType
# data$newType3 <- treecut3

# treecut2 <- cutree(hclust.average, h = 24)
# treecut2
# data$oldType <- originalType
# data$newType2 <- treecut2

data
