life.original <- source('./chap4lifeexp.dat')$value

life <- life.original
life$mw0 <- abs(life$w0 - life$m0)
life$mw25 <- abs(life$w25 - life$m25)
life$mw50 <- abs(life$w50 - life$m50)
life$mw75 <- abs(life$w75 - life$m75)

life <- life[-1:-8]

life

# K-Means
# Normalise data
n <- length(life[, 1])
wss1 <- (n - 1) * sum(apply(life, 2, var))

wss <- numeric(0)
for (i in 2:10) {
  W <- sum(kmeans(life, i)$withinss)
  wss <- c(wss, W)
}
wss <- c(wss1, wss)

plot(1:10, wss, type = 'l',
  xlab = 'Number of groups',
  ylab = 'Within groups sum of squares',
  lwd = 2)

dev.off()

# We have selected 2

par(mfrow = c(1, 3))
hclust.single <- hclust(dist(life), method = 'single')
plclust(hclust.single, labels = row.names(life), ylab='Distance')
title('(a) Single Linkage')
hclust.complete <- hclust(dist(life), method = 'complete')
plclust(hclust.complete, labels = row.names(life), ylab='Distance')
title('(a) Complete Linkage')
hclust.average <- hclust(dist(life), method = 'average')
plclust(hclust.average, labels = row.names(life), ylab='Distance')
title('(a) Average Linkage')

dev.off()

# We have selected complete linkage

treecut <- cutree(hclust.complete, h = 15)
life$group <- treecut # add group number to data
life

# Create LDA

library(MASS)
dis <- lda(group ~
  mw0 + mw25 + mw50 + mw75,
  data = life)

# Create new data object
newdata <- rbind(
  c(65, 50, 33, 15, 69, 57, 37, 16),
  c(59, 46, 31, 15, 64, 56, 33, 16)
)
colnames(newdata) <- colnames(life.original)
newdata <- data.frame(newdata)
newdata$mw0 <- abs(newdata$w0 - newdata$m0)
newdata$mw25 <- abs(newdata$w25 - newdata$m25)
newdata$mw50 <- abs(newdata$w50 - newdata$m50)
newdata$mw75 <- abs(newdata$w75 - newdata$m75)
newdata <- newdata[-1:-8]
newdata

predict(dis, newdata = newdata)
