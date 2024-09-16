library(caret)

load("pca-examples.rdata")

# We will work with nyt.frame
nyt_data = nyt.frame

str(nyt_data)
summary(nyt_data$class.labels)

colnames(nyt_data)[sample(ncol(nyt_data), 30)]
signif(nyt_data[sample(nrow(nyt_data), 5), sample(ncol(nyt_data), 10)], 3)


# ----------------------- Oppgave 1 --------------------
nyt_pca = prcomp(nyt_data[,-1])
biplot(nyt_pca, scale=0)

nyt_loading = nyt_pca$rotation[, 1:2]
informative_loadings = rbind(
  head(nyt_loading[order(abs(nyt_loading[,1]), decreasing = TRUE),]),
  head(nyt_loading[order(abs(nyt_loading[,2]), decreasing = TRUE),])
)
biplot(x = nyt_pca$x[,1:2], y = informative_loadings)

#--

pr.var <- nyt_pca$sdev^2
pr.var

pve <- pr.var/sum(pr.var)
pve

plot(1:length(pr.var),pve)
plot(1:length(pr.var),cumsum(pve))


# -------------------- Oppgave 3 -------------------

km.out <- kmeans(nyt_data[,-1],2,nstart=20)
nyt_ref <- as.factor(ifelse(nyt_data$class.labels=="art",2,1))
confmat <- confusionMatrix(as.factor(km.out$cluster), nyt_ref)$table
confmat

plot(nyt_pca$x[,1:2],type="n")
points(nyt_pca$x[,1:2],pch=1, lwd=2, cex=2, col=nyt_ref)
points(nyt_pca$x[,1:2], pch= 19, col=as.factor(km.out$cluster))

# ------------------ Oppgave 4 ---------------------

hclust.out <- hclust(dist(nyt_data[,-1]), method="complete")
plot(hclust.out)
hclust.clust <- as.factor(cutree(hclust.out,2))
nyt_ref <- as.factor(ifelse(nyt_data$class.labels=="art",1,2))

confmat <- confusionMatrix(hclust.clust, nyt_ref)$table
confmat

plot(nyt_pca$x[,1:2],type="n")
points(nyt_pca$x[,1:2],pch=1, lwd=2, cex=2, col=nyt_ref)
points(nyt_pca$x[,1:2], pch= 19, col=hclust.clust)








