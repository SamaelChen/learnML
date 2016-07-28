calcDist <- function(x, y){
    x <- as.matrix(x)
    y <- as.numeric(y)
    return(sqrt(rowSums((t(t(x) - y))^2)))
}

k_means <- function(data, k, maxIteration=100, randomSeed=1234, threshold=0.001){
    data[,ncol(data)+1] <- 'unknow'
    data[,ncol(data)+1] <- Inf
    names(data)[ncol(data)-1] <- 'cluster'
    names(data)[ncol(data)] <- 'distance'
    set.seed(randomSeed)
    randomCentroids <- sample(1:nrow(data), k)
    centroids <- data[randomCentroids, names(data)!='distance']
    centroids[, 'cluster'] <- paste('cluster', 1:k, sep='')
    centroidsNew <- centroids
    iter <- 1
    totalDiff <- -Inf
    while((iter <= maxIteration) & (totalDiff < k)){
        centroids <- centroidsNew
        for(i in 1:k){
            dist <- calcDist(data[,!(names(data) %in% c('distance', 'cluster'))], centroids[i, names(centroids)!='cluster'])
            index <- which(data$distance > dist)
            data[index, 'distance'] <- dist[index]
            data[index, 'cluster'] <- centroids[i, names(centroids)=='cluster']
        }
        centroidsNew <- aggregate(data[,!(names(data) %in% c('distance', 'cluster'))], by=list(cluster=data$cluster), FUN='mean')
        #print(summary(as.factor(data$cluster)))
        diff <- sqrt(rowSums((centroidsNew[,names(centroidsNew)!='cluster']-centroids[,names(centroids)!='cluster'])^2))
        totalDiff <- sum(diff <= 0.001)
        print(sprintf('iterations: %d', iter))
        iter = iter+1
    }
    return(data)
}
