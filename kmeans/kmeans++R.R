data <- read.table('/media/samael/Samael/CVM/1、K-means学习/kmeans', header=F, sep='\t')

#calculate Euclidean distance
calcDist <- function(x, y){
  x <- as.matrix(x)
  y <- as.numeric(y)
  return(sqrt(rowSums((t(t(x) - y))^2)))
}

initCentroids <- function(data, k, randomSeed=1234){
  set.seed(randomSeed)
  centroids <- data[sample(1:nrow(data),1),]
  i <- 1
  while(i < k){
    dist <- rep(0, nrow(data))
    #The origin algorithm is calculate the maximum probability of x. Due to the denominator is the sum
    #of all distance, here only choose the maximum distance.
    for(j in 1:nrow(centroids)){
      dist <- dist+calcDist(data, centroids[j,])
    }
    centroids <- rbind(centroids, data[which.max(dist),])
    i <- i+1
  }
  return(centroids)
}

k_means <- function(data, k, maxIteration=100, randomSeed=1234, threshold=0.001){
  centroids <- initCentroids(data, k=k, randomSeed=randomSeed)
  data[,ncol(data)+1] <- 'unknow'
  data[,ncol(data)+1] <- Inf
  names(data)[ncol(data)-1] <- 'cluster'
  names(data)[ncol(data)] <- 'distance'
  # set.seed(randomSeed)
  # randomCentroids <- sample(1:nrow(data), k)
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
      if(i==1){
          data[which(data$distance==0), 'distance']=Inf
      }
      data[index, 'cluster'] <- centroids[i, names(centroids)=='cluster']
    }
    centroidsNew <- aggregate(data[,!(names(data) %in% c('distance', 'cluster'))], by=list(cluster=data$cluster), FUN='mean')
    #print(summary(as.factor(data$cluster)))
    diff <- sqrt(rowSums((centroidsNew[,names(centroidsNew)!='cluster']-centroids[,names(centroids)!='cluster'])^2))
    totalDiff <- sum(diff <= 0.001)
    print(sprintf('iterations: %d', iter))
    dev.new()
    plot(data$V1, data$V2, col=as.factor(data$cluster), pch=1:4, xlab='X', ylab='Y')
    points(centroidsNew[,c('V1', 'V2')], pch=8, col='purple')
    abline(h=0, v=0)
    iter = iter+1
  }
  return(data)
}

dataCluster <- k_means(data=data, k=4, randomSeed = 1024, threshold = 0.0000001)
plot(dataCluster$V1, dataCluster$V2, col=as.factor(dataCluster$cluster), pch=as.numeric(as.factor(dataCluster$cluster)), xlab='X', ylab='Y')
abline(h=0, v=0)

plot(dataORIC$V1, dataORIC$V2, col=as.factor(dataORIC$cluster), pch=as.numeric(as.factor(dataORIC$cluster)), xlab='X', ylab='Y')
