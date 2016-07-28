calcDist <- function(x, y){
    x <- as.matrix(x)
    y <- as.numeric(y)
    return(sqrt(rowSums((t(t(x) - y))^2)))
}

#kmeans++ initial centroids function
initCentroids <- function(data, k, randomSeed=1234){
    set.seed(randomSeed)
    centroids <- data[sample(1:nrow(data),1),]
    i <- labels
    while(i < k){
        dist <- rep(0, nrow(data))
        for(j in 1:nrow(centroids)){
            #The origin algorithm is calculate the maximum probability of x. Due to the denominator is the sum
            #of all distance, here only choose the maximum distance.
            dist <- dist+calcDist(data, centroids[j,])
        }
        centroids <- rbind(centroids, data[which.max(dist),])
        i <- i+1
    }
    return(centroids)
}

#semi-supervised kmeans++ initial centroids function
ss_kpp_initC <- function(labeledData, data, k, labelIndex){
    nLabels <- length(unique(labeledData[,labelIndex]))
    centroids <- aggregate(labeledData[, -labelIndex], by=list(cluster=labeledData[, labelIndex]), FUN='mean')
    centroidsNew <- centroids[, -which(names(centroids)=='cluster')]
    if(k < nLabels){
        print('Error: k is less than labels')
        break
    }
    if(k > nLabels){
        i=nLabels
        while(i < k){
            dist <- rep(0, nrow(data))
            for(j in 1:nrow(centroidsNew)){
                #The origin algorithm is calculate the maximum probability of x. Due to the denominator is the sum
                #of all distance, here only choose the maximum distance.
                dist <- dist+calcDist(data, centroidsNew[j,])
            }
            centroidsNew <- rbind(centroidsNew, data[which.max(dist),])
            i <- i+1
        }
    }
    return(centroidsNew)
}

k_means <- function(labeledData, data, k, labelIndex, maxIteration=100, threshold=0.001){
    centroids <- ss_kpp_initC(labeledData=labeledData, data=data, k=k, labelIndex=labelIndex)
    data[,ncol(data)+1] <- 'unknow'
    data[,ncol(data)+1] <- Inf
    names(data)[ncol(data)-1] <- 'cluster'
    names(data)[ncol(data)] <- 'distance'
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
        diff <- sqrt(rowSums((centroidsNew[,names(centroidsNew)!='cluster']-centroids[,names(centroids)!='cluster'])^2))
        totalDiff <- sum(diff <= 0.001)
        print(sprintf('iterations: %d', iter))
        # dev.new()
        # plot(data$V1, data$V2, col=as.factor(data$cluster), pch=1:4, xlab='X', ylab='Y')
        # points(centroidsNew[,c('V1', 'V2')], pch=8, col='purple')
        # abline(h=0, v=0)
        iter = iter+1
    }
    return(data)
}
