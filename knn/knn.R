data <- read.table('/media/samael/Samael/learnML/knn/datingTestSet.txt', header=F, sep='\t')

# 'knownData' is the dataset I've already known classification. However, it just contain its coordianates.
# labels is the actual labels of 'knownData', that means it should pair knownData exactly.
knn <- function(dataSet, knownData, k, labels){
    dataSet <- as.data.frame(dataSet)
    dataSet[, ncol(dataSet)+1] <- ''
    for(i in 1:nrow(dataSet)){
        dist=sqrt(rowSums((knownData - as.numeric(dataSet[i,1:(ncol(dataSet)-1)]))**2))
        o <- order(dist, decreasing=F)
        dataSet[i, ncol(dataSet)] <- names(sort(summary(as.factor(labels[o][1:k])), decreasing=T)[1])
    }
    return(dataSet)
}

# debug, a <- as.data.frame(c(5,2,1,0), c(7,2,7,0))
knn(dataSet=a, knownData=iris[,1:4], k=5, labels=iris[,5])
