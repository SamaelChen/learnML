library(hash)

data <- as.data.frame(V1=c(1,1,1,0,0), V2=c(1,1,0,1,1), V3=c('Y','Y','N','N','N'))
data[,1] <- as.character(data[,1])
data[,2] <- as.character(data[,2])
data[,3] <- as.character(data[,3])

calcEntropy <- function(dataSet, labelIndex){
    if(is.null(dim(dataSet))|nrow(dataSet)==1){
        return(0)
    }
    tmp <- aggregate(dataSet[,labelIndex], by=list(labels=dataSet[,labelIndex]), FUN='length')
    tmp$x <- tmp$x/sum(tmp$x)
    tmp[,3] <- -tmp$x*log(tmp$x,2)
    return(sum(tmp[,3]))
}

splitDataSet <- function(dataSet, index, value){
    return(dataSet[which(dataSet[,index]==value), -index])
}

chooseFeature <- function(dataSet){
    # there must exist one column used for labels
    numFeatures <- ncol(dataSet)-1
    baseEntropy <- calcEntropy(dataSet, ncol(dataSet))
    bestInfoGain <- 0
    bestGainRatio <- 0
    bestFeature <- -1
    for(i in 1:numFeatures){
        vals <- unique(dataSet[,i])
        newEntropy <- 0
        splitInfo <- 0
        for(j in vals){
            subDataSet <- dataSet[which(dataSet[,i]==j),]
            if(is.null(dim(subDataSet))){
                subDataSet <- as.data.frame(matrix(subDataSet, nrow=1, ncol=ncol(dataSet)))
            }
            # prob <- nrow(subDataSet)/nrow(dataSet)
            newEntropy <- newEntropy+calcEntropy(subDataSet,ncol(subDataSet))
            splitInfo <- splitInfo+(-abs(nrow(subDataSet)/nrow(dataSet))*log(abs(nrow(subDataSet)/nrow(dataSet)),2))
        }
        gain <- baseEntropy - newEntropy
        newGainRatio <- gain/splitInfo
        if(newGainRatio > bestGainRatio){
            bestGainRatio <- newGainRatio
            bestFeature <- i
        }
    }
    return(bestFeature)
}

createTree <- function(dataSet){
    if(is.null(ncol(dataSet))){
        return(dataSet[length(dataSet)])
    }
    if(nrow(dataSet)==1){
        return(dataSet[,ncol(dataSet)])
    }
    if(length(unique(dataSet[,ncol(dataSet)]))==1){
        return(dataSet[1,ncol(dataSet)])
    }
    bestFeature <- chooseFeature(dataSet)
    bestFeatureName <- names(dataSet)[bestFeature]
    myTree <- hash(bestFeatureName,hash())
    vals <- unique(dataSet[,bestFeature])
    for(v in vals){
        # subLabels <- names(dataSet)[-c(bestFeature, ncol(dataSet))]
        print(splitDataSet(dataSet,bestFeature,v))
        myTree[[bestFeatureName]][[v]] <- createTree(dataSet=splitDataSet(dataSet,bestFeature,v))
    }
    return(myTree)
}

createTree(data)
