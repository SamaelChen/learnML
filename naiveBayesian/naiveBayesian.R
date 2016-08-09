library(reshape2)
library(plyr)

data <-data.frame(
    size=c("大","小","大","大","小","小"),
    weight=c("轻","重","轻","轻","重","轻"),
    color=c("红","红","红","绿","红","绿"),
    taste=c("good","good","bad","bad","bad","good")
)

# The observation data should in this type
obs<-data.frame(
    featureName=c("size", "weight", "color"),
    featureVal=c("大","重","红")
)

# Calculate the probability of each class.
classProb <- function(dataSet, varName){
    tmp <- ddply(dataSet, varName, 'nrow')
    # Here we use Laplacian correction, and use log-likelihood
    tmp <- ddply(tmp, varName, mutate, prob=log((nrow+1)/(nrow(dataSet)+length(unique(dataSet[,varName]))), 10))
    colnames(tmp) <- c('className', 'classFreq', 'classProb')
    return(tmp[,-2])
}

featureProb <- function(dataSet, varName){
    dataSet.melt <- melt(dataSet, id=c(varName))
    tmpa <- ddply(dataSet.melt, c(varName, 'variable', 'value'), 'nrow')
    tmpb <- ddply(dataSet.melt, c('variable', 'value'), 'nrow')
    tmpb <- ddply(tmpb, 'variable', 'nrow')
    colnames(tmpb)[2] <- 'freq'
    tmpa <- join(tmpa, tmpb, by='variable')
    tmpc <- ddply(tmpa, c(varName, 'variable'), mutate, prob=log((nrow+1)/(sum(nrow)+freq), 10))
    colnames(tmpc) <- c('className', 'featureName', 'featureVal', 'featureNrow', 'featureFreq', 'featureProb')
    return(tmpc[, c(1,2,3,6)])
}

NBClassify <- function(obs, trainData, varName){
    cp <- classProb(trainData, varName)
    fp <- featureProb(trainData, varName)
    allProb <- join(fp, obs, by=c('featureName', 'featureVal'), type='inner')
    allProb <- ddply(allProb, 'className', summarize, feat_prob=sum(featureProb))
    cProb <- join(allProb, cp, by=c('className'), type='inner')
    cProb <- ddply(cProb, 'className', summarize, class_prob=feat_prob+classProb)
    return(as.character(cProb[which.max(cProb$class_prob),'className']))
}
