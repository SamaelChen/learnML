attach(freeny)

linearReg <- function(dataSet, targetIndex, keepConstant=T){
    X <- dataSet[,-targetIndex]
    if(keepConstant==T){
        X <- cbind(X, intercept=1)
    }
    X <- as.matrix(X)
    w <- solve(t(X) %*% X) %*% t(X) %*% dataSet[,targetIndex]
    return(w)
}

linearReg(freeny, 1)
