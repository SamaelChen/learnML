attach(freeny)

linearReg <- function(dataSet, targetIndex){
    X <- dataSet[,-targetIndex]
    X <- cbind(X, intercept=1)
    X <- as.matrix(X)
    w <- solve(t(X) %*% X) %*% t(X) %*% dataSet[,targetIndex]
    return(w)
}

linearReg(freeny, 1)
