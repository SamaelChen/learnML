import math
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import pylab
import pandas as pd

def distEclud(matA, vecB):
    return(np.sqrt(np.sum(np.power(matA-vecB, 2), axis=1)))

def initCent(dataSet, k):
    dataSet=np.mat(dataSet)
    n=np.shape(dataSet)[1]
    dim=(k,n)
    centroids=np.mat(np.zeros(dim))
    centroids[0,]=dataSet[0,]
    for i in range(k-1):
        dist=list()
        for j in range(np.shape(dataSet)[0]):
            distance=np.sum(distEclud(centroids[0:(i+1),], dataSet[j,]))
            dist.append(distance)
        index=dist.index(max(dist))
        centroids[i+1,]=dataSet[index,]
        dataSet=np.delete(dataSet, index, axis=0)
    return(centroids)

def kmeans(dataSet, k, maxIteration=100, threshold=0.001):
    m=np.shape(dataSet)[0]
    n=np.shape(dataSet)[1]
    centroids=initCent(dataSet=dataSet, k=k)
    centroids=np.append(centroids, np.transpose(np.mat(range(1,(k+1)))), axis=1)
    dataSet=np.append(dataSet, np.transpose(np.mat(np.zeros(m))), axis=1)
    dataSet=np.append(dataSet, np.transpose(np.mat(np.repeat(math.inf,m))), axis=1)
    iteration=1
    maxThreshold=math.inf
    newCentroids=centroids
    while((iteration <= maxIteration) & (maxThreshold > threshold)):
        centroids=newCentroids
        for i in range(k):
            dist=distEclud(dataSet[:,0:n], centroids[i,0:n])
            select=np.where(dataSet[:,n+1] > dist)[0]
            dataSet[select,n+1] = np.array(dist).ravel()[select]
            if(i==0):
                dataSet[np.where(dataSet[:,n+1]==0)[0],n+1]=math.inf
            dataSet[:,n][select]=i+1
        newCentroids=np.append(np.mat(pd.DataFrame(dataSet[:,0:n+1]).groupby(n).mean()), np.transpose(np.mat(pd.DataFrame(dataSet).groupby(n).mean().index)), axis=1)
        maxThreshold=np.max(np.power(np.sum(np.power(centroids[:,0:n]-newCentroids[:,0:n],2), axis=1), 0.5))
        print('Iteration: '+str(iteration))
        iteration+=1
    return(dataSet)

df=pd.read_csv('/home/samael/python/kmeans/kmeans', sep='\t', header=None)
