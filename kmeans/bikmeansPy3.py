import numpy as np
import pandas as pd
import ggplot
import math

def calcDistance(matrix, vector):
    return(np.sqrt(np.sum(np.power(matrix-vector,2), 1)))

def initCentroids(dataSet, k):
    nrow, ncol = dataSet.shape
    centroids = pd.DataFrame(index=range(0,k), columns=dataSet.columns)
    centroids = centroids.fillna(0)
    centroids.iloc[0,:] = dataSet.iloc[0,:]
    for i in range(k - 1):
        dist = list()
        j = 0
        while j <= i:
            distance = calcDistance(dataSet, centroids.iloc[j,:])
            dist.append(distance)
            j += 1
        dist = sum(dist)
        index = dist.idxmax()
        centroids.iloc[i+1, :] = dataSet.loc[index]
    return(centroids)

def kmeans(dataSet, k, iterations=100, thresold=0.001):
    nrow, ncol = dataSet.shape
    clusterAss = pd.DataFrame(index=dataSet.index, columns=['cluster', 'distance'])
    clusterAss.iloc[:,0] = clusterAss.iloc[:,0].fillna(0)
    clusterAss.iloc[:,1] = clusterAss.iloc[:,1].fillna(math.inf)
    maxThresold = math.inf
    iterationCnt = 1
    centroids = initCentroids(dataSet, k)
    newCentroids = centroids
    while((iterationCnt < iterations) & (maxThresold > thresold)):
        centroids = newCentroids
        for i in range(k):
            dist = calcDistance(dataSet, centroids.iloc[i,:])
            flag = clusterAss.iloc[:,1][clusterAss.iloc[:,1] > dist].index
            clusterAss.loc[flag, 'distance'] = dist[flag]
            if(iterationCnt == 1):
                clusterAss.iloc[:, 1][clusterAss.iloc[:,1]==0] = math.inf
            clusterAss.loc[flag, 'cluster'] = i
        temp = pd.concat([dataSet, clusterAss], axis=1, ignore_index=True)
        newCentroids = temp.groupby(2).mean().iloc[:,0:2]
        newCentroids.index = range(k)
        newCentroids.columns = centroids.columns
        maxThresold = np.max(np.sum((centroids - newCentroids) ** 2, axis=1))
        print('Iteration: ' + str(iterationCnt))
        iterationCnt += 1
    print('Just finished!')
    return(clusterAss, centroids)

def biKmeans(dataSet, k):
    nrow = dataSet.shape[0]
    centroid = dataSet.mean()
    centroids = [centroid]
    clusterAss = pd.DataFrame(index=dataSet.index, columns=['cluster', 'distance'])
    clusterAss.iloc[:,0] = clusterAss.iloc[:,0].fillna(0)
    clusterAss.iloc[:,1] = calcDistance(dataSet, centroid)
    minSSE = math.inf
    while(len(centroids) < k):
        numCurrCluster = len(centroids)
        for i in range(numCurrCluster):
            pointsInCurrCluster = dataSet.iloc[clusterAss.iloc[:,0][clusterAss.iloc[:,0] == i].index,:]
            splitClusterAss, tmpCent = kmeans(pointsInCurrCluster, 2)
            splitSSE = sum(splitClusterAss.iloc[:,1])
            notSplitSSE = sum(clusterAss.iloc[clusterAss.iloc[:,0][clusterAss.iloc[:,0] != i].index,1])
            currentSSE = splitSSE + notSplitSSE
            if(currentSSE < minSSE):
                minSSE = currentSSE
                bestClusterToSplit = i
                bestNewCentroids = tmpCent.copy()
                bestClusterAss = splitClusterAss.copy()
        bestClusterAss.loc[bestClusterAss.loc[:,'cluster']==1, 'cluster'] = numCurrCluster
        bestClusterAss.loc[bestClusterAss.loc[:,'cluster']==0, 'cluster'] = bestClusterToSplit
        centroids[bestClusterToSplit] = bestNewCentroids.iloc[0, :]
        centroids.append(bestNewCentroids.iloc[1, :])
        clusterAss.iloc[clusterAss.iloc[:,0][clusterAss.iloc[:,0]==bestClusterToSplit].index,:] = bestClusterAss
    print('Just finished')
    return(clusterAss, pd.DataFrame(centroids))

data = pd.read_table('/home/samael/learnML/kmeans/kmeans', sep='\t', header=None)
data.columns = ['V1', 'V2']
clusterAss1, centroids1 = kmeans(data, 4)
clusterAss2, centroids2 = biKmeans(data, 4)
ggplot.ggplot(data, ggplot.aes('V1', 'V2')) + ggplot.geom_point(color=clusterAss1.cluster, size=50) + ggplot.geom_hline(y=0) + ggplot.geom_vline(x=0)
ggplot.ggplot(data, ggplot.aes('V1', 'V2')) + ggplot.geom_point(color=clusterAss2.cluster, size=50) + ggplot.geom_hline(y=0) + ggplot.geom_vline(x=0)
