from numpy import *
import matplotlib
import matplotlib.pyplot as plt
import pylab

def loadDataSet(file):
    dataMat=[]
    fr=open(file)
    for line in fr.readlines():
        curLine=line.strip().split('\t')
        fltLine=list(map(float,curLine))
        dataMat.append(fltLine)
    return(dataMat)

def distEclud(vecA, vecB):
    return(sqrt(sum(power(vecA-vecB, 2))))

def randCent(dataSet, k, randomSeed=12345):
    n=shape(dataSet)[1]
    centroids=mat(zeros((k,n)))
    random.seed(randomSeed)
    for j in range(n):
        minJ=min(dataSet[:,j])
        rangeJ=float(max(dataSet[:,j])-minJ)
        centroids[:,j]=minJ+rangeJ*random.rand(k,1)
    return(centroids)

def KMeans(dataSet, k, randomSeed=12345, maxIteration=100):
    m=shape(dataSet)[0]
    clusterAssment=mat(zeros((m,2)))
    centroids=randCent(dataSet, k, randomSeed)
    clusterChanged=True
    iterations=1
    while(iterations<=maxIteration and clusterChanged):
        clusterChanged=False
        for i in range(m):
            minDist=inf; minIndex=-1
            for j in range(k):
                distJI=distEclud(centroids[j,:], dataSet[i,:])
                if distJI < minDist:
                    minDist=distJI; minIndex=j
            if clusterAssment[i,0] != minIndex:
                clusterChanged=True
            clusterAssment[i,:]=minIndex,minDist**2
        for cent in range(k):
            ptsInClust=dataSet[nonzero(clusterAssment[:,0].A==cent)[0]]
            centroids[cent,:]=mean(ptsInClust, axis=0)
        iterations = iterations+1
    return(centroids, clusterAssment, iterations)

dataMat=mat(loadDataSet('/media/samael/Samael/python/machinelearninginaction/Ch10/testSet.txt'))
