import numpy as np
import pandas as pd
from collections import Counter

def createDataSet():
    group = np.mat([[1.0,1.1],[1.0,1.0],[0,0],[0,0.1]])
    labels = ['A','A','B','B']
    return group, labels

def knn(dataSet, knownData, k, labels):
    labels = np.array(labels)
    dist = np.power(np.sum(np.power((knownData - dataSet),2), axis=1), 0.5)
    dist = np.array(dist).ravel()
    o = dist.argsort()
    tmp = Counter(labels[o[:k]])
    label = sorted(tmp.items(), reverse=True)[0][0]
    return(label)

knn([0,0.1], group, 3, labels)
