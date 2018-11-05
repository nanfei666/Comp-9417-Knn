import numpy as np
import operator


def file2Matrix(filename):
    fr = open (filename)
    arryOline = fr.readlines()
    numberOfLine = len(arryOline)
    Matrix = np.zeros((numberOfLine,34))
    classLable = []
    index = 0
    for line in arryOline:
        line = line.strip('\n')
        listFormLine = line.split(',')
        #line = line.strip(' ')
        
        #listFormLine = line.split()        
        #print(listFormLine)
        Matrix[index,:] = listFormLine[0:34]
        if listFormLine[-1] =='g':
            classLable.append(1)
        elif listFormLine[-1] =='b':
            classLable.append(2)
        index += 1 
        # if listFormLine[-1] =='didntLike':
        #     classLable.append(1)
        # elif listFormLine[-1] =='smallDoses':
        #     classLable.append(2)
        # elif listFormLine[-1] =='largeDoses':
        #     classLable.append(3)
        # index += 1 
        
        
    return Matrix, classLable

def Nomorlize(dataset):
    MinValue = dataset.min(0)
    MaxValue = dataset.max(0)
    ranges = MaxValue- MinValue
    Nomorlized_data = np.zeros(np.shape(dataset))
    m = dataset.shape[0]
    Nomorlized_data1 = dataset - np.tile(MinValue,(m,1))
    Nomorlized_data = Nomorlized_data1/ np.tile(ranges,(m,1))
    return Nomorlized_data 

def gaussian(dist,sigma=10.0):
    """Input a distance and return it's weight"""
    weight = np.exp(-dist**2/(2*sigma**2))
    return weight

def classify(Inx, dataset, lables, k):
    dataset_size = dataset.shape[0]
    diffMat  = np.tile(Inx,(dataset_size,1))- dataset
    sqDiffMat = diffMat**2
    sqDistance = sqDiffMat.sum(axis=1)
    Distance = sqDistance ** 0.5
    sortedDistIndiex = Distance.argsort()
    classcount={}
    
    for i in range(k):
        votelable = lables[sortedDistIndiex[i]]
        weight = gaussian(Distance[sortedDistIndiex[i]])
        #print(weight)
        classcount[votelable] = classcount.get(votelable,0)+weight
    print(classcount)

    sortedClasscount = sorted(classcount.items(),key=operator.itemgetter(1),reverse=True)
    return  sortedClasscount[0][0]   

def my_KFold(n,n_folds,shuffe=False):
    """
    K-Folds cross validation iterator.
    Provides train/test indices to split data in train test sets. Split dataset 
    into k consecutive folds (without shuffling by default).
    Each fold is then used a validation set once while the k - 1 remaining fold form the training set.
    Example:
    ------
    X = np.array([[1, 2], [3, 4], [1, 2], [3, 4]])
    y = np.array([1, 2, 3, 4])
    kf = KFold(4, n_folds=2)
    for train_index, test_index in kf:
        X_train, X_test = X[train_index], X[test_index]
        y_train, y_test = y[train_index], y[test_index]
        print("TRAIN:", train_index, "TEST:", test_index)
    TRAIN: [2 3] TEST: [0 1]
    TRAIN: [0 1] TEST: [2 3]
    """
    idx = np.arange(n)
    if shuffe:
        idx = np.random.permutation(idx)
    fold_sizes = (n // n_folds) * np.ones(n_folds, dtype=np.int) # folds have size n // n_folds
    fold_sizes[:n % n_folds] += 1 # The first n % n_folds folds have size n // n_folds + 1
    current = 0
    for fold_size in fold_sizes:
        start, stop = current, current + fold_size
        train_index = list(np.concatenate((idx[:start], idx[stop:])))
        test_index = list(idx[start:stop])
        yield train_index, test_index
        current = stop # move one step forward

if __name__ == '__main__':
    #打开的文件名
    filename = "ionosphere.txt"
    #打开并处理数据
    DataMat, data_Labels = file2Matrix(filename)
    data_Labels = np.array(data_Labels)
    #Nomorlized_data = Nomorlize(datingDataMat)
    #DataMat = Nomorlize(DataMat)
    number_of_Data = len(DataMat)
    errorCount = 0.0
    kfolds = my_KFold(number_of_Data,number_of_Data)
    k_range = range(1,10)
    error_rate = []
    
    for train_index, test_index in kfolds:
        X_train, X_test = DataMat[train_index], DataMat[test_index]
        
        classify_Result = classify(X_test,X_train,data_Labels[train_index],2)
        #print(classify_Result,  data_Labels[test_index])
        if classify_Result != data_Labels[test_index]:
            errorCount += 1.0
        #error_rate.append(format(errorCount/float(len(DataMat[train_index])),'.0%'))
        #errorCount = 0.0
    rate = errorCount/float(len(DataMat[train_index]))
    print("错误率:%f%%"%(errorCount/float(len(DataMat[train_index]))*100))

            
    
    
   
    
    
    
