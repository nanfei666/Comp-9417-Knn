import numpy as np
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
X1 = np.array([[1, 2], [3, 4], [1, 2], [3, 4]])
kf = my_KFold(4, 4)
for train_index, test_index in kf:
    X_train, X_test = X1[train_index], X1[test_index]
    print("TRAIN:", train_index, "TEST:", test_index)