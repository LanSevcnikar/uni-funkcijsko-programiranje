import numpy as np
from sklearn.decomposition import PCA, TruncatedSVD
from sklearn.manifold import TSNE

class DimensionalityReducer:
    """
    Handles dimensionality reduction using various methods:
    - PCA (Principal Component Analysis)
    - SVD (Singular Value Decomposition)
    - t-SNE (t-Distributed Stochastic Neighbor Embedding)
    """
    def __init__(self, method: str = 'PCA'):
        """
        Initialize the reducer with a specific method.
        
        Args:
            method: One of 'PCA', 'SVD', 't-distribution' (or 't-SNE').
                    Defaults to 'PCA'.
        """
        self.method = method

    def reduce(self, data: np.ndarray, target_dim: int) -> np.ndarray:
        """
        Reduces the dimensionality of the input data to target_dim.
        
        Args:
            data: Numpy array of shape (N, D) where N is number of samples and D is original dimension.
            target_dim: Integer, the desired output dimension.
            
        Returns:
            Numpy array of shape (N, target_dim).
        """
        if data.size == 0:
            return np.array([])
            
        if target_dim >= data.shape[1]:
            print(f"Target dimension {target_dim} >= input dimension {data.shape[1]}. Returning original data.")
            return data

        method_key = self.method.lower()

        if method_key == 'pca':
            reducer = PCA(n_components=target_dim)
            return reducer.fit_transform(data)
            
        elif method_key == 'svd':
            # TruncatedSVD is preferred for sparse matrices, but works for dense too.
            reducer = TruncatedSVD(n_components=target_dim)
            return reducer.fit_transform(data)
            
        elif method_key in ['t-distribution', 't-sne']:
            # t-SNE usually is for 2 or 3 dimensions.
            # method='exact' is slower but more accurate, 'barnes_hut' is default (O(NlogN))
            reducer = TSNE(n_components=target_dim, init='random', learning_rate='auto')
            return reducer.fit_transform(data)
            
        else:
            raise ValueError(f"Unknown dimensionality reduction method: {self.method}")
