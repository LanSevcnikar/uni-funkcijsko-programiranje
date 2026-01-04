import numpy as np

class DimensionalityReducer:
    """
    Handles dimensionality reduction using PCA (Principal Component Analysis).
    """
    def __init__(self):
        pass

    def reduce(self, data: np.ndarray, target_dim: int) -> np.ndarray:
        """
        Reduces the dimensionality of the input data to target_dim using PCA via SVD.
        
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

        # 1. Center the data (subtract mean)
        mean = np.mean(data, axis=0)
        centered_data = data - mean

        # 2. Compute SVD
        # U: (N, N), S: (min(N, D),), Vt: (D, D)
        # We want the top components from Vt (which are the principal axes)
        # Or we can just project using U * S
        # full_matrices=False makes U (N, K) and Vt (K, D) where K = min(N, D)
        try:
            U, S, Vt = np.linalg.svd(centered_data, full_matrices=False)
        except np.linalg.LinAlgError as e:
            print(f"SVD convergence failed: {e}")
            return np.zeros((data.shape[0], target_dim))

        # 3. Project data
        # The principal components are the rows of Vt.
        # We take the first 'target_dim' rows of Vt (top principal components).
        # Vt is (K, D), so Vt[:target_dim, :] is (target_dim, D).
        # Transpose to get (D, target_dim).
        components = Vt[:target_dim, :].T
        
        # Project centered data onto components
        # (N, D) dot (D, target_dim) -> (N, target_dim)
        reduced_data = np.dot(centered_data, components)
        
        return reduced_data
