import numpy as np
import matplotlib.pyplot as plt
from typing import List, Tuple
from SignalModule import Signal, SignalGenerator
from DimensionalityReductionModule import DimensionalityReducer
import math

class SlidingWindow:
    """
    Performs sliding window embedding on a signal.
    """
    def __init__(self, signal: Signal, M: int, tau: float):
        """
        Args:
            signal: The input Signal object.
            M: Embedding dimension (number of delays).
            tau: Time delay.
        """
        self.signal = signal
        self.M = M
        self.tau = tau
        self.windowed_data: List[Tuple[float, np.ndarray, Tuple[float, float, float]]] = []
        
        # Automatically process
        self._process()

    def _process(self):
        """
        Generates the embedding vectors.
        """
        if not self.signal.data:
            return

        # Extract times from signal data
        times = self.signal.get_times()
        values = self.signal.get_values()
        colors = self.signal.get_colors()
        
        # We need to find vectors for each time t in the signal
        # Vector at t is [x(t), x(t+tau), ..., x(t+(M-1)tau)]
        # We stop when t + (M-1)tau > max_time
        
        max_time = times[-1]
        duration = (self.M - 1) * self.tau
        
        valid_indices = times <= (max_time - duration)
        valid_times = times[valid_indices]
        
        # We can vectorize the lookup
        # Create a matrix of query times
        # Shape (N_valid, M)
        # Row i: [t_i, t_i+tau, ..., t_i+(M-1)tau]
        
        delays = np.arange(self.M) * self.tau
        query_times_matrix = valid_times[:, np.newaxis] + delays[np.newaxis, :]
        
        # Flatten to query all at once
        flat_query_times = query_times_matrix.flatten()
        times_indecies = np.searchsorted(times, flat_query_times)
        flat_values = values[times_indecies]
        
        # Reshape back to (N_valid, M)
        vectors = flat_values.reshape(len(valid_times), self.M)
        
        # Store result
        # We also need to store colors corresponding to valid_times
        # valid_indices is a boolean mask, so we can use it to slice colors list if we convert to array,
        # or just zip.
        
        # Let's just iterate to store in the list format requested
        # "final array is the array of trouples time, vector, colour"
        # THIS COULD BE DONE FASTER
        self.windowed_data = []
        for i, t in enumerate(valid_times):
            self.windowed_data.append((t, vectors[i], colors[i]))
            
    def print_first_n(self, n: int = 5):
        """
        Prints the first n points in the windowed data.
        Args:
            n: Number of points to print.
        """
        if not self.windowed_data:
            print("No data to print.")
            return
        
        for i in range(min(n, len(self.windowed_data))):
            print(f"Point {i}: {self.windowed_data[i]}")

    def plot(self, d: int = 2, method: str = "pca"):
        fig, ax = plt.subplots(figsize=(10, 8))
        self.plot_on_ax(ax, d=d, method=method)
        plt.show()


    def plot_on_ax(self, ax, d: int = 2, method: str = "pca"):
        if not self.windowed_data:
            raise ValueError("No data to plot.")

        vectors = np.array([v for _, v, _ in self.windowed_data])
        colors = [c for _, _, c in self.windowed_data]

        reducer = DimensionalityReducer(method=method)
        reduced = reducer.reduce(vectors, target_dim=d)

        if d != 2:
            raise ValueError("Only d=2 supported for combined plot")

        ax.scatter(
            reduced[:, 0],
            reduced[:, 1],
            c=colors,
            s=20
        )
        ax.set_title("Sliding Window Embedding")
        ax.set_xlabel("Component 1")
        ax.set_ylabel("Component 2")
        ax.grid(True, alpha=0.3)

    def downscale_sp_space(self, dimension: int | None = None, method: str = "pca"):
        if dimension is None:
            return
            
        reducer = DimensionalityReducer(method=method)
        wd_time = np.array([w[0] for w in self.windowed_data])
        wd_vector = np.array([w[1] for w in self.windowed_data])
        wd_color = np.array([w[2] for w in self.windowed_data])
        
        reduced = reducer.reduce(wd_vector, target_dim=dimension)

        self.windowed_data = list(zip(wd_time, reduced, wd_color))

    def get_distance_matrix(self):
        # Stack all vectors into a single (N, D) array
        X = np.stack([w[1] for w in self.windowed_data])  # shape: (N, D)

        # Compute pairwise differences using broadcasting
        diff = X[:, None, :] - X[None, :, :]              # shape: (N, N, D)

        # Euclidean distance
        matrix = np.linalg.norm(diff, axis=-1)            # shape: (N, N)

        return matrix


if __name__ == "__main__":
    print("Verifying SlidingWindowModule...")
    
    # 1. Generate a Sine Wave
    # x(t) = sin(t)
    # Embedding of sine wave with M=2, tau=pi/2 should be a circle.
    
    def sine_wave(t):
        return math.sin(t)
        
    gen = SignalGenerator(start=0, end=20, step=0.05, noise_level=0.0, generator_function=sine_wave)
    sig = gen.generate()
    
    print(f"Generated signal with {len(sig.data)} points.")
    
    # 2. Create Sliding Window
    # M=3, tau=1.0
    sw = SlidingWindow(sig, M=3, tau=1.0)
    
    print(f"Created sliding window with {len(sw.windowed_data)} points.")
    
    if sw.windowed_data:
        print(f"First vector at t={sw.windowed_data[0][0]}: {sw.windowed_data[0][1]}")
    
    # 3. Plot 2D
    print("Plotting 2D projection...")
    sw.plot(d=2)
    
    # 4. Plot 3D
    print("Plotting 3D projection...")
    sw.plot(d=3)
