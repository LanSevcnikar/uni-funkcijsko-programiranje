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

    def plot(self, d: int = 2):
        """
        Plots the embedded signal reduced to d dimensions.
        Args:
            d: Target dimension (2 or 3).
        """
        if not self.windowed_data:
            print("No data to plot.")
            return

        # Extract vectors for reduction
        vectors = np.array([v for _, v, _ in self.windowed_data])
        colors = [c for _, _, c in self.windowed_data]
        
        # Reduce dimensionality
        reducer = DimensionalityReducer()
        reduced_vectors = reducer.reduce(vectors, d)
        
        if reduced_vectors.size == 0:
            print("Reduction failed or returned empty.")
            return

        # Plot
        fig = plt.figure(figsize=(10, 8))
        
        if d == 2:
            plt.scatter(reduced_vectors[:, 0], reduced_vectors[:, 1], c=colors, s=20, edgecolors='none')
            plt.xlabel("Component 1")
            plt.ylabel("Component 2")
            plt.title(f"Sliding Window Embedding (M={self.M}, tau={self.tau}) -> 2D PCA")
            
        elif d == 3:
            ax = fig.add_subplot(111, projection='3d')
            ax.scatter(reduced_vectors[:, 0], reduced_vectors[:, 1], reduced_vectors[:, 2], c=colors, s=20, edgecolors='none')
            ax.set_xlabel("Component 1")
            ax.set_ylabel("Component 2")
            ax.set_zlabel("Component 3")
            plt.title(f"Sliding Window Embedding (M={self.M}, tau={self.tau}) -> 3D PCA")
            
        else:
            print(f"Plotting for d={d} is not supported (only 2 or 3).")
            return
            
        plt.grid(True, alpha=0.3)
        plt.show()

    def get_distance_matrix(self):
        """
            NOTE
            1. This code is super unoptimized
            2. The diagonals being set to 0 is unnecessary, it should be that already
            I do not feel like it atm
        """
        matrix = np.zeros((len(self.windowed_data), len(self.windowed_data)))
        for i in range(len(self.windowed_data)):
            for j in range(len(self.windowed_data)):
                matrix[i][j] = np.linalg.norm(self.windowed_data[i][1] - self.windowed_data[j][1])
        for i in range(len(matrix)):
            matrix[i][i] = 0
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
