import math
import random
import matplotlib.pyplot as plt
import colorsys
import numpy as np
from typing import Callable, List, Tuple, Optional, Union

class Signal:
    """
    Represents a generated signal with data and operations to visualize/save it.
    """
    def __init__(self, data: List[Tuple[float, float, Tuple[float, float, float]]]):
        self.data = data
        # Cache numpy arrays for efficient interpolation
        if data:
            self._times = np.array([t for t, _, _ in data])
            self._values = np.array([v for _, v, _ in data])
            self._colors = np.array([c for _, _, c in data])
        else:
            self._times = np.array([])
            self._values = np.array([])
            self._colors = np.array([])

    def normalize(self, number_of_points: int | None = None):
        # this form of norm keeps furier transform properties
        if number_of_points is None:
            number_of_points = len(self.data)

        total_number_of_points = self._values.size
        step_length = total_number_of_points // number_of_points
        
        if total_number_of_points < number_of_points:
            raise ValueError("Number of points is greater than the total number of points in the signal.")
        
        subsampled_times = self._times[::step_length][:number_of_points]
        subsampled_values = self._values[::step_length][:number_of_points]
        subsampled_colors = self._colors[::step_length][:number_of_points]

        times_mean = np.mean(subsampled_times)
        times_std = np.std(subsampled_times)
        subsampled_times = (subsampled_times - times_mean) / times_std

        values_mean = np.mean(subsampled_values)
        values_std = np.std(subsampled_values)
        subsampled_values = (subsampled_values - values_mean) / values_std

        self.data = list(zip(subsampled_times, subsampled_values, subsampled_colors))
        self._times = subsampled_times
        self._values = subsampled_values
        self._colors = subsampled_colors

    def get_times(self):
        return self._times

    def get_values(self):
        return self._values

    def get_colors(self):
        return self._colors

    def plot(self, title: str = "Signal"):
        fig, ax = plt.subplots(figsize=(10, 6))
        self.plot_on_ax(ax, title)
        plt.show()

    def plot_on_ax(self, ax, title: str = "Signal"):
        if self.data is None or len(self.data) == 0:
            raise ValueError("No data to plot.")

        ax.plot(
            self._times,
            self._values,
            c="tab:gray",
            alpha=0.5,
            linewidth=0.5
        )
        ax.scatter(
            self._times,
            self._values,
            c=self._colors,
            s=40
        )
        ax.set_title(title)
        ax.set_xlabel("Time")
        ax.set_ylabel("Value")
        ax.grid(True, alpha=0.3)

    def save_to_txt(self, filename: str):
        """
        Saves the signal data to a text file.
        Format: t, value, r, g, b
        """
        with open(filename, 'w') as f:
            f.write("t,value,r,g,b\n")
            for t, val, (r, g, b) in self.data:
                f.write(f"{t:.6f},{val:.6f},{r:.6f},{g:.6f},{b:.6f}\n")
        print(f"Saved data to {filename}")

    @classmethod
    def from_file(cls, filename: str) -> 'Signal':
        """
        Reads signal data from a text file and returns a Signal instance.
        """
        data = []
        try:
            with open(filename, 'r') as f:
                # Skip header
                next(f)
                for line in f:
                    parts = line.strip().split(',')
                    if len(parts) == 5:
                        t = float(parts[0])
                        val = float(parts[1])
                        r = float(parts[2])
                        g = float(parts[3])
                        b = float(parts[4])
                        data.append((t, val, (r, g, b)))
            print(f"Read {len(data)} points from {filename}")
            return cls(data)
        except FileNotFoundError:
            print(f"File {filename} not found.")
            return cls([])
        except Exception as e:
            print(f"Error reading file: {e}")
            return cls([])


class SignalGenerator:
    """
    Factory class to configure and generate Signal instances.
    """
    def __init__(self, 
                 start: float, 
                 end: float, 
                 step: float, 
                 noise: float, 
                 function: Callable[[float], float]):
        self.start = start
        self.end = end
        self.step = step
        self.noise = noise
        self.function = function

    def generate(self) -> Signal:
        """
        Generates the signal data and returns a Signal object.
        """
        data = []
        t = self.start
        if self.step <= 0:
            raise ValueError("Step must be positive")

        while t <= self.end + 1e-9:
            # Calculate base value
            base_val = self.function(t)
            
            # Add noise
            noise = random.uniform(-self.noise, self.noise)
            val = base_val + noise
            
            # Calculate color
            # Avoid really bright colors by limiting Value/Brightness in HSV
            # Cycle hue based on time for continuous shift
            hue = (t * 0.1) % 1.0 
            saturation = 0.8  # Slightly desaturated
            value = 0.8       # Not too bright (max is 1.0)
            color = colorsys.hsv_to_rgb(hue, saturation, value)
            
            data.append((t, val, color))
            t += self.step
            
        return Signal(data)


if __name__ == "__main__":
    # Verification
    print("Verifying Signal and SignalGenerator...")

    # Define a custom generator function
    def my_func(t):
        return math.sin(t) + 0.1 * t

    # Create generator
    gen = SignalGenerator(
        start=0, 
        end=10, 
        step=0.1, 
        noise_level=0.2, 
        generator_function=my_func
    )

    # Generate Signal
    sig = gen.generate()

    # Plot
    print("Plotting signal...")
    sig.plot("My Custom Signal")

    # Save to file
    filename = "test_signal_split.txt"
    sig.save_to_txt(filename)

    # Read from file
    loaded_sig = Signal.from_file(filename)
    
    # Verify read data
    if loaded_sig.data:
        print(f"Successfully loaded {len(loaded_sig.data)} points.")
        print(f"First point: {loaded_sig.data[0]}")
        
        # Optional: Plot loaded data to verify
        loaded_sig.plot("Loaded Signal Data")
