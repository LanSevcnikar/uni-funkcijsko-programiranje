from SignalModule import SignalGenerator, Signal
from SlidingWindowModule import SlidingWindow
from pyRiser import Ripser


from ripser import ripser
from persim import plot_diagrams

import numpy as np

def main(
    function = lambda x: np.sin(x),
    start_time = 0,
    end_time = 20,
    step = 0.05,
    noise = 0.11,
    M = 20,
    tau = 0.31,
)