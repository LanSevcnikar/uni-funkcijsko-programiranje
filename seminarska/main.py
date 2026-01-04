# %%
from SignalModule import SignalGenerator, Signal
from SlidingWindowModule import SlidingWindow
from pyRiser import Ripser

import numpy as np

# %%
# sin function, no noise, from 0 to 5 with step 0.1
function = lambda x: np.sin(x)
start_time = 0
end_time = 20
step = 0.2
noise = 0

signalGenerator = SignalGenerator(
    start=start_time,
    end=end_time,
    step=step,
    function=function,
    noise=noise,
)
signal = signalGenerator.generate()


# %%
signal.plot()

# %%
M = 10
tau = 0.1

slidingWindow = SlidingWindow(
    signal = signal,
    M = M,
    tau = tau
)
slidingWindow.print_first_n(5)
slidingWindow.plot(d=3)

    
    


# %%
# Might be smart to do this later 
# https://github.com/Ripser/ripser?tab=readme-ov-file
# but not right now

# %%
ripser = Ripser("/home/lan-sevcnikar/Documents/university/funkcijsko programiranje/seminarska/ripser/ripser", dim=1)

# %%
ripser.compute_pd(slidingWindow.get_distance_matrix())


