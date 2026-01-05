import numpy as np
import matplotlib.pyplot as plt
from ripser import ripser

from SignalModule import SignalGenerator, Signal
from SlidingWindowModule import SlidingWindow

def plot_persistences_dim1_on_ax(data: np.ndarray, ax):
    if data.ndim != 2 or data.shape[0] != 2:
        raise ValueError("Input array must have shape (2, n)")

    births, deaths = data
    persistences = deaths - births

    try:
        longest_idx = np.argmax(persistences)
        longest_persistence = persistences[longest_idx]
    except ValueError:
        longest_idx = 0
        longest_persistence = 0

    y_positions = np.arange(len(births))

    for i, (b, d) in enumerate(zip(births, deaths)):
        color = "tab:blue" if i == longest_idx else "tab:orange"
        ax.hlines(y_positions[i], b, d, color=color, linewidth=2)

    ax.set_title("Persistence (H₁)")
    ax.set_xlabel("Value")
    ax.set_yticks([])

    return longest_persistence


def main(
    function_name="sin",
    function=lambda x: np.sin(x),
    start_time=0,
    end_time=20,
    step=0.05,
    noise=0.11,
    M=20,
    tau=0.31,
    method="pca"
):
    signal = SignalGenerator(
        start=start_time,
        end=end_time,
        step=step,
        function=function,
        noise=noise
    ).generate()

    slidingWindow = SlidingWindow(signal=signal, M=M, tau=tau)

    diagrams = ripser(
        slidingWindow.get_distance_matrix(),
        maxdim=1,
        distance_matrix=True
    )["dgms"][1].T  # H₁

    # ---------- Master figure ----------
    fig = plt.figure(figsize=(18, 12))
    gs = fig.add_gridspec(2, 2)

    ax_signal = fig.add_subplot(gs[0, :])
    ax_embedding = fig.add_subplot(gs[1, 0])
    ax_persistence = fig.add_subplot(gs[1, 1])

    # ---------- Draw ----------
    signal.plot_on_ax(
        ax_signal,
        title=f"{function_name}(t), noise={noise}"
    )

    slidingWindow.plot_on_ax(
        ax_embedding,
        d=2,
        method=method
    )

    longest_persistence = plot_persistences_dim1_on_ax(
        diagrams,
        ax_persistence
    )

    # ---------- Bottom text ----------
    fig.text(
        0.5,
        0.01,
        f"Longest persistence: {longest_persistence:.4f}",
        ha="center",
        fontsize=14,
        fontweight="bold"
    )


    # ---------- Super title ----------
    fig.suptitle(
        f"""
Signal → Sliding Window → Persistent Homology

function = {function_name}
time ∈ [{start_time}, {end_time}], step = {step}, noise = {noise}
M = {M}, τ = {tau}, reduction = {method.upper()}
""",
        fontsize=18
    )

    plt.tight_layout(rect=[0, 0, 1, 0.93])
    plt.show()

if __name__ == "__main__":
    main(
        function_name="sin",
        function=lambda x: x,
        start_time=0,
        end_time=20,
        step=0.02,
        noise=0.1,
        M=20,
        tau=0.31,
        method="pca"
    )
