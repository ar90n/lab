from dataclasses import dataclass
import sys
from pathlib import Path

import pickle
import pandas as pd
import numpy as np
from scipy.optimize import minimize


@dataclass
class FOPTDParams:
    K: float = 1
    tau: float = 1
    tau_d: float = 0


def calc_step_response(measures: np.ndarray) -> np.ndarray:
    forward_measures = measures[2:, :] 
    backward_measures = measures[:-2, :]
    delta_angles = np.angle(np.exp(1j * np.pi * forward_measures[:, 1]) * np.exp(-1j * np.pi * backward_measures[:, 1]))
    delta_time = (forward_measures[:, 0] - backward_measures[:, 0]) / 1000.0
    dtheta_dt = delta_angles / (delta_time + 1e-12)
    measure_time_sec = measures[1:-1, 0] / 1000.0
    import pdb; pdb.set_trace()
    return np.vstack([measure_time_sec, dtheta_dt]).T


def foptd(t, K=1, tau=1, tau_d=0):
    tau_d = max(0, tau_d)
    tau = max(0, tau)
    return np.array(
        [K * (1 - np.exp(-(t - tau_d) / tau)) if t >= tau_d else 0 for t in t]
    )


def estimate_foptd_params(
    step_response: np.ndarray, input_amplitude: float
) -> FOPTDParams:
    def err(X, t, y):
        K, tau, tau_d = X
        z = foptd(t, K, tau, tau_d)
        iae = sum(abs(z - y)) * (max(t) - min(t)) / len(t)
        return iae

    ts = step_response[:, 0]
    ys = step_response[:, 1] / input_amplitude
    X = [0.10 / input_amplitude, 0.1, ts[0]]
    K, tau, tau_d = minimize(err, X, args=(ts, ys)).x

    return FOPTDParams(K, tau, tau_d)


def estimate_foptd_task(csv_path: Path, input_amplitude: int) -> FOPTDParams:
    df = pd.read_csv(csv_path)
    measures = df.to_numpy()
    step_response = calc_step_response(measures)
    return estimate_foptd_params(step_response, float(input_amplitude))


def main(root: Path, output: Path) -> None:
    result = {}
    for csv_path in root.glob("*.csv"):
        input_amplitude = int(csv_path.stem)
        result[input_amplitude] = estimate_foptd_task(
            csv_path, input_amplitude=input_amplitude
        )

    import pdb; pdb.set_trace()
    avg_k = sum([v.K for v in result.values()]) / len(result)
    avg_tau = sum([v.tau for v in result.values()]) / len(result)
    print(f"avg_k={avg_k}, avg_tau={avg_tau}")

    with open(output, "wb") as f:
        pickle.dump(result, f)


if __name__ == "__main__":
    try:
        root = Path(sys.argv[1])
        output = Path(sys.argv[2])
    except IndexError:
        sys.exit("Usage: from_csv.py <root> <output.pickle>")
    main(root, output)
