from dataclasses import dataclass
import sys
from pathlib import Path

import pickle
import numpy as np
import skimage
from skimage.color import rgb2hsv
from skimage.morphology import binary_opening, binary_closing
from skimage.measure import label, regionprops, EllipseModel
from scipy.optimize import minimize
from joblib import Memory, Parallel, delayed


memory = Memory(cachedir=str(Path(__file__).parent / "cache"), verbose=0)


@memory.cache
def load_image(path: Path) -> np.ndarray:
    return skimage.io.imread(str(path))


@memory.cache
def extract_marker_coord(
    image: np.ndarray, min_th: float = 0.62, max_th: float = 0.68
) -> np.ndarray:
    hue_img = rgb2hsv(image)[:, :, 0]
    marker_candidates_mask = (min_th < hue_img) & (hue_img < max_th)
    marker_candidates_mask = binary_opening(
        marker_candidates_mask, footprint=skimage.morphology.disk(3)
    )
    marker_candidates_mask = binary_closing(
        marker_candidates_mask, footprint=skimage.morphology.disk(3)
    )
    marker_candidates_label = label(marker_candidates_mask)
    marker_candidates_prop = max(
        regionprops(marker_candidates_label), key=lambda x: x.area
    )
    return np.array(marker_candidates_prop.centroid)


@memory.cache
def extract_marker_coord_task(path: Path) -> np.ndarray:
    return extract_marker_coord(load_image(path))


def extract_marker_coords(root: Path) -> np.ndarray:
    marker_coords = Parallel(n_jobs=-1)(
        delayed(extract_marker_coord_task)(path) for path in sorted(root.glob("*.png"))
    )
    return np.stack(marker_coords)


@dataclass
class FOPTDParams:
    K: float = 1
    tau: float = 1
    tau_d: float = 0


def project_to_circle(marker_coords: np.ndarray) -> np.ndarray:
    ellipse = EllipseModel()
    ellipse.estimate(marker_coords)
    [xc, yc, a, b, theta] = ellipse.params
    A = np.array([[a, -b, xc], [b, a, yc], [0, 0, 1]])
    X = np.hstack((marker_coords, np.ones((marker_coords.shape[0], 1))))
    P = np.linalg.solve(A, X.T).T
    return P[:, :2]


def calc_step_response(coords: np.ndarray) -> np.ndarray:
    coords_on_unit_circle = coords / np.repeat(
        np.linalg.norm(coords, axis=1).reshape(-1, 1), 2, axis=1
    )
    forward_coord = coords_on_unit_circle[2:, 0] + 1j * coords_on_unit_circle[2:, 1]
    backward_coord = coords_on_unit_circle[:-2, 0] - 1j * coords_on_unit_circle[:-2, 1]
    delta_angles = np.angle(forward_coord * backward_coord) / 2.0

    max_idx = np.argmax(delta_angles)

    min_res_th = 0.004
    beg_idx = max(0, max_idx - np.argmax(delta_angles[max_idx:0:-1] < min_res_th) - 30)

    stop_th = 0.5 * delta_angles[max_idx]
    sustain_length = max(0, np.argmax(delta_angles[max_idx:] < stop_th) - 150)
    end_idx = max_idx + sustain_length
    return delta_angles[beg_idx:end_idx]


def foptd(t, K=1, tau=1, tau_d=0):
    tau_d = max(0, tau_d)
    tau = max(0, tau)
    return np.array(
        [K * (1 - np.exp(-(t - tau_d) / tau)) if t >= tau_d else 0 for t in t]
    )


def estimate_foptd_params(
    step_response: np.ndarray, input_amplitude: float, interval_sec: float
) -> FOPTDParams:
    def err(X, t, y):
        K, tau, tau_d = X
        z = foptd(t, K, tau, tau_d)
        iae = sum(abs(z - y)) * (max(t) - min(t)) / len(t)
        return iae

    X = [0.10 / input_amplitude, 10 * interval_sec, 30 * interval_sec]
    ts = np.array(range(len(step_response))) * interval_sec
    ys = step_response / input_amplitude
    K, tau, tau_d = minimize(err, X, args=(ts, ys)).x

    return FOPTDParams(K, tau, tau_d)


def estimate_foptd_task(root: Path, input_amplitude: int, fps: int) -> FOPTDParams:
    marker_coords = extract_marker_coords(root)
    projected_coords = project_to_circle(marker_coords)
    step_response = calc_step_response(projected_coords)

    interval_sec = 1.0 / fps
    return estimate_foptd_params(step_response, float(input_amplitude), interval_sec)


def main(root: Path, output: Path) -> None:
    fps = 240

    result = {}
    for capture_root in root.iterdir():
        if not capture_root.is_dir():
            continue
        input_amplitude = int(capture_root.name)
        result[input_amplitude] = estimate_foptd_task(
            capture_root, input_amplitude=input_amplitude, fps=fps
        )

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
        sys.exit("Usage: main.py <root> <output.pickle>")
    main(root, output)
