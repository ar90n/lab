from pathlib import Path

import pandas as pd

input_dir = Path(__file__).parent.parent / "input"
titanic_dir = input_dir / "ch01-titanic"
titanic_train = titanic_dir / "train.csv"
titanic_test = titanic_dir / "test.csv"


def load_titanic():
    train = pd.read_csv(str(titanic_train))
    test = pd.read_csv(str(titanic_test))
    return train, test
