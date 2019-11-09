from pathlib import Path

import pandas as pd

input_dir = Path(__file__).parent.parent / "input"
titanic_dir = input_dir / "ch01-titanic"
titanic_train = titanic_dir / "train.csv"
titanic_test = titanic_dir / "test.csv"

sample_dir = input_dir / "sample-data"
sample_train = sample_dir / "train_preprocessed.csv"
sample_test = sample_dir / "test_preprocessed.csv"

def load_titanic():
    train = pd.read_csv(str(titanic_train))
    test = pd.read_csv(str(titanic_test))
    return train, test

def load_sample():
    train = pd.read_csv(str(sample_train))
    test = pd.read_csv(str(sample_test))
    return train, test

