# Neural network with Rust and nalgebra

## Feature

* Support following layers
  * Affine
  * Relu
  * Sigmoid
  * Softmax
* Support following loss functions
  * MSE
  * Cross endtroy
* Support following optimizers
  * SGD
  * Momentum
* Support following metrics
  * Accuracy

## How to Run

```bash
$ cargo run --release
   Compiling nuerayl_network_rust_nalgebra v0.1.0 (/Users/argon/workspace/managed/build-my-own-x/neural_network/rust_nalgebra)
    Finished release [optimized] target(s) in 6.06s
     Running `target/release/nuerayl_network_rust_nalgebra`
Attempting to download and extract train-images-idx3-ubyte.gz...
  File "data/train-images-idx3-ubyte.gz" already exists, skipping downloading.
  Extracted file "data/train-images-idx3-ubyte" already exists, skipping extraction.
Attempting to download and extract train-labels-idx1-ubyte.gz...
  File "data/train-labels-idx1-ubyte.gz" already exists, skipping downloading.
  Extracted file "data/train-labels-idx1-ubyte" already exists, skipping extraction.
Attempting to download and extract t10k-images-idx3-ubyte.gz...
  File "data/t10k-images-idx3-ubyte.gz" already exists, skipping downloading.
  Extracted file "data/t10k-images-idx3-ubyte" already exists, skipping extraction.
Attempting to download and extract t10k-labels-idx1-ubyte.gz...
  File "data/t10k-labels-idx1-ubyte.gz" already exists, skipping downloading.
  Extracted file "data/t10k-labels-idx1-ubyte" already exists, skipping extraction.
train_loss :0.40808707709279984, val_loss:0.19911552595315996
train_loss :0.18310949472804847, val_loss:0.14974677991154411
train_loss :0.13293040212965526, val_loss:0.1345652207894021
train_loss :0.10533780569992023, val_loss:0.1239502186258136
train_loss :0.08606324070746257, val_loss:0.1153736756832318
train_loss :0.07207094016809537, val_loss:0.11063348936007525
train_loss :0.06150422025973127, val_loss:0.10686321562458755
train_loss :0.05267341873088317, val_loss:0.1048867846294252
train_loss :0.04527769945800332, val_loss:0.1026046686923554
train_loss :0.03874164985971278, val_loss:0.10210111763531152
train_loss :0.03331281402100414, val_loss:0.1010923892390939
train_loss :0.028392551351683667, val_loss:0.10146607925872926
accuray: 0.9733573717948718
```
