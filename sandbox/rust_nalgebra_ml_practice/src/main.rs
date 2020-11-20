mod affine;
mod loss;
mod nn;
mod relu;
mod sigmoid;
mod softmax;

extern crate nalgebra as na;

use affine::Affine;
use loss::cross_entropy;
use na::{U2, U4, U6};
use nn::*;
use relu::Relu;
use sigmoid::Sigmoid;
use softmax::Softmax;

type N = U4;

define_network!(
    TwoLayerNetwork,
    (fc0, Affine<N, U2, U6>),
    (act0, Sigmoid<N, U6>),
    (fc1, Affine<N, U6, U2>),
    (act1, Sigmoid<N, U2>),
    (softmax0, Softmax<N, U2>)
);

fn main() {
    let x = Feature::<N, U2>::new(0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0);
    let y = Feature::<N, U2>::new(1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0);
    let mut network = TwoLayerNetwork::new();
    //train(&mut network, &x, &y, mse, 8192 * 3);
    train(&mut network, &x, &y, cross_entropy, 0.01, 0.9, 8192);
    let y_hat2 = network.forward(x.clone());

    println!("{:?}", y_hat2);
    println!("{:?}", y);
}
