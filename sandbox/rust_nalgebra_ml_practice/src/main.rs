mod affine;
mod loss;
mod nn;
mod relu;
mod sigmoid;
mod softmax;

extern crate mnist;
extern crate nalgebra as na;

use affine::Affine;
use loss::{cross_entropy, mse};
use na::{DimName, DimProd, U1, U10, U2, U28, U32, U4, U6, U64};
use nn::*;
use relu::Relu;
use sigmoid::Sigmoid;
use softmax::Softmax;

type N = U64;
type C = U10;
type M = U64;
type F = DimProd<U28, U28>;

define_network!(
    TwoLayerNetwork,
    (fc0, Affine<N, F, M>),
    (act0, Sigmoid<N, M>),
    (fc1, Affine<N, M, C>),
    (softmax0, Softmax<N, C>)
);

fn main() {
        use mnist::{Mnist, MnistBuilder};
        let (trn_size, rows, cols) = (50000, 28, 28);
        let Mnist {
            trn_img, trn_lbl, ..
        } = MnistBuilder::new()
            .label_format_digit()
            .training_set_length(trn_size)
            .validation_set_length(100)
            .test_set_length(100)
            .download_and_extract()
            .finalize();
    
        let x = Feature::<N, F>::from_iterator(
            trn_img
                .iter()
                .take(N::dim() * rows * cols)
                .map(|v| (*v as f64) / 255.0),
        );
    
        let mut y = Feature::<N, C>::zeros();
        for (i, n) in trn_lbl.iter().take(N::dim()).enumerate() {
            y[(i, *n as usize)] = 1.0;
        }
    
        let mut network = TwoLayerNetwork::new();
        train(&mut network, &x, &y, cross_entropy, 0.01, 0.9, 1 * 512 * 10);
        let y_hat2 = network.forward(x.clone());
    
        println!("{:?}", y_hat2);
        println!("{:?}", y);
}
