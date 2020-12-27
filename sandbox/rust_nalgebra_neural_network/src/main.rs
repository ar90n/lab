mod affine;
mod loss;
mod metric;
mod nn;
mod relu;
mod sigmoid;
mod softmax;

extern crate mnist;
extern crate nalgebra as na;
extern crate num;

use affine::Affine;
use loss::cross_entropy;
use metric::accuracy;
use mnist::{MnistBuilder, NormalizedMnist};
use na::{DimName, DimProd, U10, U28, U64, U80};
use nn::*;
use num::cast::AsPrimitive;
use relu::Relu;
use softmax::Softmax;

type N = U64;
type C = U10;
type M = U80;
type F = DimProd<U28, U28>;

define_network!(
    TwoLayerNetwork,
    (fc0, Affine<N, F, M>),
    (act0, Relu<N, M>),
    (fc1, Affine<N, M, M>),
    (act1, Relu<N, M>),
    (fc2, Affine<N, M, C>),
    (softmax0, Softmax<N, C>)
);

fn load_dataset<N: DimName, F: DimName, C: DimName>(
    img: &[f32],
    label: &[u8],
) -> impl Iterator<Item = (Feature<N, F>, Feature<N, C>)>
where
    na::DefaultAllocator: na::allocator::Allocator<f64, N, F> + na::allocator::Allocator<f64, N, C>,
{
    fn create_mini_batches<N: DimName, C: DimName, T>(feature: &[T]) -> Vec<Feature<N, C>>
    where
        T: AsPrimitive<f64>,
        na::DefaultAllocator: na::allocator::Allocator<f64, N, C>,
    {
        feature
            .into_iter()
            .map(|&v| v.as_())
            .collect::<Vec<_>>()
            .chunks_exact(N::dim() * C::dim())
            .map(|chunk| Feature::<N, C>::from_row_slice(chunk))
            .collect::<Vec<_>>()
    }
    let x = create_mini_batches::<N, F, f32>(img);
    let y = create_mini_batches::<N, C, u8>(label);

    x.into_iter().zip(y.into_iter())
}

fn main() {
    let NormalizedMnist {
        trn_img,
        trn_lbl,
        val_img,
        val_lbl,
        tst_img,
        tst_lbl,
    } = MnistBuilder::new()
        .label_format_one_hot()
        .training_set_length(50000)
        .validation_set_length(10000)
        .test_set_length(10000)
        .download_and_extract()
        .finalize()
        .normalize();

    let mut network = TwoLayerNetwork::new();
    let epochs = 12;
    let lr = 0.01;
    let momentum = 0.9;

    for _ in 0..epochs {
        let train_loss = {
            let (acc_train_loss, n) = load_dataset::<N, F, C>(&trn_img, &trn_lbl).fold(
                (0.0, 0),
                |(acc, n), (train_x, train_y)| {
                    let (loss, delta) = cross_entropy(&network.forward(train_x), &train_y);
                    network.backward(delta);
                    network.update(lr, momentum);
                    (acc + loss, n + 1)
                },
            );
            acc_train_loss / (n as f64)
        };

        let val_loss = {
            let (acc_val_loss, n) = load_dataset::<N, F, C>(&val_img, &val_lbl).fold(
                (0.0, 0),
                |(acc, n), (val_x, val_y)| {
                    let (loss, _) = cross_entropy(&network.forward(val_x), &val_y);
                    (acc + loss, n + 1)
                },
            );
            acc_val_loss / (n as f64)
        };
        println!("train_loss :{:?}, val_loss:{:?}", train_loss, val_loss);
    }

    let accuracy = {
        let (acc_accuracy, n) = load_dataset::<N, F, C>(&tst_img, &tst_lbl)
            .fold((0.0, 0), |(acc, n), (tst_x, tst_y)| {
                (acc + accuracy(&network.forward(tst_x), &tst_y), n + 1)
            });
        acc_accuracy / (n as f64)
    };
    println!("accuray: {:?}", accuracy);
}