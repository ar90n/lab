use na::{allocator::Allocator, DefaultAllocator, DimName, U1};

use crate::nn::Feature;

pub fn accuracy<N: DimName, C: DimName>(y_hat: &Feature<N, C>, y: &Feature<N, C>) -> f64
where
    DefaultAllocator: Allocator<f64, N, C> + Allocator<f64, C, U1>,
{
    let acc = y_hat
        .row_iter()
        .zip(y.row_iter())
        .map(|(_y_hat, _y)| _y_hat.transpose().imax() == _y.transpose().imax())
        .fold(0, |acc, m| acc + m as u32);
    (acc as f64) / (N::dim() as f64)
}