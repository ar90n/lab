use na::{allocator::Allocator, DefaultAllocator, DimName};

use crate::nn::Feature;

pub fn mse<N: DimName, C: DimName>(x: &Feature<N, C>, y: &Feature<N, C>) -> (f64, Feature<N, C>)
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    let forward = 0.5 * (x - y).map(|e| e * e).sum();
    let backward = x - y;
    (forward, backward)
}

pub fn cross_entropy<N: DimName, C: DimName>(
    x: &Feature<N, C>,
    y: &Feature<N, C>,
) -> (f64, Feature<N, C>)
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    let epsillon = 1e-12;
    let forward = -x.zip_map(y, |_x, _y| _y * (_x + epsillon).ln()).sum();
    let backward = x.zip_map(y, |_x, _y| -_y / (_x + epsillon));
    (forward, backward)
}
