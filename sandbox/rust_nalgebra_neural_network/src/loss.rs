use na::{allocator::Allocator, DefaultAllocator, DimName};

use crate::nn::Feature;

#[allow(dead_code)]
pub fn mse<N: DimName, C: DimName>(x: &Feature<N, C>, y: &Feature<N, C>) -> (f64, Feature<N, C>)
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    let n = N::dim() as f64;
    let forward = 0.5 * (x - y).map(|e| e * e).sum() / n;
    let backward = (x - y) / n;
    (forward, backward)
}

#[allow(dead_code)]
pub fn cross_entropy<N: DimName, C: DimName>(
    x: &Feature<N, C>,
    y: &Feature<N, C>,
) -> (f64, Feature<N, C>)
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    let n = N::dim() as f64;
    let epsillon = 1e-12;
    let forward = -x.zip_map(y, |_x, _y| _y * (_x + epsillon).ln()).sum() / n;
    let backward = x.zip_map(y, |_x, _y| -_y / (_x + epsillon)) / n;
    (forward, backward)
}
