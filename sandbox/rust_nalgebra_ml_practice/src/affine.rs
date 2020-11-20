use na::{
    allocator::{Allocator, Reallocator},
    DefaultAllocator, DimAdd, DimName, DimSum, MatrixMN, U1,
};

use crate::nn::{Feature, Layer, LoadableLayer};

fn to_homogeneous<N: DimName, C: DimName + DimAdd<U1>>(
    input: Feature<N, C>,
) -> Feature<N, DimSum<C, U1>>
where
    DefaultAllocator: Allocator<f64, N, C> + Reallocator<f64, N, C, N, DimSum<C, U1>>,
{
    input.insert_column(C::dim(), 1.0)
}

pub struct Affine<N: DimName, C0: DimName + DimAdd<U1>, C1: DimName>
where
    DefaultAllocator: Allocator<f64, N, DimSum<C0, U1>> + Allocator<f64, DimSum<C0, U1>, C1>,
{
    input: Option<Feature<N, DimSum<C0, U1>>>,
    d_w: MatrixMN<f64, DimSum<C0, U1>, C1>,
    v: MatrixMN<f64, DimSum<C0, U1>, C1>,
    w: MatrixMN<f64, DimSum<C0, U1>, C1>,
}

impl<N: DimName, C0: DimName + DimAdd<U1>, C1: DimName> Affine<N, C0, C1>
where
    DimSum<C0, U1>: DimName,
    DefaultAllocator: Allocator<f64, N, DimSum<C0, U1>> + Allocator<f64, DimSum<C0, U1>, C1>,
{
    pub fn new() -> Self {
        Self {
            input: None,
            d_w: MatrixMN::<f64, DimSum<C0, U1>, C1>::zeros(),
            v: MatrixMN::<f64, DimSum<C0, U1>, C1>::zeros(),
            w: MatrixMN::<f64, DimSum<C0, U1>, C1>::new_random(),
        }
    }
}

impl<N: DimName, C0: DimName + DimAdd<U1>, C1: DimName> Layer for Affine<N, C0, C1>
where
    DimSum<C0, U1>: DimName,
    DefaultAllocator: Allocator<f64, N, C0>
        + Allocator<f64, C1, C0>
        + Allocator<f64, N, C1>
        + Allocator<f64, DimSum<C0, U1>, N>
        + Allocator<f64, DimSum<C0, U1>, C1>
        + Reallocator<f64, N, C0, N, DimSum<C0, U1>>,
{
    type Input = Feature<N, C0>;
    type Output = Feature<N, C1>;

    fn forward(&mut self, input: Self::Input) -> Self::Output {
        let input = to_homogeneous(input);
        let output = &input * &self.w;
        self.input = Some(input);
        output
    }
    fn backward(&mut self, delta: Self::Output) -> Self::Input {
        self.d_w = self.input.as_ref().unwrap().transpose() * &delta;
        delta * self.w.fixed_slice::<C0, C1>(0, 0).transpose()
    }

    fn update(&mut self, lr: f64, momentum: f64) {
        self.v = momentum * &self.v - lr * &self.d_w;
        self.w += &self.v;
    }
}

impl<N: DimName, C0: DimName + DimAdd<U1>, C1: DimName> LoadableLayer for Affine<N, C0, C1>
where
    DimSum<C0, U1>: DimName,
    DefaultAllocator: Allocator<f64, N, C0>
        + Allocator<f64, C1, C0>
        + Allocator<f64, N, C1>
        + Allocator<f64, DimSum<C0, U1>, N>
        + Allocator<f64, DimSum<C0, U1>, C1>
        + Reallocator<f64, N, C0, N, DimSum<C0, U1>>,
{
    type Weight = Feature<DimSum<C0, U1>, C1>;
    fn load(&mut self, weight: Self::Weight) {
        self.w = weight;
    }
}

#[test]
fn test_affine() {
    use approx::assert_relative_eq;
    use na::{U2, U3};

    let mut affine_layer = Affine::<U2, U2, U3>::new();
    affine_layer.load(MatrixMN::<f64, U3, U3>::new(
        1.0, 2.0, 1.0, -1.0, 0.0, 1.0, 1.0, -1.0, 1.0,
    ));
    assert_eq!(affine_layer.d_w, MatrixMN::<f64, U3, U3>::zeros());

    let x = Feature::<U2, U2>::new(-1.0, 2.0, 0.0, 4.0);
    let y = affine_layer.forward(x);

    assert_eq!(y, Feature::<U2, U3>::new(-2.0, -3.0, 2.0, -3.0, -1.0, 5.0));
    assert_eq!(affine_layer.d_w, MatrixMN::<f64, U3, U3>::zeros());

    let diff_y = Feature::<U2, U3>::new(-2.0, -2.5, 2.5, 0.0, 1.0, 5.0) - y;
    let diff_x = affine_layer.backward(diff_y);
    assert_relative_eq!(diff_x, Feature::<U2, U2>::new(1.5, 0.5, 7.0, -3.0));
    assert_relative_eq!(
        affine_layer.d_w,
        MatrixMN::<f64, U3, U3>::new(0.0, -0.5, -0.5, 12.0, 9.0, 1.0, 3.0, 2.5, 0.5)
    );
}
