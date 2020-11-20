use na::{allocator::Allocator, DefaultAllocator, DimName};
use crate::nn::{Feature, Layer};

pub struct Relu<N: DimName, C: DimName>
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    input: Option<Feature<N, C>>,
}

impl<N: DimName, C: DimName> Relu<N, C>
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    pub fn new() -> Self {
        Self { input: None }
    }
}

impl<N: DimName, C: DimName> Layer for Relu<N, C>
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    type Input = Feature<N, C>;
    type Output = Feature<N, C>;

    fn forward(&mut self, input: Self::Input) -> Self::Output {
        let output = input.map(|v| if 0.0 < v { v } else { 0.0 });
        self.input = Some(input);
        output
    }
    fn backward(&mut self, delta: Self::Output) -> Self::Input {
        self.input
            .as_ref()
            .unwrap()
            .zip_map(&delta, |x, d| if 0.0 < x { d } else { 0.0 })
    }
}

#[test]
fn test_relu() {
    use approx::assert_relative_eq;
    use na::{U2, U3};

    let mut relu_layer = Relu::<U2, U3>::new();
    assert_eq!(relu_layer.input, None);

    let x = Feature::<U2, U3>::new(-1.0, 1.2, 3.4, 2.1, -2.2, 2.3);
    let y = relu_layer.forward(x.clone());

    assert_eq!(y, Feature::<U2, U3>::new(0.0, 1.2, 3.4, 2.1, 0.0, 2.3));
    assert_eq!(relu_layer.input, Some(x));

    let delta = Feature::<U2, U3>::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0) - y;
    let diff_x = relu_layer.backward(delta);
    assert_relative_eq!(
        diff_x,
        Feature::<U2, U3>::new(0.0, 0.8, -0.4, 1.9, 0.0, 3.7)
    );
}
