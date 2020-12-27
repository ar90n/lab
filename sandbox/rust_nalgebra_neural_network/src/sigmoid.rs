use na::allocator::Allocator;
use na::{DefaultAllocator, DimName};

use crate::nn::{Feature, Layer};

pub struct Sigmoid<N: DimName, C: DimName>
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    input: Option<Feature<N, C>>,
}

impl<N: DimName, C: DimName> Sigmoid<N, C>
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self { input: None }
    }

    fn _forward(input: &Feature<N, C>) -> Feature<N, C> {
        input.map(|v: f64| 1.0 / (1.0 + (-v).exp()))
    }
}

impl<N: DimName, C: DimName> Layer for Sigmoid<N, C>
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    type Input = Feature<N, C>;
    type Output = Feature<N, C>;

    fn forward(&mut self, input: Self::Input) -> Self::Output {
        let output = Self::_forward(&input);
        self.input = Some(input);
        output
    }
    fn backward(&mut self, delta: Self::Output) -> Self::Input {
        let output = Self::_forward(self.input.as_ref().unwrap());
        delta.zip_map(&output, |d, y| d * (1.0 - y) * y)
    }
}

#[test]
fn test_sigmoid() {
    use approx::assert_relative_eq;
    use na::U2;

    let mut sigmoid_layer = Sigmoid::<U2, U2>::new();
    assert_eq!(sigmoid_layer.input, None);

    let x = Feature::<U2, U2>::new(-1.0, 1.2, 3.4, 2.1);
    let y = sigmoid_layer.forward(x);

    assert_eq!(
        y,
        Feature::<U2, U2>::new(
            0.2689414213699951,
            0.7685247834990175,
            0.9677045353015494,
            0.8909031788043871
        )
    );
    assert_eq!(sigmoid_layer.input, Some(x));

    let delta = Feature::<U2, U2>::new(1.0, 2.0, 3.0, 4.0) - y;
    let diff_x = sigmoid_layer.backward(delta);
    assert_relative_eq!(
        diff_x,
        Feature::<U2, U2>::new(
            0.14373484045721513,
            0.21907259480984634,
            0.0635142482827235,
            0.3021877477326704
        )
    )
}
