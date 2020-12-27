use na::{allocator::Allocator, DefaultAllocator, DimName, U1};

use crate::nn::{Feature, Layer};
pub struct Softmax<N: DimName, C: DimName>
where
    DefaultAllocator: Allocator<f64, N, C>,
{
    input: Option<Feature<N, C>>,
}

impl<N: DimName, C: DimName> Softmax<N, C>
where
    DefaultAllocator: Allocator<f64, N, C> + Allocator<f64, U1, C> + Allocator<f64, C, U1>,
{
    pub fn new() -> Self {
        Self { input: None }
    }
    fn _forward(input: &Feature<N, C>) -> Feature<N, C> {
        Feature::<N, C>::from_rows(
            &input
                .row_iter()
                .map(|row| row.map(|v| (v - row.max()).exp()))
                .map(|row| &row / row.sum())
                .collect::<Vec<_>>(),
        )
    }
}

impl<N: DimName, C: DimName> Layer for Softmax<N, C>
where
    DefaultAllocator: Allocator<f64, N, C> + Allocator<f64, U1, C> + Allocator<f64, C, U1>,
{
    type Input = Feature<N, C>;
    type Output = Feature<N, C>;

    fn forward(&mut self, input: Feature<N, C>) -> Feature<N, C> {
        let output = Self::_forward(&input);
        self.input = Some(input);
        output
    }
    fn backward(&mut self, delta: Feature<N, C>) -> Feature<N, C> {
        let output = Self::_forward(self.input.as_ref().unwrap());
        Feature::<N, C>::from_rows(
            &delta
                .row_iter()
                .zip(output.row_iter())
                .map(|(d, o)| o.component_mul(&d.add_scalar(-(d * o.transpose())[0])))
                .collect::<Vec<_>>(),
        )
    }
}

#[test]
fn test_softmax() {
    use approx::assert_relative_eq;
    use na::U3;

    let mut softmax_layer = Softmax::<U3, U3>::new();
    assert_eq!(softmax_layer.input, None);

    let x = Feature::<U3, U3>::new(5.0, 4.0, 2.0, 4.0, 2.0, 8.0, 4.0, 4.0, 1.0);
    let y = softmax_layer.forward(x);

    assert_relative_eq!(
        y,
        Feature::<U3, U3>::new(0.705, 0.259, 0.035, 0.018, 0.002, 0.980, 0.488, 0.488, 0.024),
        max_relative = 5.0,
    );
    assert_eq!(softmax_layer.input, Some(x));

    //    let diff_y = Feature::<U3, U3>::new(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0) - y;
    //    let diff_x = softmax_layer.backward(diff_y);
    //    assert_relative_eq!(
    //        diff_x,
    //        Feature::<U3, U3>::new(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,)
    //    )
}
