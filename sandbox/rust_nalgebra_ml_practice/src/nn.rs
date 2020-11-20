use na::MatrixMN;

pub type Feature<N, C> = MatrixMN<f64, N, C>;

pub trait Layer {
    type Input;
    type Output;
    fn forward(&mut self, input: Self::Input) -> Self::Output;
    fn backward(&mut self, output: Self::Output) -> Self::Input;
    fn update(&mut self, _lr: f64, _momentum: f64) {}
}

pub trait LoadableLayer: Layer {
    type Weight;
    fn load(&mut self, _weight: Self::Weight);
}

pub trait Network {
    type Input;
    type Output;

    fn forward(&mut self, input: Self::Input) -> Self::Output;
    fn backward(&mut self, output: Self::Output) -> Self::Input;
    fn update(&mut self, lr: f64, momentum: f64);
}

pub fn train<T, F>(
    network: &mut T,
    x: &<T as Network>::Input,
    y: &<T as Network>::Output,
    loss: F,
    lr: f64,
    momentum: f64,
    epochs: usize,
) where
    T: Network,
    F: Fn(&<T as Network>::Output, &<T as Network>::Output) -> (f64, <T as Network>::Output),
    <T as Network>::Input: Clone,
{
    for i in 0..epochs {
        let (_loss, _delta) = loss(&network.forward(x.clone()), y);
        if i % 128 == 0 {
            println!("loss: {:}", _loss);
        }
        network.backward(_delta);
        network.update(lr, momentum);
    }

    let (_loss, _) = loss(&network.forward(x.clone()), y);
    println!("loss: {:}", _loss);
}

#[macro_export]
macro_rules! expr {
    ($x:expr) => {
        $x
    };
}

#[macro_export]
macro_rules! tuple_index {
    ($tuple:expr, $idx:ident) => {
        expr!($tuple.$idx)
    };
}

#[macro_export]
macro_rules! impl_forward {
    ($self:expr, $input:expr, $head:ident,) => {
        tuple_index!($self, $head).forward($input)
    };
    ($self:expr, $input:expr, $head:ident, $($tail:ident,)*) => {
        impl_forward!($self, tuple_index!($self, $head).forward($input), $($tail,)*)
    };
}

#[macro_export]
macro_rules! impl_backward {
    ($self:expr, $input:expr, ) => {$input};
    ($self:expr, $input:expr, $head:ident, $($tail:ident,)*) => {
        tuple_index!($self, $head).backward(
            impl_backward!($self, $input, $($tail,)*)
        )
    };
}

#[macro_export]
macro_rules! impl_update {
    ($self:expr, $lr:expr, $momentum:expr, ) => {};
    ($self:expr, $lr:expr, $momentum:expr, $head:ident, $($tail:ident,)*) => {
        tuple_index!($self, $head).update($lr, $momentum);
        impl_update!($self, $lr, $momentum, $($tail,)*)
    };
}

#[macro_export]
macro_rules! first_layer {
    (($head:ty, $($_:ty,)*)) => {
        $head
    };
}

#[macro_export]
macro_rules! last_layer {
    (($head:ty,)) => {
        $head
    };
    (($head:ty, $($tail:ty,)*)) => {
        last_layer!(($($tail,)*))
    };
}

#[macro_export]
macro_rules! define_network {
    ($network_name:ident, $(($name:ident, $layer:ty)),*) => {
        struct $network_name{$($name:$layer,)*}
        impl $network_name {
            pub fn new() -> Self {
                Self{$($name:<$layer>::new(),)*}
            }
        }
        impl Network for $network_name {
            type Input = <first_layer!(($($layer,)*)) as Layer>::Input;
            type Output = <last_layer!(($($layer,)*)) as Layer>::Output;

            fn forward(&mut self, input: Self::Input) -> Self::Output{
                impl_forward!(self, input, $($name,)*)
            }
            fn backward(&mut self, output: Self::Output) -> Self::Input{
                impl_backward!(self, output,$($name,)*)
            }
            fn update(&mut self, lr: f64, momentum: f64) {
                impl_update!(self, lr, momentum, $($name,)*);
            }
        }
    };
}
