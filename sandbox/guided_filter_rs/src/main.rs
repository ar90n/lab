use image::{GenericImageView, ImageBuffer, Luma, Pixel, Primitive};
use num_traits::NumCast;

fn cast_subpixel<S, T>(pixel: &Luma<S>) -> Luma<T>
where
    S: Primitive,
    T: Primitive,
{
    let Luma([data]) = *pixel;
    Luma([NumCast::from(data).unwrap(); 1])
}

fn split_channels<I, P, S>(image: &I) -> Vec<ImageBuffer<Luma<S>, Vec<S>>>
where
    I: image::GenericImageView<Pixel = P>,
    P: image::Pixel<Subpixel = S> + 'static,
    S: Primitive + 'static,
{
    let (width, height) = image.dimensions();
    let mut result = vec![ImageBuffer::new(width, height); P::CHANNEL_COUNT.into()];

    for y in 0..height {
        for x in 0..width {
            for (i, v) in image.get_pixel(x, y).channels().iter().enumerate() {
                *result[i].get_pixel_mut(x, y) = Luma([*v; 1]);
            }
        }
    }

    result
}

fn mean2d<I, S>(image: &I, r: u32) -> ImageBuffer<Luma<f64>, Vec<f64>>
where
    I: GenericImageView<Pixel = Luma<S>>,
    S: Primitive + 'static,
{
    fn get_luma_as_f64<I, S>(image: &I, x: u32, y: u32) -> f64
    where
        I: GenericImageView<Pixel = Luma<S>>,
        S: Primitive + 'static,
    {
        let Luma(data) = image.get_pixel(x, y);
        NumCast::from(data[0]).unwrap()
    }

    fn calc_mean(head_value: f64, tail_value: f64, r: u32) -> f64 {
        let den = 2.0 * r as f64 + 1.0;
        let num = head_value - tail_value;
        num / den
    }

    fn meand2d_tr<I, S>(image: &I, r: u32) -> ImageBuffer<Luma<f64>, Vec<f64>>
    where
        I: GenericImageView<Pixel = Luma<S>>,
        S: Primitive + 'static,
    {
        let (width, height) = image.dimensions();
        let mut result = ImageBuffer::new(height, width);

        for y in 0..height {
            let mut head_x: u32 = 0;
            let mut mid_x: u32 = 0;
            let mut tail_x: u32 = 0;
            let mut acc_head_x: f64 = 0.0;
            let mut acc_tail_x: f64 = 0.0;

            while mid_x < width {
                if head_x < width {
                    acc_head_x += get_luma_as_f64(image, head_x, y);
                    head_x += 1;
                }
                if r < mid_x {
                    acc_tail_x += get_luma_as_f64(image, tail_x, y);
                    tail_x += 1;
                }
                if r <= head_x {
                    let pixel = Luma([calc_mean(acc_head_x, acc_tail_x, r); 1]);
                    result.put_pixel(y, mid_x, pixel);
                    mid_x += 1;
                }
            }
        }

        result
    }

    meand2d_tr(&meand2d_tr(image, r), r)
}

fn cals_eps(eps: f64) -> f64 {
    let eps = 255.0 * eps;
    eps * eps
}

fn main() {
    let r = 18;
    let eps = cals_eps(0.1);
    let img = image::open("./cat.jpg").unwrap().to_luma8();

    let i = &img;
    let mean_i = mean2d(i, r);

    let (w, h) = i.dimensions();
    let ii = ImageBuffer::from_fn(w, h, |x, y| {
        cast_subpixel(i.get_pixel(x, y)).map(|v: f64| v * v)
    });
    let corr_i = mean2d(&ii, r);

    let var_i = ImageBuffer::from_fn(w, h, |x, y| {
        mean_i
            .get_pixel(x, y)
            .map2(corr_i.get_pixel(x, y), |m, c| c - m * m)
    });

    let a = ImageBuffer::from_fn(w, h, |x, y| {
        var_i.get_pixel(x, y).map_without_alpha(|v| v / (v + eps))
    });

    let b = ImageBuffer::from_fn(w, h, |x, y| {
        mean_i
            .get_pixel(x, y)
            .map2(a.get_pixel(x, y), |m, a| (1.0 - a) * m)
    });

    let mean_a = mean2d(&a, r);
    let mean_b = mean2d(&b, r);

    let q = ImageBuffer::from_fn(w, h, |x, y| {
        let i_v = {
            let Luma([data]) = *i.get_pixel(x, y);
            data as f64
        };

        mean_a
            .get_pixel(x, y)
            .map2(mean_b.get_pixel(x, y), |a, b| a * i_v + b)
    });
    let vv = ImageBuffer::from_fn(w, h, |x, y| -> Luma<u8> {
        cast_subpixel(q.get_pixel(x, y))
    });
    vv.save(format!("tmp_{}.png", eps)).unwrap();
}