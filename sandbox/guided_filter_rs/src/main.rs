use image::{DynamicImage, GenericImageView, ImageBuffer, Luma, Pixel, Primitive, Rgb};
use num_traits::NumCast;

extern crate image;
extern crate num_complex;

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

fn meand2d<I, S>(image: &I, r: u32) -> ImageBuffer<Luma<f64>, Vec<f64>>
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

            while head_x < r {
                acc_head_x += get_luma_as_f64(image, head_x, y);

                head_x += 1;
            }

            while mid_x <= r {
                acc_head_x += get_luma_as_f64(image, head_x, y);

                let pixel = Luma([calc_mean(acc_head_x, acc_tail_x, r); 1]);
                result.put_pixel(y, mid_x, pixel);

                head_x += 1;
                mid_x += 1;
            }

            while head_x < width {
                acc_head_x += get_luma_as_f64(image, head_x, y);
                acc_tail_x += get_luma_as_f64(image, tail_x, y);

                let pixel = Luma([calc_mean(acc_head_x, acc_tail_x, r); 1]);
                result.put_pixel(y, mid_x, pixel);

                head_x += 1;
                mid_x += 1;
                tail_x += 1;
            }

            while mid_x < width {
                acc_tail_x += get_luma_as_f64(image, tail_x, y);

                let pixel = Luma([calc_mean(acc_head_x, acc_tail_x, r); 1]);
                result.put_pixel(y, mid_x, pixel);

                mid_x += 1;
                tail_x += 1;
            }
        }

        result
    }

    let tr_tmp = meand2d_tr(image, r);
    meand2d_tr(&tr_tmp, r)
}

fn main() {
    let img = image::open("./cat.jpg").unwrap();
    let imgbuf = split_channels(&img);

    let r = 4;
    let i = &imgbuf[0];
    let mean_i = meand2d(i, r);

    let (w, h) = i.dimensions();
    let ii = ImageBuffer::from_fn(w, h, |x, y| {
        let Luma(data) = i.get_pixel(x, y);
        let v = data[0] as f64;
        Luma([v * v; 1])
    });
    let corr_i = meand2d(&ii, r);

    let var_i = ImageBuffer::from_fn(w, h, |x, y| {
        let Luma(mean_v) = mean_i.get_pixel(x, y);
        let Luma(corr_v) = corr_i.get_pixel(x, y);
        let v = corr_v[0] - mean_v[0] * mean_v[0];
        Luma([v; 1])
    });

    let eps = 255.0 * 0.8;
    let eps = eps * eps;
    let a = ImageBuffer::from_fn(w, h, |x, y| {
        let Luma(var_v) = var_i.get_pixel(x, y);
        let v = var_v[0] / (var_v[0] + eps);
        Luma([v; 1])
    });

    let b = ImageBuffer::from_fn(w, h, |x, y| {
        let Luma(mean_v) = mean_i.get_pixel(x, y);
        let Luma(a_v) = a.get_pixel(x, y);
        let v = (1.0 - a_v[0]) * mean_v[0];
        Luma([v; 1])
    });

    let mean_a = meand2d(&a, r);
    let mean_b = meand2d(&b, r);

    let q = ImageBuffer::from_fn(w, h, |x, y| {
        let Luma(mean_a_v) = mean_a.get_pixel(x, y);
        let Luma(i_v) = i.get_pixel(x, y);
        let Luma(mean_b_v) = mean_b.get_pixel(x, y);
        let v = mean_a_v[0] * i_v[0] as f64 + mean_b_v[0];
        Luma([v; 1])
    });

    let vv = ImageBuffer::from_fn(w, h, |x, y| {
        let Luma(data) = q.get_pixel(x, y);
        Luma([data[0] as u8; 1])
    });
    vv.save(format!("tmp_{}.png", eps)).unwrap();
    //for (i, v) in imgbuf.iter().enumerate() {
    //    let vv = meand2d(v, 4);
    //    let (w, h) = vv.dimensions();
    //    let vv = ImageBuffer::from_fn(w, h, |x, y| {
    //        let Luma(data) = vv.get_pixel(x, y);
    //        Luma([data[0] as u8; 1])
    //    });
    //    vv.save(format!("tmp_{}.png", i)).unwrap();
    //}
}
