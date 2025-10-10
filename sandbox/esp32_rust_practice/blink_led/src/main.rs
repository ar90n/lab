use anyhow::Result;
use esp_idf_svc::{
    hal::{delay::FreeRtos, gpio::PinDriver, peripherals::Peripherals},
    log::EspLogger,
};

const PERIOD_MS: u32 = 500;

fn main() -> Result<()> {
    esp_idf_svc::sys::link_patches();
    EspLogger::initialize_default();

    let p = Peripherals::take()?;
    let mut led_a = PinDriver::output(p.pins.gpio16)?;
    let mut led_b = PinDriver::output(p.pins.gpio17)?;

    led_a.set_high()?;
    led_b.set_low()?;https://chatgpt.com/c/68e25758-f80c-8322-8e78-4c63276f157c
    loop {
        FreeRtos::delay_ms(PERIOD_MS);
        led_a.toggle()?;
        led_b.toggle()?;
    }
}
