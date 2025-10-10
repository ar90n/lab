use anyhow::Result;
use esp_idf_svc::{
    hal::{
        delay::{Delay, BLOCK},
        i2c::{I2cConfig, I2cDriver},
        peripherals::Peripherals,
        gpio::PinDriver,
        prelude::*,
    },
    log::EspLogger,
};
use std::thread;
use std::time::Duration;

const I2C_FREQ_HZ: u32 = 100_000;

// --- MPU-6500 定数 ---
const MPU_ADDRS: [u8; 2] = [0x68, 0x69];
const MPU_REG_PWR_MGMT_1: u8 = 0x6B;
const MPU_REG_WHO_AM_I: u8 = 0x75; // 期待値 0x70
const MPU_REG_ACCEL_XOUT_H: u8 = 0x3B; // 6バイト: Accel X/Y/Z
const MPU_REG_GYRO_XOUT_H: u8 = 0x43; // 6バイト: Gyro X/Y/Z

// --- MT6701（候補） ---
// 実機の設定/派生で異なるケースがあるため候補を複数用意。
// 1) 角度上位/下位(ビッグエンディアン) 14bit相当想定
const MT6701_CANDIDATES: &[(u8, u8, u8)] = &[
    // (start_addr_msb, start_addr_lsb, bits)
    (0x0E, 0x0F, 14),
    (0x03, 0x04, 14),
    (0x0C, 0x0D, 14),
];

fn i2c_write(i2c: &mut I2cDriver, addr: u8, reg: u8, data: &[u8]) -> Result<()> {
    let mut buf = Vec::with_capacity(1 + data.len());
    buf.push(reg);
    buf.extend_from_slice(data);
    i2c.write(addr, &buf, BLOCK)?;
    Ok(())
}

fn i2c_read(i2c: &mut I2cDriver, addr: u8, reg: u8, len: usize) -> Result<Vec<u8>> {
    let mut out = vec![0u8; len];
    i2c.write_read(addr, &[reg], &mut out, BLOCK)?;
    Ok(out)
}

fn mpu6500_init_and_read(i2c: &mut I2cDriver) -> Result<()> {
    for &addr in &MPU_ADDRS {
        // WHO_AM_I
        if let Ok(who) = i2c_read(i2c, addr, MPU_REG_WHO_AM_I, 1) {
            if who[0] == 0x70 {
                log::info!("MPU-6500 detected at 0x{:02X}", addr);
                // スリープ解除
                i2c_write(i2c, addr, MPU_REG_PWR_MGMT_1, &[0x00])?;

                // ちょい待ち
                thread::sleep(Duration::from_millis(50));

                // Accel
                let a = i2c_read(i2c, addr, MPU_REG_ACCEL_XOUT_H, 6)?;
                let ax = i16::from_be_bytes([a[0], a[1]]);
                let ay = i16::from_be_bytes([a[2], a[3]]);
                let az = i16::from_be_bytes([a[4], a[5]]);
                log::info!("Accel raw: x={} y={} z={}", ax, ay, az);

                // Gyro
                let g = i2c_read(i2c, addr, MPU_REG_GYRO_XOUT_H, 6)?;
                let gx = i16::from_be_bytes([g[0], g[1]]);
                let gy = i16::from_be_bytes([g[2], g[3]]);
                let gz = i16::from_be_bytes([g[4], g[5]]);
                log::info!("Gyro raw:  x={} y={} z={}", gx, gy, gz);

                return Ok(());
            }
        }
    }
    log::warn!("MPU-6500 not found on 0x68/0x69 (check AD0 pin / wiring).");
    Ok(())
}

fn try_read_mt6701_once(i2c: &mut I2cDriver, addr: u8) -> Option<(u16, u8)> {
    for _ in 0..50 {
        for &(msb_reg, lsb_reg, bits) in MT6701_CANDIDATES {
            if let (Ok(msb), Ok(lsb)) = (
                i2c_read(i2c, addr, msb_reg, 1),
                i2c_read(i2c, addr, lsb_reg, 1),
            ) {
                let raw = ((msb[0] as u16) << 8) | (lsb[0] as u16);
                // 14bit想定なら下位2bitマスクなど（安全に上位側を採用）
                let val = if bits < 16 { raw >> (16 - bits) } else { raw };
                if val != 0 {
                    return Some((val, bits as u8));
                }
            }
        }
    }
    None
}

fn scan_bus(i2c: &mut I2cDriver) {
    let mut found = Vec::new();
    for addr in 0x03..=0x77 {
        if i2c.write(addr, &[], BLOCK).is_ok() {
            found.push(addr);
        }
    }
    if found.is_empty() {
        log::warn!("I2C scan: no device found.");
    } else {
        let list = found
            .iter()
            .map(|a| format!("0x{:02X}", a))
            .collect::<Vec<_>>()
            .join(", ");
        log::info!("I2C scan found: {}", list);
    }
}

fn main() -> Result<()> {
    EspLogger::initialize_default();

    let p = Peripherals::take().unwrap();
    let i2c0 = I2cDriver::new(
        p.i2c0,
        p.pins.gpio22,
        p.pins.gpio21,
        &I2cConfig::new().baudrate(Hertz(I2C_FREQ_HZ)),
    )?;
    let mut i2c0 = i2c0;

    let i2c1 = I2cDriver::new(
        p.i2c1,
        p.pins.gpio18,
        p.pins.gpio19,
        &I2cConfig::new().baudrate(Hertz(I2C_FREQ_HZ)),
    )?;
    let mut i2c1 = i2c1;

    let mut nsleep = PinDriver::output(p.pins.gpio27)?;
    let mut left_in1 = PinDriver::output(p.pins.gpio25)?;
    let mut left_in2 = PinDriver::output(p.pins.gpio26)?;
    let mut right_in1 = PinDriver::output(p.pins.gpio33)?;
    let mut right_in2 = PinDriver::output(p.pins.gpio32)?;

    let d = Delay::new_default();

    // 1) バススキャン
    scan_bus(&mut i2c0);
    scan_bus(&mut i2c1);

    // 2) MPU-6500を読む
    mpu6500_init_and_read(&mut i2c0)?;
    mpu6500_init_and_read(&mut i2c1)?;
    
    // 1) まずドライバは寝かせたまま入力を安全状態に
    left_in1.set_low()?; left_in2.set_low()?; // コースト
    right_in1.set_low()?; right_in2.set_low()?;

    // 2) nSLEEPをHIGHに → ウェイク待ち
    nsleep.set_high()?;               // 有効化
    d.delay_ms(2);            // ★ 1～2ms待つのがミソ

    // 3) 左右とも“正転”（IN1=1, IN2=0）
    left_in1.set_high()?; left_in2.set_low()?;
    right_in1.set_high()?; right_in2.set_low()?;


    // 3) MT6701候補（スキャンで見つかった中からMPU以外）
    log::info!("Trying to read MT6701 angle candidates...");
    loop {
        let Some((val0, _)) = try_read_mt6701_once(&mut i2c0, 0x06) else {
          continue
        };
        let Some((val1, _)) = try_read_mt6701_once(&mut i2c1, 0x06) else {
          continue
        };

        log::info!("MT6701 candidate read: Bus0=0x{:04X}, Bus1=0x{:04X}", val0, val1);
        d.delay_ms(100);
    }
}
