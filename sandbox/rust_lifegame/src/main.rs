use anyhow::*;
use itertools::*;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;
use std::fmt;

#[derive(Debug, Clone)]
struct Field<T> {
    width: usize,
    height: usize,
    buffer: Vec<T>,
}

impl std::fmt::Display for Field<bool> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg: String = self
            .buffer
            .iter()
            .map(|&v| if v { 'o' } else { '_' })
            .chunks(self.width)
            .into_iter()
            .map(|cs| cs.collect::<String>())
            .collect::<Vec<_>>()
            .join("\n");

        write!(f, "{}", msg)?;
        Ok(())
    }
}

#[derive(Debug)]
struct Game {
    field: Field<bool>,
}

impl Game {
    fn builder() -> GameBuilder {
        GameBuilder::new()
    }

    fn start(&mut self) -> impl Iterator<Item = Field<bool>> + '_ {
        fn advance_index(i: usize, width: usize) -> usize {
            let mut ret = i + 1;
            if (i % width) == (width - 1) {
                ret -= width
            }
            ret
        }

        fn backward_index(i: usize, width: usize) -> usize {
            let mut ret = i;
            if (i % width) == 0 {
                ret += width
            }
            ret -= 1;
            ret
        }

        std::iter::from_fn(move || {
            let buffer = (0..self.field.buffer.len())
                .map(|i| {
                    let i4 = i;
                    let i1 =
                        (i - self.field.width + self.field.buffer.len()) % self.field.buffer.len();
                    let i7 =
                        (i + self.field.width + self.field.buffer.len()) % self.field.buffer.len();
                    let i0 = backward_index(i1, self.field.width);
                    let i2 = advance_index(i1, self.field.width);
                    let i3 = backward_index(i4, self.field.width);
                    let i5 = advance_index(i4, self.field.width);
                    let i6 = backward_index(i7, self.field.width);
                    let i8_ = advance_index(i7, self.field.width);

                    let mut neighbors_count = 0;
                    neighbors_count += self.field.buffer[i0] as u8;
                    neighbors_count += self.field.buffer[i1] as u8;
                    neighbors_count += self.field.buffer[i2] as u8;
                    neighbors_count += self.field.buffer[i3] as u8;
                    neighbors_count += self.field.buffer[i5] as u8;
                    neighbors_count += self.field.buffer[i6] as u8;
                    neighbors_count += self.field.buffer[i7] as u8;
                    neighbors_count += self.field.buffer[i8_] as u8;

                    let v4 = self.field.buffer[i4];
                    (v4 && neighbors_count == 2) || neighbors_count == 3
                })
                .collect::<Vec<_>>();

            self.field = Field {
                width: self.field.width,
                height: self.field.height,
                buffer: buffer,
            };
            Some(self.field.clone())
        })
    }
}

#[derive(Debug)]
struct GameBuilder {
    width: Option<usize>,
    height: Option<usize>,
    buffer: Option<Vec<bool>>,
}

impl GameBuilder {
    fn new() -> Self {
        Self {
            width: None,
            height: None,
            buffer: None,
        }
    }

    fn set_width(&mut self, width: usize) -> &Self {
        self.width = Some(width);
        self
    }

    fn set_height(&mut self, height: usize) -> &Self {
        self.height = Some(height);
        self
    }

    fn set_buffer(&mut self, buffer: Vec<bool>) -> &Self {
        self.buffer = Some(buffer);
        self
    }

    fn build(self) -> Result<Game> {
        let width = self.width.context("width is missing")?;
        let height = self.height.context("height is missing")?;
        let buffer = self.buffer.unwrap_or(vec![false; width * height]);
        if buffer.len() != (width * height) {
            bail!(
                "given buffer is wrong size:{} != {}",
                buffer.len(),
                width * height
            );
        }

        Ok(Game {
            field: Field {
                width: width,
                height: height,
                buffer: buffer,
            },
        })
    }
}

#[derive(Debug)]
enum RLEItem {
    Dead,
    Alive,
    EoL,
    EoF,
    Count(usize),
}

impl FromStr for RLEItem {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        match s {
            "b" => Ok(RLEItem::Dead),
            "o" => Ok(RLEItem::Alive),
            "$" => Ok(RLEItem::EoL),
            "!" => Ok(RLEItem::EoF),
            _ => Err(anyhow!("")),
        }
    }
}

fn parse_param(s: &str, builder: &mut GameBuilder) -> Result<()> {
    s.trim()
        .split(",")
        .flat_map(|s| s.split("="))
        .map(|s| s.trim().to_string())
        .chunks(2)
        .into_iter()
        .filter_map(|c| c.collect_tuple::<(String, String)>())
        .map(|(k, v)| match k.as_str() {
            "x" => v
                .parse()
                .map(|x| {
                    builder.set_width(x);
                })
                .context(format!("invalid parameter value found:{} = {}", k, v)),
            "y" => v
                .parse()
                .map(|y| {
                    builder.set_height(y);
                })
                .context(format!("invalid parameter value found:{} = {}", k, v)),
            "rule" => {
                eprintln!("Ignore specified rule:{}", v);
                Ok(())
            }
            _ => Err(anyhow!("unknown parameter found:{:?}", k)),
        })
        .fold(Ok(()), |acc: Result<()>, e| match e {
            Ok(_) => acc,
            Err(e_) => acc
                .map_err(|acc_e| anyhow!("{:?},{:?}", acc_e, e_))
                .and_then(|_| Err(anyhow!("{:?}", e_))),
        })
}

fn load_rle(mut file: File) -> anyhow::Result<Game> {
    let mut buf = String::new();
    let _size = file.read_to_string(&mut buf)?;

    let mut builder = Game::builder();
    let mut line_iter = buf.split("\n").filter(|&s| !s.starts_with("#"));

    line_iter
        .next()
        .context("fail to read line")
        .and_then(|s| parse_param(s, &mut builder))?;

    let width = builder.width.unwrap();
    let buffer = line_iter
        .fold(String::new(), |acc, l| acc + l)
        .chars()
        .group_by(|c| c.is_numeric())
        .into_iter()
        .map(|(k, g)| {
            if k {
                g.collect::<String>()
                    .parse()
                    .map(|count| RLEItem::Count(count))
                    .into_iter()
                    .collect::<Vec<_>>()
            } else {
                g.filter_map(|c| RLEItem::from_str(c.to_string().as_ref()).ok())
                    .collect::<Vec<_>>()
            }
        })
        .flatten()
        .scan((0, 1), |st, item| {
            let (pos, l) = *st;
            let (pos, l, v, n) = match item {
                RLEItem::Dead => (pos + l, 1, false, l),
                RLEItem::Alive => (pos + l, 1, true, l),
                RLEItem::EoL | RLEItem::EoF => {
                    let n = (width - pos) + (l - 1) * width;
                    (0, 1, false, n)
                }
                RLEItem::Count(c) => (pos, c, false, 0),
            };
            *st = (pos, l);
            Some(vec![v; n])
        })
        .flatten()
        .collect::<Vec<_>>();
    builder.set_buffer(buffer);

    builder.build()
}

fn main() -> anyhow::Result<()> {
    let file = File::open("Banks-I-demo.rle")?;
    let mut game = load_rle(file)?;
    //dbg!(&game);
    let mut it = game.start();
    for _ in 0..10 {
        println!("{:}", it.next().unwrap());
        println!("");
    }
    Ok(())
}
