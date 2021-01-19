use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub struct Model {
    pub vertices: Vec<f32>,
    pub facets: Vec<u32>,
}

impl Model {
    pub fn new(path: &impl AsRef<Path>) -> Self {
        let mut vertices: Vec<f32> = Vec::new();
        let mut facets: Vec<u32> = Vec::new();
        for result in BufReader::new(File::open(path).unwrap()).lines() {
            if let Ok(l) = result.map(|l| l.split(" ").map(|s| s.to_string()).collect::<Vec<_>>()) {
                if l[0] == "v" {
                    let pos = l[1..]
                        .into_iter()
                        .filter_map(|v| v.parse::<f32>().ok())
                        .collect::<Vec<_>>();
                    if pos.len() == 3 {
                        vertices.push(pos[0]);
                        vertices.push(pos[1]);
                        vertices.push(pos[2]);
                    }
                } else if l[0] == "f" {
                    let pos = l[1..]
                        .into_iter()
                        .filter_map(|v| v.parse::<u32>().ok())
                        .collect::<Vec<_>>();
                    if pos.len() == 3 {
                        // vertex index starts with 1.
                        facets.push(pos[0] - 1);
                        facets.push(pos[1] - 1);
                        facets.push(pos[2] - 1);
                    }
                } else {
                    dbg!(&l);
                }
            }
        }

        Self { vertices, facets }
    }
}
