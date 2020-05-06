#[derive(Debug)]
pub struct HighScores<'a> {
    pub scores: &'a [u32],
}

impl<'a> HighScores<'a> {
    pub fn new(scores: &'a [u32]) -> Self {
        HighScores { scores: scores }
    }

    pub fn scores(&self) -> &[u32] {
        self.scores
    }

    pub fn latest(&self) -> Option<u32> {
        self.scores.last().map(|&x| x)
    }

    pub fn personal_best(&self) -> Option<u32> {
        self.scores.iter().max().map(|&x| x)
    }

    pub fn personal_top_three(&self) -> Vec<u32> {
        let mut res: Vec<&u32> = self.scores.iter().collect();
        res.sort_by(|a, b| b.cmp(a));
        res.iter().take(3).map(|&x| *x).collect::<Vec<u32>>()
    }
}
