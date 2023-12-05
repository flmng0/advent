use std::collections::{HashMap, HashSet};

pub struct Day4;
pub type Impl = Day4;

#[derive(Debug)]
struct Card {
    id: usize,
    winning: Vec<i32>,
    have: Vec<i32>,
}

impl Card {
    fn parse(line: &str) -> Card {
        let (head, tail) = line.split_once(':').expect("invalid line");
        let id = head
            .strip_prefix("Card ")
            .expect("no game prefix")
            .trim()
            .parse()
            .expect("failed to parse card id");

        let (w, h) = tail.split_once('|').expect("invalid nums");

        let winning = w
            .split_whitespace()
            .map(|n| n.trim().parse().unwrap())
            .collect();
        let have = h
            .split_whitespace()
            .map(|n| n.trim().parse().unwrap())
            .collect();

        Card { id, winning, have }
    }

    fn count_wins(&self) -> usize {
        let unique_w: HashSet<_> = self.winning.iter().collect();
        let unique_h: HashSet<_> = self.have.iter().collect();

        unique_w.intersection(&unique_h).count()
    }
}

impl crate::Solver for Day4 {
    fn new() -> Self {
        Self
    }

    fn part_a(&self, input: String) -> String {
        let cards = input.split_terminator('\n').map(Card::parse);

        let scores = cards.map(|c| match c.count_wins() {
            0 => 0,
            n => 1 << (n - 1),
        });

        let total = scores.sum::<usize>();

        format!("{total}")
    }

    fn part_b(&self, input: String) -> String {
        let cards = input
            .split_terminator('\n')
            .map(Card::parse)
            .collect::<Vec<_>>();

        let mut copies: HashMap<_, _> = cards.iter().map(|c| (c.id, 1)).collect();

        for c in cards.iter() {
            let wins = c.count_wins();

            if wins == 0 {
                continue;
            }

            let times = copies.get(&c.id).cloned().unwrap();

            for id in (c.id + 1)..=(c.id + wins) {
                if let Some(n) = copies.get_mut(&id) {
                    *n += times;
                }
            }
        }

        let total = copies.iter().map(|(_, n)| n).fold(0, |acc, x| acc + x);

        format!("{total}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Solver;

    #[test]
    fn day4_a() {
        let input = r#"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"#
        .to_string();
        let output = "13".to_string();

        let solver = Day4::new();
        let result = solver.part_a(input);

        assert_eq!(result, output);
    }

    #[test]
    fn day4_b() {
        let input = r#"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"#
        .to_string();
        let output = "30".to_string();

        let solver = Day4::new();
        let result = solver.part_b(input);

        assert_eq!(result, output);
    }
}
