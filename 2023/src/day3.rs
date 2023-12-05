use std::collections::HashSet;

use crate::Solver;

pub struct Day3;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
struct Number {
    x: i32,
    y: i32,
    width: usize,
    value: u32,
}

impl Number {
    fn touches(&self, x: i32, y: i32) -> bool {
        return x >= self.x && x < self.x + self.width as i32 && y == self.y;
    }
}

#[derive(Debug)]
struct Symbol {
    x: i32,
    y: i32,
    value: char,
}

#[derive(Debug)]
struct Board {
    numbers: Vec<Number>,
    symbols: Vec<Symbol>,
}

impl Board {
    fn parse(input: String) -> Self {
        let lines = input.split_terminator('\n');

        let mut symbols = Vec::new();
        let mut numbers = Vec::new();
        let mut skip = 0;

        for (y, line) in lines.enumerate() {
            for (x, val) in line.chars().enumerate() {
                if skip > 0 {
                    skip = skip - 1;
                    continue;
                }

                let (x, y) = (x as i32, y as i32);

                match val {
                    v if v.is_digit(10) => {
                        let num: String = line
                            .chars()
                            .skip(x as usize)
                            .take_while(|c| c.is_digit(10))
                            .collect();
                        let width = num.len();
                        let value = num.parse::<u32>().expect("Failed to parse number");

                        numbers.push(Number { x, y, width, value });
                        skip = width - 1;
                    }
                    '.' => {}
                    _ => symbols.push(Symbol { x, y, value: val }),
                }
            }
        }

        Board { numbers, symbols }
    }
}

impl Solver for Day3 {
    fn part_a(&self, input: String) -> String {
        let board = Board::parse(input);

        #[rustfmt::skip]
        let boundary = [
            (-1, -1), (0, -1), (1, -1),
            (-1, 0), /*(0, 0),*/ (1, 0),
            (-1, 1), (0, 1), (1, 1),
        ];

        let mut touching = HashSet::new();

        for symbol in board.symbols {
            let boundary = boundary.map(|(dx, dy)| (symbol.x + dx, symbol.y + dy));

            for num in board.numbers.iter() {
                for (x, y) in boundary {
                    if num.touches(x, y) {
                        touching.insert(num);
                        break;
                    }
                }
            }
        }

        let total = touching.into_iter().fold(0, |acc, x| acc + x.value);

        format!("{total}")
    }

    fn part_b(&self, input: String) -> String {
        let board = Board::parse(input);

        #[rustfmt::skip]
        let boundary = [
            (-1, -1), (0, -1), (1, -1),
            (-1, 0), /*(0, 0),*/ (1, 0),
            (-1, 1), (0, 1), (1, 1),
        ];

        let mut total = 0;

        for symbol in board.symbols {
            let mut touching = HashSet::new();
            let boundary = boundary.map(|(dx, dy)| (symbol.x + dx, symbol.y + dy));

            for num in board.numbers.iter() {
                for (x, y) in boundary {
                    if num.touches(x, y) {
                        touching.insert(num);
                        break;
                    }
                }
            }

            if touching.len() == 2 {
                total += touching.iter().fold(1, |acc, x| acc * x.value);
            }
        }

        format!("{total}")
    }
}

pub fn init() -> Day3 {
    Day3
}

#[cfg(test)]
mod tests {

    use super::*;
    #[test]
    fn day3_a() {
        let input = r#"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"#
        .to_string();
        let output = "4361".to_string();

        let solver = Day3;
        let result = solver.part_a(input);
        assert_eq!(result, output);
    }

    #[test]
    fn day3_b() {
        let input = r#"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"#
        .to_string();
        let output = "467835".to_string();

        let solver = Day3;
        let result = solver.part_b(input);
        assert_eq!(result, output);
    }
}
