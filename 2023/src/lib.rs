use std::path::PathBuf;

mod day1;

trait Solver {
    fn part_a(&self, input: String) -> String;
    fn part_b(&self, input: String) -> String;
}

macro_rules! day {
    ($mod: ident) => {
        Some(Box::new($mod::init()))
    };
}

fn get_solver(day: i32) -> Option<Box<dyn Solver>> {
    match day {
        1 => day!(day1),
        _ => None,
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum)]
pub enum Part {
    #[default]
    A,
    B,
}

pub fn solve(day: i32, part: Part) -> String {
    let mut path = PathBuf::new();
    path.push("../inputs/2023");
    path.push(format!("day{day}.txt"));

    let input = std::fs::read_to_string(path).expect("Input not found");

    match get_solver(day) {
        Some(s) => match part {
            Part::A => s.part_a(input),
            Part::B => s.part_b(input),
        },
        None => format!("Day {day} solver not yet implemented"),
    }
}
