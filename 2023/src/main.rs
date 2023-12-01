use advent_2023::*;
use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    #[arg(value_parser = clap::value_parser!(u16).range(1..=25))]
    day: i32,

    #[arg(value_enum, short, long, default_value_t)]
    part: Part,
}

fn main() {
    let Args { day, part } = Args::parse();

    println!("Solving for day {day}, part {part:?}");
    let solution = solve(day, part);

    dbg!(solution);
}
