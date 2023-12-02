use crate::Solver;

pub struct Day2;

#[derive(Default, Debug)]
struct Round {
    red: u32,
    green: u32,
    blue: u32,
}

#[derive(Debug)]
struct Game {
    id: u32,
    rounds: Vec<Round>,
}

impl Game {
    fn parse(line: &str) -> Self {
        let (head, tail) = line.split_once(':').expect("invalid line");
        let id = head
            .strip_prefix("Game ")
            .expect("no game prefix")
            .parse::<u32>()
            .expect("failed to parse game id");

        let rounds = tail
            .split(';')
            .map(|r| {
                r.split(',')
                    .fold(Round::default(), |acc, h| match h.trim().split_once(' ') {
                        Some((count, "red")) => Round {
                            red: acc.red + count.parse::<u32>().expect("failed to parse red count"),
                            ..acc
                        },
                        Some((count, "green")) => Round {
                            green: acc.green
                                + count.parse::<u32>().expect("failed to parse green count"),
                            ..acc
                        },

                        Some((count, "blue")) => Round {
                            blue: acc.blue
                                + count.parse::<u32>().expect("failed to parse blue count"),
                            ..acc
                        },

                        _ => acc,
                    })
            })
            .collect();

        let game = Game { id, rounds };

        game
    }
}

impl Solver for Day2 {
    fn part_a(&self, input: String) -> String {
        let games = input.split_terminator('\n').map(Game::parse);

        let valid = games.filter(|g| {
            let is_valid = g
                .rounds
                .iter()
                .all(|r| r.red <= 12 && r.green <= 13 && r.blue <= 14);

            is_valid
        });

        let ids = valid.map(|g| g.id);
        let total = ids.fold(0, |acc, x| acc + x);

        format!("{total}")
    }

    fn part_b(&self, input: String) -> String {
        let games = input.split_terminator('\n').map(Game::parse);

        let powers = games.map(|g| {
            let mins = g.rounds.iter().fold(Round::default(), |acc, r| Round {
                red: acc.red.max(r.red),
                green: acc.green.max(r.green),
                blue: acc.blue.max(r.blue),
            });

            mins.red * mins.green * mins.blue
        });

        let total = powers.fold(0, |acc, x| acc + x);

        format!("{total}")
    }
}

pub fn init() -> Day2 {
    Day2
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn day2_a() {
        let input = r#"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"#
        .to_string();
        let result = "8".to_string();

        let solver = Day2;
        let output = dbg!(solver.part_a(input));

        assert_eq!(output, result);
    }

    #[test]
    fn day2_b() {
        let input = r#"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"#
        .to_string();
        let result = "2286".to_string();

        let solver = Day2;
        let output = dbg!(solver.part_b(input));

        assert_eq!(output, result);
    }
}
