pub struct Day1;
pub type Impl = Day1;

impl crate::Solver for Day1 {
    fn new() -> Self {
        Self
    }

    fn part_a(&self, input: String) -> String {
        let lines = input.split_terminator('\n');

        let mut values = vec![];

        for line in lines {
            let first_idx = line.find(|c: char| c.is_digit(10)).expect("No first");
            let last_idx = line.rfind(|c: char| c.is_digit(10)).expect("No last");

            let first_num = line[first_idx..=first_idx].parse::<i32>().unwrap();
            let last_num = line[last_idx..=last_idx].parse::<i32>().unwrap();

            let res = first_num * 10 + last_num;

            values.push(res);
        }

        let res = values.into_iter().sum::<i32>();

        format!("{}", res)
    }

    fn part_b(&self, input: String) -> String {
        let lines = input.split_terminator('\n');

        let mut values = vec![];

        let words = vec![
            "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
        ];

        for line in lines {
            let mut found_words = vec![];

            for (val, word) in words.iter().enumerate().map(|(i, w)| (i as i32 + 1, w)) {
                if let Some(idx) = line.find(word) {
                    let len = word.len();
                    found_words.push((val, idx, idx + len));
                }
            }

            found_words.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

            let mut first = line.find(|c: char| c.is_digit(10)).map(|i| {
                let val = line[i..=i].parse::<i32>().expect("failed to parse first");

                (i, val)
            });
            let mut last = line.rfind(|c: char| c.is_digit(10)).map(|i| {
                let val = line[i..=i].parse::<i32>().expect("failed to parse last");
                print!("{}", val);

                (i, val)
            });

            for (val, start, _stop) in found_words.clone() {
                first = match first {
                    Some((i, v)) => {
                        if start >= i {
                            break;
                        }

                        Some((start, val))
                    }
                    None => Some((start, val)),
                }
            }

            let mut found_words_rev = vec![];

            for (val, word) in words.iter().enumerate().map(|(i, w)| (i as i32 + 1, w)) {
                if let Some(idx) = line.rfind(word) {
                    let len = word.len();
                    found_words_rev.push((val, idx, idx + len));
                }
            }
            found_words_rev.sort_by(|a, b| b.2.partial_cmp(&a.2).unwrap());

            for (val, _start, stop) in found_words_rev.clone() {
                last = match last {
                    Some((i, v)) => {
                        if stop < i + 1 {
                            break;
                        }

                        Some((stop, val))
                    }
                    None => Some((stop, val)),
                }
            }

            let first_num = first.expect("No first!").1;
            let last_num = last.expect("No last!").1;

            println!("Line {line} had first {first_num} and last {last_num}: \n\t{found_words:?}\n\t{found_words_rev:?}");

            values.push(first_num * 10 + last_num);
        }

        let res = values.into_iter().sum::<i32>();

        format!("{}", res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Solver;

    #[test]
    fn day1_a() {
        let input = r#"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"#
        .to_string();

        let solver = Day1::new();
        let out = solver.part_a(input);

        assert_eq!(out, "142".to_string());
    }

    #[test]
    fn day1_b() {
        let input = r#"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
7five7gfnnfbs4fourfive
"#
        .to_string();

        let solver = Day1::new();
        let out = solver.part_b(input);

        assert_eq!(out, "356".to_string());
    }
}
