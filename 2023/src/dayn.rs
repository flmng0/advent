pub struct Day4;

impl crate::Solver for Day4 {
    fn part_a(&self, input: String) -> String {
        todo!()
    }

    fn part_b(&self, input: String) -> String {
        todo!()
    }
}

pub fn init() -> Day4 {
    Day4
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Solver;

    #[test]
    fn day4_a() {
        let input = r#""#.to_string();
        let output = "".to_string();

        let solver = init();
        let result = solver.part_a(input);

        assert_eq!(result, output);
    }

    #[test]
    fn day4_b() {
        let input = r#""#.to_string();
        let output = "".to_string();

        let solver = init();
        let result = solver.part_b(input);

        assert_eq!(result, output);
    }
}
