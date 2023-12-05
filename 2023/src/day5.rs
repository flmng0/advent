pub struct Day5;
pub type Impl = Day5;

impl crate::Solver for Day5 {
    fn new() -> Self {
        Self
    }

    fn part_a(&self, input: String) -> String {
        let _ = input;
        todo!()
    }

    fn part_b(&self, input: String) -> String {
        let _ = input;
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Solver;

    #[test]
    fn day5_a() {
        let input = r#""#.to_string();
        let output = "".to_string();

        let solver = Day5::new();
        let result = solver.part_a(input);

        assert_eq!(result, output);
    }

    #[test]
    fn day5_b() {
        let input = r#""#.to_string();
        let output = "".to_string();

        let solver = Day5::new();
        let result = solver.part_b(input);

        assert_eq!(result, output);
    }
}
