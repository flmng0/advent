pub struct DayN;
pub type Impl = DayN;

impl crate::Solver for DayN {
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
    fn dayN_a() {
        let input = r#""#.to_string();
        let output = "".to_string();

        let solver = DayN::new();
        let result = solver.part_a(input);

        assert_eq!(result, output);
    }

    #[test]
    fn dayN_b() {
        let input = r#""#.to_string();
        let output = "".to_string();

        let solver = DayN::new();
        let result = solver.part_b(input);

        assert_eq!(result, output);
    }
}
