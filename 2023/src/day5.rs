use std::ops::Range;

pub struct Day5;
pub type Impl = Day5;

impl crate::Solver for Day5 {
    fn new() -> Self {
        Self
    }

    fn part_a(&self, input: String) -> String {
        let (seeds, tail) = input.split_once("\n\n").expect("Failed to do first split");
        let seeds = seeds
            .strip_prefix("seeds: ")
            .expect("Failed to strip prefix of seeds")
            .split_whitespace()
            .map(|s| s.parse::<u64>().unwrap());

        let chunks = tail.split_terminator("\n\n");

        let maps = chunks.map(Map::parse);
        let locations = seeds.map(|seed| {
            let location = maps.clone().fold(seed, |current, m| m.travel(current));

            location
        });

        let lowest = locations.min().expect("Failed to get min");

        format!("{lowest}")
    }

    fn part_b(&self, input: String) -> String {
        let (seeds, tail) = input.split_once("\n\n").expect("Failed to do first split");
        let seeds = seeds
            .strip_prefix("seeds: ")
            .expect("Failed to strip prefix of seeds")
            .split_whitespace()
            .map(|s| s.parse::<u64>().unwrap())
            .array_chunks()
            .flat_map(|[start, len]| (start..(start + len)));

        let chunks = tail.split_terminator("\n\n");

        let maps = chunks.map(Map::parse).collect::<Vec<Map>>();

        let mut min = u64::MAX;

        for (i, s) in seeds.enumerate() {
            let v = maps.iter().fold(s, |v, m| m.travel(v));

            min = min.min(v);

            // Only here so I don't think my computer crashed
            if i % 1_000_000 == 0 {
                println!("Progress: {i}");
            }
        }

        format!("{:?}", min)
    }
}

#[derive(Clone, Copy)]
struct Route {
    dest_start: u64,
    src_start: u64,
    width: u64,
}

impl Route {
    fn new(line: &str) -> Self {
        let parts = line
            .split_whitespace()
            .map(|p| p.parse::<u64>().expect("Failed to parse num part"))
            .collect::<Vec<_>>();

        if parts.len() != 3 {
            panic!("Invalid length of parts array, got: {}", parts.len());
        }

        Self {
            dest_start: parts[0],
            src_start: parts[1],
            width: parts[2],
        }
    }

    #[inline]
    fn dest_range(&self) -> Range<u64> {
        self.dest_start..(self.dest_start + self.width)
    }

    #[inline]
    fn src_range(&self) -> Range<u64> {
        self.src_start..(self.src_start + self.width)
    }
}

struct Map {
    routes: Vec<Route>,
}

impl Map {
    fn parse(chunk: &str) -> Self {
        let routes = chunk
            .split_terminator('\n')
            .skip(1)
            .map(Route::new)
            .collect();
        Self { routes }
    }

    fn travel(&self, value: u64) -> u64 {
        for route in self.routes.iter() {
            if route.src_range().contains(&value) {
                let offset = value - route.src_start;
                return route.dest_start + offset;
            }
        }

        value
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Solver;

    #[test]
    fn day5_a() {
        let input = r#"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"#
        .to_string();
        let output = "35".to_string();

        let solver = Day5::new();
        let result = solver.part_a(input);

        assert_eq!(result, output);
    }

    #[test]
    fn day5_b() {
        let input = r#"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"#
        .to_string();
        let output = "46".to_string();

        let solver = Day5::new();
        let result = solver.part_b(input);

        assert_eq!(result, output);
    }
}
