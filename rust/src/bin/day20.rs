use rust::blank_lines;

struct Tile {
    id: usize,
    contents: Vec<String>,
}

impl Tile {
    fn border_edges(&self) -> Vec<String> {
        let right = self.right();
        let right_f = flip(&right);
        let left = self.left();
        let left_f = flip(&left);
        let up = self.up();
        let up_f = flip(&up);
        let down = self.down();
        let down_f = flip(&down);
        vec![left, left_f, right, right_f, up, up_f, down, down_f]
    }
    fn left(&self) -> String {
        self.contents
            .iter()
            .map(|line| line.chars().next().unwrap())
            .collect()
    }
    fn right(&self) -> String {
        self.contents
            .iter()
            .map(|line| line.chars().last().unwrap())
            .collect()
    }
    fn up(&self) -> String {
        self.contents[0].clone()
    }
    fn down(&self) -> String {
        self.contents.last().unwrap().clone()
    }
}

fn flip(s: &str) -> String {
    s.chars().rev().collect()
}

fn main() {
    println!("{}", part1(include_str!("../../data/day20.txt")));
}

fn lines_to_tile(lines: &[&str]) -> Tile {
    let id = lines[0]
        .split_ascii_whitespace()
        .nth(1)
        .unwrap()
        .strip_suffix(':')
        .unwrap()
        .parse()
        .unwrap();
    let contents = lines[1..].iter().map(|line| line.to_string()).collect();
    Tile { id, contents }
}

fn missing_edges(t: &Tile, tiles: &[Tile]) -> usize {
    let edges = [t.left(), t.right(), t.up(), t.down()];
    4 - edges
        .iter()
        .filter(|e| {
            tiles
                .iter()
                .filter(|other| other.id != t.id)
                .any(|other_t| other_t.border_edges().contains(e))
        })
        .count()
}

fn part1(s: &str) -> usize {
    let chunks = blank_lines(s);
    let tiles = chunks
        .iter()
        .map(|chunk| lines_to_tile(&chunk))
        .collect::<Vec<_>>();

    // 12 * 12 tile

    let corners = tiles
        .iter()
        .filter(|t| missing_edges(t, &tiles) == 2)
        .map(|u| u.id)
        .take(4);
    corners.product()
}

#[test]
fn test_sample() {
    let input = r#"Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."#;

    let product = 20899048083289;
    assert_eq!(part1(input), product);
}
