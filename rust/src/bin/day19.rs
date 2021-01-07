use regex::{Regex, RegexSet};
use rust::blank_lines;
use std::{collections::HashMap, vec};

#[derive(Debug)]
enum Data {
    Terminal(char),
    Sequence(Vec<usize>),
    Choice(Vec<usize>, Vec<usize>),
}
fn main() {
    let regexes = vec![
        r#"(\d*): "(.)""#,
        r"(\d*): (.*) \| (.*)",
        r"(\d*):((?: \d+)+)$",
    ];
    let input = include_str!("../../data/day19.txt");
    let set = RegexSet::new(&regexes).unwrap();
    let captures = regexes
        .iter()
        .map(|s| Regex::new(s))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    let mut hm = HashMap::new();
    let input = blank_lines(input);
    for line in &input[0] {
        let matched = set.matches(line).iter().next().unwrap();
        let captured = captures[matched].captures(line).unwrap();
        let key: usize = captured[1].parse().unwrap();
        let value = match matched {
            0 => Data::Terminal(captured[2].chars().nth(0).unwrap()),
            1 => Data::Choice(
                captured[2]
                    .split_ascii_whitespace()
                    .map(|num| num.parse().unwrap())
                    .collect(),
                captured[3]
                    .split_ascii_whitespace()
                    .map(|num| num.parse().unwrap())
                    .collect(),
            ),
            2 => Data::Sequence(
                captured[2]
                    .trim()
                    .split_ascii_whitespace()
                    .map(|num| num.parse().unwrap())
                    .collect(),
            ),
            _ => unreachable!(),
        };
        hm.insert(key, value);
    }
    let total = input[1].iter().filter(|s| valid(s, &hm)).count();
    println!("part1: {}", total);

    hm.insert(8, Data::Choice(vec![42], vec![42, 8]));
    hm.insert(11, Data::Choice(vec![42, 31], vec![42, 11, 31]));
    let total = input[1].iter().filter(|s| valid(s, &hm)).count();
    println!("part2: {}", total)
}

fn valid(start: &str, rules: &HashMap<usize, Data>) -> bool {
    fn valid_inner<'s>(
        starts: &[&'s str],
        rule_index: usize,
        rules: &HashMap<usize, Data>,
    ) -> Vec<&'s str> {
        let rule = &rules[&rule_index];
        starts
            .iter()
            .flat_map(|start| match rule {
                Data::Terminal(c) => start
                    .strip_prefix(*c)
                    .map(|remainder| vec![remainder])
                    .unwrap_or_else(Vec::new),
                Data::Sequence(seq) => seq.iter().fold(vec![*start], |remainders, rule_index| {
                    valid_inner(&remainders, *rule_index, rules)
                }),
                Data::Choice(left, right) => {
                    let mut left_results =
                        left.iter().fold(vec![*start], |remainders, rule_index| {
                            valid_inner(&remainders, *rule_index, rules)
                        });
                    left_results.append(
                        &mut right.iter().fold(vec![*start], |remainders, rule_index| {
                            valid_inner(&remainders, *rule_index, rules)
                        }),
                    );
                    left_results
                }
            })
            .collect()
    }
    valid_inner(&[start], 0, rules).iter().any(|v| v.is_empty())
}

#[test]
fn sample() {
    let _sample = r#"0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"#;

    use std::iter::FromIterator;
    let rules = HashMap::from_iter(vec![
        (0, Data::Sequence(vec![4, 1, 5])),
        (1, Data::Choice(vec![2, 3], vec![3, 2])),
        (2, Data::Choice(vec![4, 4], vec![5, 5])),
        (3, Data::Choice(vec![4, 5], vec![5, 4])),
        (4, Data::Terminal('a')),
        (5, Data::Terminal('b')),
    ]);

    assert!(valid("ababbb", &rules));
    assert!(valid("abbbab", &rules));
    assert!(!valid("bababa", &rules));
    assert!(!valid("aaabbb", &rules));
    assert!(!valid("aaaabbb", &rules));
}
