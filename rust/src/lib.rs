pub fn blank_lines(s: &str) -> Vec<Vec<&str>> {
    let (mut agg, final_group) =
        s.lines()
            .fold((Vec::new(), Vec::new()), |(mut agg, mut grouped), line| {
                if line.is_empty() {
                    agg.push(grouped);
                    (agg, Vec::new())
                } else {
                    grouped.push(line);
                    (agg, grouped)
                }
            });
    agg.push(final_group);
    agg
}
