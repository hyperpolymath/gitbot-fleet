// Test sample for sustainabot analysis

fn efficient_function(x: i32) -> i32 {
    x * 2
}

fn nested_loops_function(data: Vec<Vec<i32>>) -> i32 {
    let mut sum = 0;
    for row in &data {
        for col in row {
            for _ in 0..10 {
                sum += col;
            }
        }
    }
    sum
}
