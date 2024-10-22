use std::f64::consts::E;

fn log2_approx(x: f64) -> f64 {
    let mut sum = 0.0;
    let term = (x - 1.0) / (x + 1.0);
    for i in 1..=100 {
        sum += term.powf(2.0 * i as f64 - 1.0) / (2.0 * i as f64 - 1.0);
    }
    2.0 * sum
}

fn compute_power_of_2(n: f64) -> f64 {
    let log2_approx_exp = n * log2_approx(2.0);
    E.powf(log2_approx_exp)
}

fn main() {
    let n = 265.754; // Example for large exponent
    println!("{}", compute_power_of_2(n));