compute_power_of_2 <- function(n) {
  log2_approx <- function(x) {
    sum <- 0
    term <- (x - 1) / (x + 1)
    for (i in 1:100) {
      sum <- sum + (term^(2 * i - 1)) / (2 * i - 1)
    }
    return(2 * sum)
  }
  
  log2_approx_exp <- n * log2_approx(2)
  result <- exp(log2_approx_exp)
  return(result)
}

n <- 265.754 # Example for large exponent
print(compute_power_of_2(n))