# Inverse logit function
ilogit <- function(x) {
  1 / (1 + exp(-x))
}

# Function to generate l1
gen_l1 <- function(samp_size = n){
  rnorm(samp_size)
}

# Function to generate a1 with constrained probability
gen_a1 <- function(l1, samp_size = n){
  p <- 1/3 + 1/3 * ilogit(.5 * l1)
  rbinom(samp_size, size = 1, prob = p)
}

# Function to generate z1
gen_z1 <- function(l1, a1, samp_size = n){
  rnorm(samp_size, mean = .5 * (-l1 + a1 - 0.5), sd = 1)
}

# Function to generate m1 with constrained probability
gen_m1 <- function(l1, a1, z1, samp_size = n){
  linear_pred <- .5 * (l1 - a1 - z1 + 0.5)
  p <- 1/3 + 1/3 * ilogit(linear_pred)
  rbinom(samp_size, size = 1, prob = p)
}

# Function to generate l2
gen_l2 <- function(l1, a1, z1, m1, samp_size = n){
  rnorm(samp_size, mean = .5 * (-l1 + a1 + z1 - m1), sd = 1)
}

# Function to generate a2 with constrained probability
gen_a2 <- function(l1, a1, z1, m1, l2, samp_size = n){
  linear_pred <- .5 * (l1 - a1 - z1 + m1 + l2)
  p <- 1/3 + 1/3 * ilogit(linear_pred)
  rbinom(samp_size, size = 1, prob = p)
}

# Function to generate z2
gen_z2 <- function(l1, a1, z1, m1, l2, a2, samp_size = n){
  rnorm(samp_size, mean = .5 * (-l1 + a1 + z1 - m1 - l2 + a2 - 0.5), sd = 1)
}

# Function to generate m2 with constrained probability
gen_m2 <- function(l1, a1, z1, m1, l2, a2, z2, samp_size = n){
  linear_pred <- .5 * (l1 - a1 - z1 + m1 + l2 - a2 - z2 + 0.5)
  p <- 1/3 + 1/3 * ilogit(linear_pred)
  rbinom(samp_size, size = 1, prob = p)
}

# Function to generate y
gen_y <- function(l1, a1, z1, m1, l2, a2, z2, m2, U, V, samp_size = n){
  rnorm(samp_size, mean = .5 * (-l1 - a1 - z1 - m1 + l2 + U*a2 - z2 + V*m2), sd = 1)
}


