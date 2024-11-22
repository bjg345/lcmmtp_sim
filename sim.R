# Get command-line arguments
args <- commandArgs(trailingOnly = TRUE)
n <- as.integer(args[1])
U <- as.numeric(args[2])
V <- as.numeric(args[3])
task_id <- as.integer(args[4])  # Task ID, used to select the seed

filename <- sprintf("results/results_n%d_U%s_V%s_task%d.rds", n, U, V, task_id)
if(file.exists(filename)){
   quit()
}

library(lcmmtp)
library(mlr3extralearners)

# Load the seeds vector from seeds.rds and select the task-specific seed
seeds <- readRDS("seeds.rds")
seed <- seeds[task_id]

# Set up RNG for reproducibility using the selected seed
set.seed(seed)

# Ensure results directory exists
if (!dir.exists("results")) {
  dir.create("results")
}

# Make variable names
vars <- lcmmtp:::lcmmtp_variables$new(
  L = list(c('l1'), c('l2')),
  A = c('a1', 'a2'),
  Z = list(c('z1'), c('z2')),
  M = c('m1', 'm2'),
  Y = "y",
  cens = c('c1', 'c2')
)

# Define intervention
d_int <- function(data, trt) {
  if (trt == 'a2') {
    return(rep(1, nrow(data)))
  } else if (trt == 'a1') {
    return(data[[trt]])
  }
}

# Define "status quo"
d_sq <- function(data, trt) {
  return(data[[trt]])
}

# Get functions for data generation
source('functions.R')

print(paste("Running simulation with seed:", seed))

# Generate data
l1 <- gen_l1(n)
a1 <- gen_a1(l1, n)
z1 <- gen_z1(l1, a1, n)
m1 <- gen_m1(l1, a1, z1, n)
l2 <- gen_l2(l1, a1, z1, m1, n)
a2 <- gen_a2(l1, a1, z1, m1, l2, n)
z2 <- gen_z2(l1, a1, z1, m1, l2, a2, n)
m2 <- gen_m2(l1, a1, z1, m1, l2, a2, z2, n)
y <- gen_y(l1, a1, z1, m1, l2, a2, z2, m2, U, V, n)

df <- data.frame(l1=l1, a1=a1, z1=z1, m1=m1, l2=l2, a2=a2, z2=z2, m2=m2, y=y, c1=1, c2=1)

# Fit model
folds <- 3
lrnrs <- c('mean', 'glm', 'earth', 'ranger')

fit <- lcmmtp(df, vars, d_int, d_sq, id = NULL, .lcmmtp_control(folds = folds,
                                                                     learners_trt = lrnrs,
                                                                     learners_mediator = lrnrs,
                                                                     learners_QL = lrnrs,
                                                                     learners_QZ = lrnrs,
                                                                     learners_QM = lrnrs))

# Save the list as an RDS file with a unique filename based on the parameters
saveRDS(fit, file = filename)

cat("Saved results for n =", n, "U =", U, "V =", V, "with seed =", seed, "\n")

