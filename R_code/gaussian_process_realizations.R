# Load library for Multivariate Normal sampling
if (!require("MASS")) install.packages("MASS")
library(MASS)

# 1. Set Parameters from the provided text
sigma_sq <- 1.0   # Variance (sigma^2)
l <- 0.15          # Length-scale (phi)
n_grid <- 50      # Resolution

# 2. Define the spatial domain D = [0, 1] x [0, 1]
x <- seq(0, 1, length.out = n_grid)
y <- seq(0, 1, length.out = n_grid)
grid <- expand.grid(x = x, y = y)

# 3. Compute Isotropic Covariance Matrix (Definition 2.1.2)
# Calculate Euclidean distance matrix ||s - t||
dist_matrix <- as.matrix(dist(grid))

# Squared Exponential (Gaussian) Covariance
K <- sigma_sq * exp(-(dist_matrix^2) / (2 * l^2))
K <- K + diag(1e-8, nrow(grid)) # Jitter for numerical stability

# 4. Sample 4 realizations of the Gaussian Process (Definition 2.1.9)
set.seed(123)
# Sample from MVN with mean vector 0
z_samples <- mvrnorm(n = 4, mu = rep(0, nrow(grid)), Sigma = K)

# 5. Plotting in a 2x2 grid
# pty = "s" ensures the entire plot region is square
par(mfrow = c(2, 2), pty = "s", mar = c(4, 4, 2, 1))

for (i in 1:4) {
  Z_matrix <- matrix(z_samples[i, ], nrow = n_grid, ncol = n_grid)
  
  # Heatmap rendering
  image(x, y, Z_matrix, 
        col = terrain.colors(100), 
        main = paste("Realization", i),
        xlab = "s1", ylab = "s2")
}