# Load library for sampling
if (!require("MASS")) install.packages("MASS")
library(MASS)

# Define the Matern covariance function
# nu controls smoothness; l controls length-scale
matern_cov <- function(r, nu, l = 1) {
  if (r == 0) return(1)
  # Formula using modified Bessel function of the second kind (K_nu)
  part1 <- (2^(1 - nu)) / gamma(nu)
  part2 <- (sqrt(2 * nu) * r / l)^nu
  part3 <- besselK(sqrt(2 * nu) * r / l, nu)
  return(part1 * part2 * part3)
}

# Define squared exponential covariance 
sq_exp_cov <- function(r, l = 1) {
  return(exp(-(r^2) / (2 * l^2)))
}

# Setup plotting parameters
par(mfrow = c(1, 2), pty = "s") # "pty=s" makes both plots square
colors <- c("blue", "red", "green3")
ltys <- c(1, 2, 4) # Solid, dashed, dot-dash
r_seq <- seq(0, 3, length.out = 100)

# Plot pointwise variance versus input distance
plot(r_seq, sapply(r_seq, matern_cov, nu = 0.5), type = "l", col = colors[1], 
     lty = ltys[1], lwd = 2, ylim = c(0, 1),
     xlab = "input distance, r", ylab = "covariance, k(r)", main = "(a)")
lines(r_seq, sapply(r_seq, matern_cov, nu = 2), col = colors[2], lty = ltys[2], lwd = 2)
lines(r_seq, sapply(r_seq, sq_exp_cov), col = colors[3], lty = ltys[3], lwd = 2)
legend("topright", legend = c("nu=1/2", "nu=2", "nu -> inf"), 
       col = colors, lty = ltys, lwd = 2, cex = 0.8)

# Set up variables to plot GP realizations
x_seq <- seq(-5, 5, length.out = 500)
dist_mat <- as.matrix(dist(x_seq))
set.seed(42)

# Create an empty plot
plot(NULL, xlim = c(-5, 5), ylim = c(-3, 3), 
     xlab = "input, x", ylab = "output, f(x)", main = "(b)")

# Function to sample and plot GP realizations
plot_path <- function(K, color, lty) {
  # Add jitter for numerical stability
  K_stable <- K + diag(1e-8, nrow(K))
  y <- mvrnorm(n = 1, mu = rep(0, length(x_seq)), Sigma = K_stable)
  lines(x_seq, y, col = color, lty = lty, lwd = 1.5)
}

# Realization for nu=1/2
K_1 <- apply(dist_mat, c(1,2), matern_cov, nu = 0.5)
plot_path(K_1, colors[1], ltys[1])

# Realization for nu=2
K_2 <- apply(dist_mat, c(1,2), matern_cov, nu = 2)
plot_path(K_2, colors[2], ltys[2])

# Realization for Squared Exponential (nu -> Inf)
K_inf <- sq_exp_cov(dist_mat)
plot_path(K_inf, colors[3], ltys[3])