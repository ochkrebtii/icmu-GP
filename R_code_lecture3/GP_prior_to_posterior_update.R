# Load library for multivariate normal sampling
if (!require("MASS")) install.packages("MASS")
library(MASS)

set.seed(123) # optional random seed

# Set up parameters
t_grid <- seq(-5, 5, length.out = 200) # Input 't'
n_samples <- 3                        # Number of function draws
sf2 <- 1.0                            # prior variance
sn2 <- 0.01                           # error variance
ell <- c(1.0, 1.5)                    # lengthscales for plots

# Define covariance function
calc_covariance <- function(t1, t2, l, sf2) {
  outer(t1, t2, function(a, b) sf2 * exp(-0.5 * (a - b)^2 / l^2))
}

# Create plots
par(mfrow = c(2, 2)) # plot both panels on the same plot area

for (i in 1:2) {
  
    # --- PLOT (a): PRIOR ---
    C_prior <- calc_covariance(t_grid, t_grid, ell[i], sf2)
    # Draw samples from the prior: N(0, C)
    prior_samples <- mvrnorm(n_samples, mu = rep(0, length(t_grid)), Sigma = C_prior)
    
    plot(t_grid, rep(0, length(t_grid)), type="n", ylim=c(-2.5, 2.5), 
         xlab="t", ylab="X(t)", main=paste("prior with l =", ell[i]))
    # Add uncertainty ribbon (2 standard deviations)
    polygon(c(t_grid, rev(t_grid)), c(2*sqrt(diag(C_prior)), rev(-2*sqrt(diag(C_prior)))), 
            col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)
    matlines(t_grid, t(prior_samples), lty = 1, col = c("red", "green", "blue"))
    
    # --- PLOT (b): POSTERIOR ---
    # Observed data (the '+' markers in your image)
    t_obs <- c(-4, -3, -1, 0, 2)
    y_obs <- c(-2, 0, 1, 2, -1)
    
    # Compute components for conditioning
    C_obs  <- calc_covariance(t_obs, t_obs, ell[i], sf2) + sn2 * diag(length(t_obs))
    C_star <- calc_covariance(t_grid, t_obs, ell[i], sf2)
    C_grid <- calc_covariance(t_grid, t_grid, ell[i], sf2)
    
    # Predictive mean and covariance (see notes)
    f_mean <- C_star %*% solve(C_obs) %*% y_obs
    C_post <- C_grid - C_star %*% solve(C_obs) %*% t(C_star)
    
    # Draw samples from the posterior
    post_samples <- mvrnorm(n_samples, mu = f_mean, Sigma = C_post)
    
    plot(t_grid, f_mean, type="n", ylim=c(-2.5, 2.5), 
         xlab="t", ylab="X(t)", main=paste("posterior with l =", ell[i]))
    
    # Add point-wise evalautions of the posterior variance
    sd_post <- sqrt(pmax(0, diag(C_post)))
    polygon(c(t_grid, rev(t_grid)), c(f_mean + 2*sd_post, rev(f_mean - 2*sd_post)), 
            col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)
    matlines(t_grid, t(post_samples), lty = 1, col = c("red", "green", "blue"))
    points(t_obs, y_obs, pch = 3, lwd = 2, cex = 1.5) # The '+' markers
    
}


