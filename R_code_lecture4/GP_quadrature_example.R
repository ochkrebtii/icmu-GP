
# Set up functions
g_true <- function(t) sin(t)
true_val <- 2 # Integral of sin(t) from 0 to pi
cov_sqexp <- function(t1, t2, l) exp(-0.5 * (t1 - t2)^2 / l^2)

# Set up variables
a <- 0; b <- pi # limits of integration
n_samples <- c(3,4,5,6)
l_param <- 0.8

# Dense grid for plotting the surrogate function g(t)
t_grid <- seq(a, b, length.out = 200)

# Create a 2x2 plotting grid
par(mfrow = c(2, 2))

for (i in 1:4){
  
    # Nodes and function evaluations
    t_nodes <- seq(a, b, length.out = n_samples[i])
    y_nodes <- g_true(t_nodes)
    
    # posterior mean and variance calculation
    C <- outer(t_nodes, t_nodes, cov_sqexp, l = l_param)
    c_star <- outer(t_grid, t_nodes, cov_sqexp, l = l_param)
    C_inv <- solve(C + diag(1e-8, n_samples[i])) 
    
    mu_post <- c_star %*% C_inv %*% y_nodes
    var_post <- 1 - rowSums((c_star %*% C_inv) * c_star)
    sd_post <- sqrt(pmax(0, var_post))
    
    # Plotting results
    plot(t_grid, g_true(t_grid), type = "n", ylim = c(-0.75, 1.75), xlim = c(a,b), 
         main = paste("GP quadrature with n =", n_samples[i]), 
         xlab = "t", ylab = "g(t)")
    
    polygon(c(t_grid, rev(t_grid)), 
            c(mu_post + 2*sd_post, rev(mu_post - 2*sd_post)), 
            col = rgb(0.1, 0.5, 0.8, 0.2), border = NA)
    
    lines(t_grid, g_true(t_grid), col = "red", lty = 1, lwd = 2) # lty instead of linetype
    lines(t_grid, mu_post, col = "darkblue", lwd = 2)             
    points(t_nodes, y_nodes, col = "red", pch = 19, cex = 1)
    
    legend("topright", legend=c("g(t)", "posterior mean", "posterior variance"),
           col=c("gray", "darkblue", rgb(0.1, 0.5, 0.8, 0.2)), lty=c(2, 1, 1), lwd=2)
    
    
    # Compute the GP quadrature estimate of I
    I <- sapply(t_nodes, function(ti) {
      integrate(function(t) cov_sqexp(t, ti, l = l_param), lower = a, upper = b)$value
    })
    
    # Calculate GP quadrature weights: w = I^T * C^-1
    weights <- t(I) %*% C_inv
    
    # Sum of (weights * observations)
    quad_estimate <- sum(weights * y_nodes)
    
    # Add numerical results to the plot
    text_x <- a + 0.025
    text_y <- 1.4
    text(text_x, text_y, pos = 4, labels = paste0(
      "GP Estimate: ", round(quad_estimate, 4), "\n",
      "True Value:  ", round(true_val, 4), "\n",
      "Abs Error:   ", round(abs(true_val - quad_estimate), 6)
    ), cex = 0.8, font = 2)

    }    
