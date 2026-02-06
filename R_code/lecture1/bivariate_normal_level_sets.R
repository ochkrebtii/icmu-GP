# Load required libraries
if (!require("mvtnorm")) install.packages("mvtnorm")
library(mvtnorm)

# Set up parameters
mu    <- c(2, 5)
Sigma <- matrix(c(4, 2.5, 2.5, 3), nrow = 2)

# Create grid and compute density
x <- seq(-4, 10, length.out = 100)
y <- seq(-1, 12, length.out = 100)
z <- matrix(dmvnorm(expand.grid(x, y), mean = mu, sigma = Sigma), nrow = 100)

# Define the levels and a color palette
n_levels <- 20
levels <- seq(min(z), max(z), length.out = n_levels)
cols   <- hcl.colors(n_levels, palette = "Viridis")

# Plot level sets
contour(x, y, z, 
        levels = levels, 
        col = cols, 
        lwd = 2,
        drawlabels = FALSE,
        main = "Bivariate Normal: Colored Contour Lines",
        xlab = expression(x[1]), ylab = expression(x[2]))

# Add center point for reference
points(mu[1], mu[2], col = "yellow", pch = 18, cex = 1)
