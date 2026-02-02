# This R script generates n observations from a bivariate normal distribution
# and show the directions of maximum variability

set.seed(42) # set the random seed (optional)

# define parameters
mu <- c(2, 5)
Sigma <- matrix(c(4, 2.5, 2.5, 3), nrow = 2) 
n <- 500 

# this generates bivariate normal realizations using a Cholesky decomposition
# of the covariance matrix
L <- t(chol(Sigma)) 
Z <- matrix(rnorm(n * 2), nrow = 2) 
X <- mu + (L %*% Z) 
X_data <- t(X)

# Calculate eigenvectors and eigenvalues of Sigma
ev <- eigen(Sigma)
eigen_vecs <- ev$vectors
eigen_vals <- ev$values

# Set up the plot area
plot(X_data[,1], X_data[,2], 
     col = rgb(0.1, 0.2, 0.5, 0.3), # blue with transparency
     pch = 19, 
     asp = 1, # Ensure 1:1 aspect ratio to see orthogonality clearly
     xlab = "x1", ylab = "x2",
     main = "Bivariate Normal Realizations",
     sub = "Arrows indicate eigenvector directions (scaled by 2 sd)")

# Add the mean point
points(mu[1], mu[2], col = "yellow", pch = 18, cex = 2)

# Overlay eigenvectors (principal components)
scale_factor <- 2 # scale factor for visibility (2 standard deviations)

# Draw the primary eigenvector (direction of max variability)
arrows(mu[1], mu[2], 
       mu[1] + scale_factor * sqrt(eigen_vals[1]) * eigen_vecs[1,1], 
       mu[2] + scale_factor * sqrt(eigen_vals[1]) * eigen_vecs[2,1], 
       col = "red", lwd = 3, length = 0.1)

# Draw the secondary eigenvector
arrows(mu[1], mu[2], 
       mu[1] + scale_factor * sqrt(eigen_vals[2]) * eigen_vecs[1,2], 
       mu[2] + scale_factor * sqrt(eigen_vals[2]) * eigen_vecs[2,2], 
       col = "darkgreen", lwd = 3, length = 0.1)

# Add a legend
legend("topleft", legend=c("Max Variance (PC1)", "Min Variance (PC2)"),
       col=c("red", "darkgreen"), lwd=3, bty="n")