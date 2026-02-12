# Note that this code may take a few minutes to render the figures

# Load required libraries
library(sf)
library(giscoR)
library(ggplot2)
library(dplyr)

station_data <- data.frame(
  station = c("KYIV", "LVIV", "KHARKIV", "ODESA", "DNIPROPETROVSK", 
              "CHERNIHIV", "IVANO-FRANKIVSK", "POLTAVA", "SUMY", "ZAPORIZHZHYA"),
  lat = c(50.40, 49.81, 49.92, 46.43, 48.36, 51.48, 48.88, 49.61, 50.85, 47.82),
  lng = c(30.45, 23.95, 36.28, 30.77, 35.10, 31.23, 24.69, 34.49, 34.67, 35.22),
  temp = c(2.5, 1.2, -3.4, 5.8, -1.1, 0.5, 2.1, -0.8, -2.5, 1.4) 
)

# Define data and prior hyperparameters
X_obs <- as.matrix(station_data[, c("lng", "lat")])
y_obs <- station_data$temp
ell <- 2.0  # Length-scale
alphasq <- 10.0 # prior variance
sigsq <- 0.5  # Noise variance

# Create a dense grid of inputs for plotting
grid_points <- expand.grid(
  lng = seq(22, 41, length.out = 150), 
  lat = seq(44, 53, length.out = 150)
)
X_star <- as.matrix(grid_points)

# Square exponential covariance with prior variance alphasq
calc_cov <- function(x1, x2, l, alphasq) {
  dists <- as.matrix(dist(rbind(x1, x2)))
  dists <- dists[1:nrow(x1), (nrow(x1) + 1):ncol(dists)]
  return(alphasq * exp(-0.5 * (dists^2) / l^2))
}

# Calculate posterior mean
C_yy <- calc_cov(X_obs, X_obs, ell, alphasq) + sigsq * diag(nrow(X_obs))
C_star_obs <- calc_cov(X_star, X_obs, ell, alphasq)
m0 <- 0
post_mean <- m0 + C_star_obs %*% solve(C_yy, y_obs - m0)
grid_points$predicted_temp <- as.numeric(post_mean)

# Find the grid points that lie on the territory of Ukraine
ukraine_outline <- gisco_get_countries(country = "Ukraine", resolution = "1")
grid_sf <- st_as_sf(grid_points, coords = c("lng", "lat"), crs = 4326)
grid_ukraine <- st_intersection(grid_sf, ukraine_outline)

# Convert this back to dataframe for geom_raster
plot_df <- grid_ukraine %>%
  mutate(lng = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# Plot posterior mean
ggplot() +
  # Use geom_raster for the solid mean field
  geom_raster(data = plot_df, aes(x = lng, y = lat, fill = predicted_temp), interpolate = TRUE) +
  # Add outline and stations
  geom_sf(data = ukraine_outline, fill = NA, color = "black", size = 0.5) +
  geom_point(data = station_data, aes(x = lng, y = lat), shape = 3, size = 2) +
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Posterior Predictive Mean", fill = "Temp (°C)")


# Compute prior variances first
# We only need the diagonal for marginal variance to save memory
C_star_star_diag <- rep(alphasq, nrow(X_star)) 

# Solve the linear system for the variance term: C_star_obs * inv(C_yy) * C_obs_star
# We do this row by row (or via cross-products) to get the diagonal efficiently
temp_solve <- solve(C_yy, t(C_star_obs))
post_var <- C_star_star_diag - colSums(t(C_star_obs) * temp_solve)

# Add variance and standard deviation to the grid
grid_points$variance <- as.numeric(post_var)
grid_points$sd <- sqrt(pmax(0, grid_points$variance))

# # process the variables for plotting
grid_sf <- st_as_sf(grid_points, coords = c("lng", "lat"), crs = 4326)
grid_ukraine <- st_intersection(grid_sf, ukraine_outline)

# Convert this back to dataframe
plot_df_var <- grid_ukraine %>%
  mutate(lng = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# Plot posterior variances
ggplot() +
  # Use geom_raster for a solid color surface of uncertainty
  geom_raster(data = plot_df_var, aes(x = lng, y = lat, fill = variance), interpolate = TRUE) +
  # Use a different color scale (e.g., Viridis) to distinguish from temperature
  scale_fill_viridis_c(option = "plasma") + 
  geom_sf(data = ukraine_outline, fill = NA, color = "white", size = 0.5) +
  geom_point(data = station_data, aes(x = lng, y = lat), color = "white", shape = 3) +
  theme_minimal() +
  labs(
    title = "Posterior Predictive Variance (point-wise)",
    fill = "Variance",
    x = "Longitude", y = "Latitude"
  )
