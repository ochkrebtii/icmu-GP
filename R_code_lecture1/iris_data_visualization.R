# Load the Iris dataset
data(iris)

# Filter for the Setosa species (columns 1 through 4)
setosa_data <- iris[iris$Species == "setosa", 1:4]

# Clean up variable names for the plot
colnames(setosa_data) <- c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width")

# Create the scatterplot matrix
pairs(setosa_data, 
      main = "Scatterplot Matrix for Iris Setosa",
      pch = 21,              # Circle symbol
      bg = "skyblue",        # Fill color
      col = "darkblue",      # Border color
      upper.panel = panel.smooth, # Adds a trend line to the upper triangles
      diag.panel = function(x, ...) {
        # Adds a histogram on the diagonal
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5))
        h <- hist(x, plot = FALSE, breaks = 15)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = "skyblue", border = "white")
      })