
# Let's examine how the posterior distribution changes as the number of data points increases

plt <- function(n, prior_mean = 26, prior_sd = 2, legend = TRUE){
  # Read in the temperature data
  temp_data <- read.table("temperature.txt",header=T) 
  temp <- temp_data$Temp; N <- length(temp); 
  
  sigma_y <- 2  # assumed standard deviation of the data
  
  y <- temp[1:n]  # select the first n measurements
  post_sd <- sqrt(1/((1/prior_sd^2) + (n/sigma_y^2))) # calculate posterior sd
  post_mean <- post_sd^2*((prior_mean/prior_sd^2) + (n*mean(y)/sigma_y^2)) # calculate posterior mean		
  x <- seq(15,50,length=500)
  
  plot(x,dnorm(x,prior_mean,prior_sd), type='l', col="red", ylim=c(0,dnorm(post_mean,post_mean,post_sd)), 
       xlim = range(x), xlab="temperature", ylab="", 
       main=paste("n = ",n,", mu_0 = ", prior_mean, ", sigma_0 = ", prior_sd)) 
  
  lines(x, dnorm(x, post_mean, post_sd), col="blue")              
  
  points(y, rep(0,n), col="green", pch=19)
  
  abline(v=prior_mean, col="red")
  
  abline(v=post_mean, col="blue")
  
  abline(v=mean(y), col="green")
  
  if (legend == TRUE){
    legend("topright", dnorm(post_mean,post_mean,post_sd), c("prior & prior mean","data & sample mean","posterior & posterior mean"), 
           col=c("red","green","blue"), text.col="black",lty=1)
  }
}




# Make the plots
plt(n=30)
plt(n=20)
plt(n=10)
plt(n=5)
plt(n=1)
