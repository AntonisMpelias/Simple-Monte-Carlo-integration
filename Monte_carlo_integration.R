
#Creating any mathematical function
f <- function (x) {
  3*x^2 + 2*x + 1
}

#Deciding what the ends of the integral we want to calculate are (from a to b)
a <- 0
b <- 2

#Taking 200 random f(x)s to find the average f(x) from 0 to 2.
samples <- runif(100, a, b)

#Calculating the integral by assuming it is a rectangle from 0 to 2 with its height
#being the mean we calculated from before. 
integral_estimate <- (b-a) * mean(f(samples))

#Finally, we create a vector where we see how our estimate converged overtime to
#reach the final estimate above
estimates <- cumsum(f(samples)) / seq_along(samples) * (b-a)

#We plot the function's graph as well as the convergence 
layout(matrix(c(2,1), nrow = 2, byrow = TRUE))  
par(mar = c(4, 4, 3, 1))

x_vals <- seq(a, b, length.out=1000)
plot(x_vals, f(x_vals), type='l', main= "Function f(x) = 3x² + 2x + 1",  
     xlab = "x values", ylab = "f(x)",  
     lwd = 3, col = "red")  
grid()
plot(estimates, type = 'l', col = 'blue',
     main = "Monte Carlo Convergence",
     xlab = "Number of samples", ylab ="Integral estimate ",
     lwd = 2)

#The actual value of the integral to see how the convergence approaches it
exact_integral <- integrate(f, lower = 0, upper = 2)$value
abline(h = exact_integral, col = "red", lty = 2, lwd = 1.5)
     
