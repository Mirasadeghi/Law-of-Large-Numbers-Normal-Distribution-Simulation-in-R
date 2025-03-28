# --------------------------------------
# Law of Large Numbers – Normal Distribution Simulation in R
# --------------------------------------

# Step 1: Generate N random samples from a standard normal distribution
N <- 1000000
counter <- 0
data <- rnorm(N)

# Step 2: Count how many values fall between -1 and 1
for(i in data){
  if(i < 1 & i > -1){
    counter <- counter + 1
  }
}
# Step 3: Calculate and print the proportion
proportion <- counter / N   # Proportion of values between -1 and 1
print(paste("Simulated Proportion:", proportion))


# ------------------------------
# Plot 1: Density Plot with shaded area between -1 and 1
# ------------------------------

# Calculate the density of the data
d <- density(data)
plot(d, main = "Standard Normal Distribution", xlab = "Value")  # Plot the density curve

# Shade the area between -1 and 1
polygon(d$x[d$x >= -1 & d$x <= 1],
        d$y[d$x >= -1 & d$x <= 1],
        col = rgb(1, 0, 0, 0.6), border = NA)

legend("topright", legend = "Between -1 and 1", fill = rgb(1, 0, 0, 0.6))

# ------------------------------
# Plot 2: Histogram with highlighted area between -1 and 1
# ------------------------------

hist(data, breaks = 30, col = "green", 
     main = "Histogram of Normal Distribution", xlab = "Value")  # Plot the histogram

# Add vertical lines to indicate the range -1 to 1
abline(v = -1, col = "red", lwd = 2, lty = 2)
abline(v = 1, col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Range: -1 to 1", lty = 2, col = "red")

# ------------------------------
# Plot 3: Bar Plot comparing theoretical vs simulated proportion
# ------------------------------

# Theoretical value for the range (-1, 1) according to the 68–95–99.7 rule
theoretical <- 0.682

# Create a bar plot to compare theoretical and simulated values
barplot(c(theoretical, proportion),
        names.arg = c("Theoretical", "Simulated"),
        col = c("lightblue", "lightgreen"),
        ylim = c(0, 1),
        main = "Comparison of Theoretical vs Simulated Probability",
        ylab = "Proportion")


