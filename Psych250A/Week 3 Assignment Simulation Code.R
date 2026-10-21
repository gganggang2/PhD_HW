# ---------------------------------------------------------------------
# SAMPLING ERROR SIMULATION
# ---------------------------------------------------------------------

# SIMULATION INPUTS ----

pop_mean <- 27        # population mean
pop_sd <- 10.10       # population standard deviation
sample_N <- 939       # sample size

# ---------------------------------------------------------------------
# SIMULATION CODE
# ---------------------------------------------------------------------

# VERIFICATION AUDIT ----

# makes the results identical every time the script is run
set.seed(250)

# draw a single sample of scores from the population
mysample <- rnorm(n = sample_N, mean = pop_mean, sd = pop_sd)

# mean, standard deviation, skewness, and excess kurtosis of the sample
audit_mean <- mean(mysample)
audit_sd <- sd(mysample)
audit_skew <- mean((mysample - audit_mean)^3) / audit_sd^3
audit_kurt <- mean((mysample - audit_mean)^4) / audit_sd^4 - 3

# tallest bar in the audit histogram, used to leave room for the legend
audit_barheight <- max(hist(mysample, breaks = 25, plot = FALSE)$counts)

# graph the sample
hist(mysample, breaks = 25,
     ylim = c(0, audit_barheight * 1.35),
     main = paste('Verification Audit: One Sample of Size =', sample_N),
     xlab = 'Scores')

legend('topright', bty = 'n',
       legend = c(paste('Mean =', round(audit_mean, 3)),
                  paste('SD =', round(audit_sd, 3)),
                  paste('Skewness =', round(audit_skew, 3)),
                  paste('Excess kurtosis =', round(audit_kurt, 3))))

# DRAW 100,000 RANDOM SAMPLES ----

# create an empty vector to hold the 100,000 sample means
samplemeans <- rep(0, 100000)

# draw a sample, compute its mean, and store the mean; repeat 100,000 times
for(s in 1:100000){
  sampledata <- rnorm(n = sample_N, mean = pop_mean, sd = pop_sd)
  samplemeans[s] <- mean(sampledata)
}

# EXAMINE THE SAMPLING DISTRIBUTION ----

# horizontal axis half-width
plot_halfwidth <- 5

# histogram bin width, also held constant so the bars are comparable
bin_width <- 0.05

# vertical axis maximum, held constant so bar heights are comparable
plot_maxcount <- 8000

# graph the 100,000 sample means with fixed bin width and axis ranges so graphs are comparable
hist(samplemeans,
     breaks = seq(pop_mean - 50, pop_mean + 50, by = bin_width),
     xlim = c(pop_mean - plot_halfwidth, pop_mean + plot_halfwidth),
     ylim = c(0, plot_maxcount),
     main = 'Distribution of 100,000 Sample Means',
     xlab = 'Sample Means')

legend('topright', bty = 'n',
       legend = c(paste('N =', sample_N),
                  paste('Mean of the sample means =', round(mean(samplemeans), 3)),
                  paste('SD of the sample means =', round(sd(samplemeans), 3)),
                  paste('Standard error formula =', round(pop_sd / sqrt(sample_N), 3))))
