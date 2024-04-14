##########################
#
library(latex2exp)
#
#########################
#
n <- 1000
#
T <- 5
#
ts <- seq(0,T,length = n+1)
#
delta <- T / n
#
Z <- rnorm(n, 0, sqrt(delta))
#
B <- cumsum(Z)
#
B_with_zero <- c(0, B)
#
plot(ts, 
     B_with_zero, 
     type = "l", 
     col = "blue",
     xlab = "Time", 
     ylab = "BM")
#
# Bigger margin, if needed
#
par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#
plot(ts, 
     B_with_zero, 
     type = "l", 
     col = "blue",
     xlab = TeX("Time, $t$"), 
     ylab = TeX("$B_t$"),
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
dev.copy2pdf(file = "BM.pdf",
             width = 12,
             height = 8)
#
# $\\mu$
#
###########################
#
# matplot
#
N_rep <- 500
#
B_mat <- matrix(NA,
                nrow = n + 1,
                ncol = N_rep)
#
for(j in 1:N_rep){
  #
  # Generate Brownian motion
  #
  Z <- rnorm(n, 0, sqrt(delta))
  #
  B <- cumsum(Z)
  #
  B_with_zero <- c(0, B)
  #
  # Store it
  #
  B_mat[,j] <- B_with_zero
  #
}
#
# -------------------
#
matplot(ts,
        B_mat,
        type = "l",
        lty = 1,
        xlab = TeX("Time, $t$"), 
        ylab = TeX("$B_t$"),
        cex.lab = 2.25,
        cex.axis = 2.25,
        cex.main = 3)
#
abline(h = 0,
       lwd = 2)
#
abline(v = T,
       lwd = 2)

dev.copy2pdf(file = "BMM.pdf",
             width = 12,
             height = 8)
#
# ----------------------
#
hist(B_mat[n + 1, ],
     nclass = 25,
     freq = FALSE,
     main = NULL,
     xlab = TeX("$B_t$"),
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
curve(dnorm(x, 0, sqrt(T)),
      add = TRUE)

dev.copy2pdf(file = "HSBM.pdf",
             width = 12,
             height = 8)
#
###############################
#
# Brownian motion with drift
#
n <- 1000
#
T <- 5
#
ts <- seq(0,T,length = n+1)
#
delta <- T / n
#
Z <- rnorm(n, 0, sqrt(delta))
#
B <- cumsum(Z)
#
B_with_zero <- c(0, B)
#
mu <- -0.5
#
sigma <- 0.05
#
B_with_drift <- mu * ts + sigma * B_with_zero
#
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(ts, 
     B_with_drift, 
     type = "l", 
     col = "blue",
     xlab = "Time", 
     ylab = TeX("$\\mu t + \\sigma B_t$"),
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)

dev.copy2pdf(file = "BMD.pdf",
             width = 12,
             height = 8)
#
# ------------------
#
# matplot
#
N_rep <- 500
#
B_drift_mat <- matrix(NA,
                      nrow = n + 1,
                      ncol = N_rep)
#
for(j in 1:N_rep){
  #
  # Generate Brownian motion
  #
  Z <- rnorm(n, 0, sqrt(delta))
  #
  B <- cumsum(Z)
  #
  B_with_zero <- c(0, B)
  #
  # Brownian motion with drift
  #
  B_with_drift <- mu * ts + sigma * B_with_zero
  #
  # Store it
  #
  B_drift_mat[,j] <- B_with_drift
  #
}
#
# -------------------
#
matplot(ts,
        B_drift_mat,
        type = "l",
        lty = 1,
        xlab = TeX("Time, $t$"), 
        ylab = TeX("$\\mu t + \\sigma B_t$"),
        cex.lab = 2.25,
        cex.axis = 2.25,
        cex.main = 3)
#
abline(0, mu,
       lwd = 3)
dev.copy2pdf(file = "BMDM.pdf",
             width = 12,
             height = 8)
#
###############################
#
# Geometric Brownian motion
#
X_0 <- 5
#
n <- 1000
#
T <- 5
#
ts <- seq(0,T,length = n+1)
#
delta <- T / n
#
Z <- rnorm(n, 0, sqrt(delta))
#
B <- cumsum(Z)
#
B_with_zero <- c(0, B)
#
mu <- -0.05
#
sigma <- 0.1
#
B_with_drift <- mu * ts + sigma * B_with_zero
#
geometric_Brownian_motion <- X_0 * exp(B_with_drift)
#
plot(ts, 
     geometric_Brownian_motion, 
     type = "l", 
     col = "blue",
     xlab = "Time", 
     ylab = TeX("$X_0$ exp($\\mu t + \\sigma B_t$)"),
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)

dev.copy2pdf(file = "GBM.pdf",
             width = 12,
             height = 8)

#
# ------------------
#
# matplot
#
N_rep <- 500
#
geometric_BM_mat <- matrix(NA,
                           nrow = n + 1,
                           ncol = N_rep)
#
for(j in 1:N_rep){
  #
  # Generate Brownian motion
  #
  Z <- rnorm(n, 0, sqrt(delta))
  #
  B <- cumsum(Z)
  #
  B_with_zero <- c(0, B)
  #
  # Brownian motion with drift
  #
  B_with_drift <- mu * ts + sigma * B_with_zero
  #
  # Geometric Brownian motion
  #
  geometric_Brownian_motion <- X_0 * exp(B_with_drift)
  #
  # Store it
  #
  geometric_BM_mat[,j] <- geometric_Brownian_motion
  #
}
#
# -------------------
#
matplot(ts,
        geometric_BM_mat,
        type = "l",
        lty = 1,
        xlab = TeX("Time, $t$"), 
        ylab = TeX("$X_0 \\exp(\\mu t + \\sigma B_t)$"),
        cex.lab = 2.25,
        cex.axis = 2.25,
        cex.main = 3)

dev.copy2pdf(file = "GBMM.pdf",
             width = 12,
             height = 8)
#
###############################
#
share_price <- rexp(500) # Should be downloaded
#
ts.plot(share_price)
#
log_share_price <- log(share_price)
#
log_share_price
#
diff_log_share_price <- diff(log_share_price)
#
diff_log_share_price
#
mu_hat <- mean(diff_log_share_price)
#
mu_hat
#
sigma_hat <- sd(diff_log_share_price)
#
sigma_hat
#################################################################################


library(quantmod)

jpm <- getSymbols(Symbols = "JPM",
                  src = "yahoo", # Data provider (can also be "google")
                  auto.assign = FALSE, # Allows data to be assigned directly to the jpm object and 
                  # only works for downloading data on a single stock
                  return.class = "xts")

JPM_Close <- jpm[,"JPM.Close"]
#
JPM_Close
#
sum(is.na(JPM_Close))
#
plot(JPM_Close)
#
# Extract the values themselves
#
JPM_Close_values <- drop(coredata(JPM_Close))
#
JPM_Close_values
#
plot(JPM_Close_values,
     type = "l")
#
# differences of logs
#
JPM_diff_log <- diff(log(JPM_Close_values))
#
mu_hat <- mean(JPM_diff_log)
#
mu_hat
#
sigma_hat <- sd(JPM_diff_log)
#
sigma_hat
#
#############################
#
#
# Geometric Brownian motion
#
X_0 <- JPM_Close_values[1]
#
n <- 10000
#
T <- length(JPM_Close_values)
#
ts <- seq(0,T,length = n+1)
#
delta <- T / n
#
Z <- rnorm(n, 0, sqrt(delta))
#
B <- cumsum(Z)
#
B_with_zero <- c(0, B)
#
B_with_drift <- mu_hat * ts + sigma_hat * B_with_zero
#
geometric_Brownian_motion <- X_0 * exp(B_with_drift)
#
par(mfrow = c(2, 1),
    mar = c(5.1, 4.5 + 1, 4.1, 2.1))
#
plot(JPM_Close_values,
     type = "l",
     main = "Observed Data",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
plot(ts, 
     geometric_Brownian_motion, 
     type = "l", 
     col = "blue",
     xlab = "Time", 
     ylab = TeX("$X_0 \\exp(\\mu t + \\sigma B_t)$"),
     main = "Simulated Data",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)

dev.copy2pdf(file = "GBMJPM.pdf",
             width = 12,
             height = 8)
#


######################
#
# Function to generate Geometric Brownian Motion
#
gBm_function <- function(X_0, mu, sigma, 
                         T = 1, n = 10000){
  #
  ts <- seq(0, T, length = n+1)
  #
  delta <- T / n
  #
  Z <- rnorm(n, 0, sqrt(delta))
  #
  B <- cumsum(Z)
  #
  B_with_zero <- c(0, B)
  #
  B_with_drift <- mu * ts + sigma * B_with_zero
  #
  geometric_Brownian_motion <- X_0 * exp(B_with_drift)
  #
  plot(ts, 
       geometric_Brownian_motion, 
       type = "l", 
       col = "blue",
       xlab = "Time", 
       ylab = "Geometric Brownian motion",
       main = "Simulated Data")
  #
}
#
gBm_function(X_0 = JPM_Close_values[1], 
             mu = mu_hat, 
             sigma = sigma_hat, 
             T = length(JPM_Close_values), 
             n = 10000)
#
par(mfrow = c(2, 1),
    mar = c(5.1, 4.1 + 1, 4.1, 2.1))
#
plot(JPM_Close_values,
     type = "l",
     main = "Observed Data")
#
gBm_function(X_0 = JPM_Close_values[1], 
             mu = mu_hat, 
             sigma = sigma_hat, 
             T = length(JPM_Close_values), 
             n = 10000)
#
######################

gbm.f = function(n, s0, mu_hat, sigma_hat){
  n <- jpm
  #
  T <- 5
  #
  ts <- seq(0,T,length = n+1)
  #
  delta <- T / n
  #
  Z <- rnorm(n, 0, sqrt(delta))
  #
  B <- cumsum(Z)
  #
  B_with_zero <- c(0, B)
  #
  st <- s0*exp((mu_hat-sigma_hat^2/2)*ts+sigma_hat*B)
}

rt <- gbm.f(n,s0 = 10, mu_hat, sigman_hat)
plot(seq(0,1,length = 1000+1),rt,type ="l",ylab ="Returns",xlab = "Time")
##############################################################################

install.packages("ggplot2")

# Load the required libraries
library(quantmod)
library(ggplot2)

# Define the stock symbol and date range
stock_symbol <- "JPM"
start_date <- "2022-01-01"
end_date <- "2022-12-31"

# Download historical stock data using quantmod
getSymbols(stock_symbol, from = start_date, to = end_date)
jpm_data <- Ad(get(stock_symbol))

# Calculate daily returns
daily_returns <- diff(log(jpm_data))

# Calculate the parameters for GBM
mu <- mean(daily_returns)
sigma <- sd(daily_returns)
S0 <- tail(jpm_data, 1)

# Simulate GBM
set.seed(123)
n_simulations <- 100
n_days <- length(daily_returns)
simulated_prices <- matrix(0, n_simulations, n_days)

for (i in 1:n_simulations) {
  simulated_prices[i, ] <- S0 * exp(cumsum(rnorm(n_days, mean = mu, sd = sigma)))
}

# Create a data frame for plotting
simulated_data <- data.frame(
  Date = index(jpm_data)[-1],
  Simulated_Price = as.vector(simulated_prices)
)

# Plot the GBM
ggplot(data = simulated_data, aes(x = Date, y = Simulated_Price)) +
  geom_line() +
  ggtitle("Geometric Brownian Motion Simulation for JPM") +
  xlab("Date") +
  ylab("Price")

#################################
#

#

g(5, 6, 10, 0.02, 0.01)

##################################
#
install.packages("RQuantLib")
#
library(RQuantLib)

EuropeanOption(type = "call",
               underlying = 50, # X_0
               strike = 45, # K
               dividendYield = 0.0,
               riskFreeRate = 0.06, # r
               maturity = 0.25, # T
               volatility = 0.45) # sigma
#
###############################
#
# Now with dividendYield = 0.1
#
EuropeanOption(type = "call",
               underlying = 50, # X_0
               strike = 45, # K
               dividendYield = 0.1,
               riskFreeRate = 0.06, # r
               maturity = 0.25, # T
               volatility = 0.45) # sigma
#
# We discount the share by the dividend
#
X_0_discounted_by_dividend <- 50 * exp(-0.1 * 0.25)
#
X_0_discounted_by_dividend
#
# So this is what we need to invest today in the share
# to yield X_0 after the payment of the dividend
#
EuropeanOption(type = "call",
               underlying = X_0_discounted_by_dividend, # X_0 discounted by dividend
               strike = 45, # K
               dividendYield = 0.0,
               riskFreeRate = 0.06, # r
               maturity = 0.25, # T
               volatility = 0.45) # sigma
#
EuropeanOption(type = "call",
               underlying = 50, # X_0
               strike = 45, # K
               dividendYield = 0.1,
               riskFreeRate = 0.06, # r
               maturity = 0.25, # T
               volatility = 0.45) # sigma
#
# Same option price!
#
#################################################
#
# g
#
g <- function(X_0, K, T, sigma, r){
  #
  (log(X_0 / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T)) 
}
#
# --------------------------------------------
#
# h
#
h <- function(X_0, K, T, sigma, r){
  #
  g(X_0, K, T, sigma, r) - sigma * sqrt(T)
}
#
# --------------------------------------------
#
# rational_price
#
rational_price <- function(X_0, K, T, sigma, r){
  #
  X_0 * pnorm(g(X_0, K, T, sigma, r)) - K * exp(-r * T) * pnorm(h(X_0, K, T, sigma, r))
  #
}
#
##########################################
#
rational_price(50, 45, 0.25, 0.45, 0.06)
#
N <- 1000
#
X_0 <- seq(from = 0,
           to = 100,
           length = N)
#
RP <- rational_price(X_0, 45, 0.25, 0.45, 0.06)
#
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(X_0, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
abline(h = 0,
       v = 45)

abline(0, 1,
       lty = 2)

P <- X_0 - exp(-0.06 *0.25) * 45

lines(X_0, P,
      col = "red")

dev.copy2pdf(file = "ECO.pdf",
             width = 12,
             height = 8)
#
# As X_0 increases, the share starts at a higher price.
# Therefore, it's likely to finish at a higher price.
# Therefore, the option is more likely to be exercised.
# Therefore, the rational price increases.
# We see that it seems to increase linearly, but
# the rational price is always less than X_0.  It doesn't make
# sense to pay more later than you can pay now.
#
# -----------------------
#
K <- seq(from = 0,
         to = 100,
         length = N)
#
RP <- rational_price(50, K, 0.25, 0.45, 0.06)
#
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(K, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
abline(h = 0,
       v = 50,
       lty = 2)
#
# As K gets bigger, the option is less likely to be exercised,
# so is less valuable.
# It's essentially valueless when K > X_0; why pay more later?
# Of course, if there's now volatility and no interest, then
# it is valueless when K > X_0
#
P <- 50 - exp(-0.06 *0.25) * K

lines(K, P,
      col = "red")

dev.copy2pdf(file = "ECO2.pdf",
             width = 12,
             height = 8)
#
#
# -----------------------
#
T <- seq(from = 0,
         to = 200,
         length = N)
#
RP <- rational_price(50, 45, T, 0.45, 0.06)
#
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(T, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
abline(h = c(0, 50),
       v = 50,
       lty = 2)
#
# The longer you run the option, the more likely the final share
# value is to be greater than K.  Therefore, the option value increases.
# However, it can't more worth more than if you were to buy the share
# at the beginning for X_0
#
abline(h = 50 - 45,
       col = "red")

dev.copy2pdf(file = "ECO3.pdf",
             width = 12,
             height = 8)
#
# -----------------------
#
sigma <- seq(from = 0,
             to = 40,
             length = N)
#
RP <- rational_price(50, 45, 0.25, sigma, 0.06)
#
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(sigma, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
abline(h = c(0, 50),
       lty = 2)
#
# The greater the volatility in share price, the more likely the final share
# value is to be greater than K.  Therefore, the option value increases.
# However, it can't more worth more than if you were to buy the share
# at the beginning for X_0
#
abline(h = 50 - exp(-0.06 *0.25) * 45,
       col = "red")
dev.copy2pdf(file = "ECO4.pdf",
             width = 12,
             height = 8)
#
# -----------------------
#
r <- seq(from = 0,
         to = 30,
         length = N)
#
RP <- rational_price(50, 45, 0.25, 0.45, r)
#
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(r, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
abline(h = c(0, 50),
       lty = 2)
#
# The greater the interest rate, the lower the present value of K.
# You need K exp(-r T) now to have K at time T.
# Therefore, your future expenditure is effectively less, so this
# has to be balanced by increased expenditure now.
# Therefore, the option value increases.
# However, it can't more worth more than if you were to buy the share
# at the beginning for X_0
#
Phi_delta_1 <- pnorm((log(50 / 45) + 0.5 * 0.45^2 * 0.25) / (0.45 * sqrt(0.25)))
Phi_delta_2 <- pnorm((log(50 / 45) - 0.5 * 0.45^2 * 0.25) / (0.45 * sqrt(0.25)))
#
abline(h = 50 * Phi_delta_1 - 45 * Phi_delta_2,
       col = "red")

dev.copy2pdf(file = "ECO5.pdf",
             width = 12,
             height = 8)
###################################
#
# Can we vectorize the package function
#
library(RQuantLib)
#
RP_seq <- rep(NA, N)
#
for(i in 1:N){
  #
  RP_seq[i] <- EuropeanOption(type = "call",
                              underlying = 50, # X_0
                              strike = 45, # K
                              dividendYield = 0.0,
                              riskFreeRate = r[i], # r
                              maturity = 0.25, # T
                              volatility = 0.45)$value # sigma
}
#
plot(r, RP_seq,
     type = "l")
#
# Test
#
lines(r, RP,
      col = "red")
#
###########################################
###########################################
#
# Put
#
# rational_price
#
rational_price_put <- function(X_0, K, T, sigma, r){
  #
  K * exp(-r * T) * pnorm(-h(X_0, K, T, sigma, r)) - X_0 * pnorm(-g(X_0, K, T, sigma, r))
  #
}
#
##########################################
#
rational_price_put(50, 45, 0.25, 0.45, 0.06)
#
N <- 1000
#
X_0 <- seq(from = 0,
           to = 100,
           length = N)
#
RP <- rational_price_put(X_0, 45, 0.25, 0.45, 0.06)
#
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(X_0, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
abline(h = 0)
#
P <- exp(-0.06 *0.25) * 45 - X_0

lines(X_0, P,
      col = "red")

dev.copy2pdf(file = "EPO.pdf",
             width = 12,
             height = 8)
#
# As X_0 increases, the share starts at a higher price.
# Therefore, it's likely to finish at a higher price.
# Therefore, the option is more likely to be exercised.
# Therefore, the rational price increases.
# We see that it seems to increase linearly, but
# the rational price is always less than X_0.  It doesn't make
# sense to pay more later than you can pay now.
#
# -----------------------
#
K <- seq(from = 0,
         to = 100,
         length = N)
#
RP <- rational_price_put(50, K, 0.25, 0.45, 0.06)
#
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(K, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
abline(h = 0,
       v = 50,
       lty = 2)
#
# Modify:
#
# As K gets bigger, the option is less likely to be exercised,
# so is less valuable.
# It's essentially valueless when K > X_0; why pay more later?
# Of course, if there's now volatility and no interest, then
# it is valueless when K > X_0
#
P <- exp(-0.06 *0.25) * K-50

lines(K, P,
      col = "red")

dev.copy2pdf(file = "EPO2.pdf",
             width = 12,
             height = 8)
#
#
# -----------------------
#
T <- seq(from = 0,
         to = 200,
         length = N)
#
RP <- rational_price_put(50, 45, T, 0.45, 0.06)
#
0.06 - 0.5 *0.45^2
#
par(mfrow = c(2, 1))
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(T, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
# Modify:
#
abline(h = c(0, 50),
       v = 50,
       lty = 2)


# The longer you run the option, the more likely the final share
# value is to be greater than K.  Therefore, the option value increases.
# However, it can't more worth more than if you were to buy the share
# at the beginning for X_0
#

#
# Derivative
#
dg_dT <- function(X_0, K, T, sigma, r){
  #
  - log(X_0 / K) / (2 * sigma * T^(3/2)) +  (r + sigma^2 / 2) / (2 *sigma * sqrt(T))
  #
}
#
# --------------------------------------------
#
# h
#
dh_dT <- function(X_0, K, T, sigma, r){
  #
  - log(X_0 / K) / (2 * sigma * T^(3/2)) +  (r - sigma^2 / 2) / (2 *sigma * sqrt(T))
  #
}
#
# --------------------------------------------
#
r <- 0.06
K <- 45
X_0 <- 50
sigma <- 0.45
#
dP_dT <- -exp(-r * T) * K * dnorm(-h(X_0, K, T, sigma, r)) * dh_dT(X_0, K, T, sigma, r) - r * exp(-r * T) * K * pnorm(-h(X_0, K, T, sigma, r)) + X_0 * dnorm(-g(X_0, K, T, sigma, r)) * dg_dT(X_0, K, T, sigma, r) 
#
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(T, dP_dT,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
abline(h = 0)

dev.copy2pdf(file = "EPO3.pdf",
             width = 12,
             height = 8)
#
# -----------------------
#
sigma <- seq(from = 0,
             to = 40,
             length = N)
#
RP <- rational_price_put(50, 45, 0.25, sigma, 0.06)
#
par(mfrow = c(1,1))
par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(sigma, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
# Modify
#

#
# The greater the volatility in share price, the more likely the final share
# value is to be greater than K.  Therefore, the option value increases.
# However, it can't more worth more than if you were to buy the share
# at the beginning for X_0
#
abline(h =  exp(-0.06 *0.25) * 45,
       col = "red")

dev.copy2pdf(file = "EPO4.pdf",
             width = 12,
             height = 8)
#
# -----------------------
#
r <- seq(from = 0,
         to = 30,
         length = N)
#
RP <- rational_price_put(50, 45, 0.25, 0.45, r)
#

par(mar = c(5.1, 5, 4.1, 2.1))
#
plot(r, RP,
     type = "l",
     cex.lab = 2.25,
     cex.axis = 2.25,
     cex.main = 3)
#
# Modify:
#
abline(h = c(0, 50),
       lty = 2)


#
# The greater the interest rate, the lower the present value of K.
# You need K exp(-r T) now to have K at time T.
# Therefore, your future expenditure is effectively less, so this
# has to be balanced by increased expenditure now.
# Therefore, the option value increases.
# However, it can't more worth more than if you were to buy the share
# at the beginning for X_0
#
Phi_delta_1 <- pnorm((log(50 / 45) + 0.5 * 0.45^2 * 0.25) / (0.45 * sqrt(0.25)))
Phi_delta_2 <- pnorm((log(50 / 45) - 0.5 * 0.45^2 * 0.25) / (0.45 * sqrt(0.25)))
#
abline(h = 50 * Phi_delta_1 - 45 * Phi_delta_2,
       col = "red")

dev.copy2pdf(file = "EPO5.pdf",
             width = 12,
             height = 8)
###################################
#
# Can we vectorize the package function
#
library(RQuantLib)
#
RP_seq <- rep(NA, N)
#
for(i in 1:N){
  #
  RP_seq[i] <- EuropeanOption(type = "call",
                              underlying = 50, # X_0
                              strike = 45, # K
                              dividendYield = 0.0,
                              riskFreeRate = r[i], # r
                              maturity = 0.25, # T
                              volatility = 0.45)$value # sigma
}
#
plot(r, RP_seq,
     type = "l")
#
# Test
#
lines(r, RP,
      col = "red")

