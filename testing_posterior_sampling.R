
library(mgcv) # gams
library("MASS") ## for mvrnorm

# going from here https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam

df <- data.frame(year = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018), inc = c(386, 395, 401, 403, 402, 398, 390, 377, 358, 335, 310, 283, 257, 236, 218, 204, 193, 185, 181))

# fit the model
m <- gam(inc ~ s(year), data = df, bs="cr")
# make a data frame witha  row for each year
p  <- data.frame(year = seq(2000, 2035, length = 36))
# predict for everything in p
pp <- predict(m, newdata = p, type = "response")

# plot the actual data
plot(df$year, df$inc, xlim = c(2000, 2035), ylim = c(0, 425))
# plot the predicted data
lines(pp ~ year, data = p, col='red')

# I'm not really sure what these three lines do - extract model params?
Xp <- predict(m, p, type="lpmatrix") ## map coefs to fitted curves
beta <- coef(m)
Vb   <- vcov(m) ## posterior mean and cov of coefs

# set the number of bootstraps
n <- 10000
set.seed(10)
# sampling from the model posterior?
mrand <- mvrnorm(n, beta, Vb) 

# make a copy of p for storing the outputs
output <- p

# for each bootstrap
# predict using the samples from the model posterior
# and save the output
for (i in seq_len(n)) {
  pred   <- ilink(Xp %*% mrand[i, ])
  #opt[i] <- p$year[which.max(pred)]
  output <- cbind(output, pred)
}

# get rid of the year column, and convert to matrix
q <- as.matrix(output[, !names(output) %in% c("year")])
# for each row (which is a year), take the 95% CI
s <- rowQuantiles(q, probs=c(.05, .95))
# combine the quantiles with the year
s <- cbind(p, s)


