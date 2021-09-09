
library(mgcv) # gams

# going from here https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam

df <- data.frame(year = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018), inc = c(386, 395, 401, 403, 402, 398, 390, 377, 358, 335, 310, 283, 257, 236, 218, 204, 193, 185, 181))


m <- gam(inc ~ s(year), data = df, bs="cr")
p  <- data.frame(year = seq(2000, 2018, length = 500))
pp <- predict(m, newdata = p, type = "response")

plot(df$year, df$inc)
lines(pp ~ year, data = p, col='red')

Xp <- predict(m, p, type="lpmatrix") ## map coefs to fitted curves
beta <- coef(m)
Vb   <- vcov(m) ## posterior mean and cov of coefs


n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
mrand <- mvrnorm(n, beta, Vb) 


opt <- rep(NA, n)
ilink <- family(m)$linkinv


pred   <- ilink(Xp %*% mrand[1, ])
opt[1] <- p$year[which.max(pred)]

for (i in seq_len(n)) { 
  pred   <- ilink(Xp %*% mrand[i, ])
  opt[i] <- p$year[which.max(pred)]
}

ci <- quantile(opt, c(.025,.975))
ci
