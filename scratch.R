
source("../delirium_package/R/random_weibull.R")

set.seed(9876543210)

# The idea is to simulate a lot of potential calendar-time
# values, but only retain them if they are greater than
# the previous value.
# We can calculate this sequence of progressive
# maxima with unique(cummax(.))
y <- unique(cummax(
	rweibRH(5000000, shape = .3, scale = 5, rh = exp(0))
))
length(y)
min(y);max(y)
# even after drawing 5 million values, we only get 16 unique events.

# We only stop drawing if the maximum recurrent event time
# is larger than the observed terminal event time.
t1 <- rweibRH(1, shape = 1, scale = 25, rh = exp(0))
t2 <- rweibRH(1, shape = 1, scale = 25, rh = exp(0))
max(y) > min(t1, t2)
y <- y[y<min(t1, t2)]
length(y)
# we are left with only 3 events

# We can compare this to a sample of 16 iid draws from the initial
# distribution to verify that they follow the same distribution

y2 <- rweibRH(length(y), shape = .5, scale = 5, rh = exp(0))

plot((sort(y))~sort(y2))
plot(pweibull(sort(y), shape = .5, scale = 5)~(1:length(y))/length(y))



set.seed(9876543210)
# What if instead, we drew until we got an event past the end point?
y <- rweibRH(1000, shape = .3, scale = 5, rh = exp(0))
# even after drawing 5 million values, we only get 16 unique events.

# We only stop drawing if the maximum recurrent event time
# is larger than the observed terminal event time.
t1 <- rweibRH(1, shape = 1, scale = 25, rh = exp(0))
t2 <- rweibRH(1, shape = 1, scale = 25, rh = exp(0))
#t <- min(t1, t2)
t <- 1000
y <- sort(y[0:min(length(y), (min(which(y>t))-1))])

# We can compare this to a sample of 16 iid draws from the initial
# distribution to verify that they follow the same distribution

y2 <- rweibRH(1000, shape = .3, scale = 5, rh = exp(0))
y2 <- y2[y2<t]
y2 <- y2[1:length(y)]

plot((sort(y))~sort(y2))
plot(pweibull(sort(y), shape = .3, scale = 5)~seq(0,1,length =  length(y)))
abline(a = c(0,1), col = "red")

