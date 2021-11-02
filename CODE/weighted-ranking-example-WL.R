# Test for weighting function in cNORM
# Procedure: Simulate population with two differing samples, e. g. 80% vs. 20% with
# different mean and sd
#
# First, some population parameters
# n.population specifies the size of the completely representative population
# n.normsample is the smaller norming sample, drawn randomly
# Later on, both groups will be drawn with a 50% share each and the percentiles
# will be estimated without weighting (red) or with weighting (blue)
# The other parameters can be set at will.
library(tidyverse)
library(cNORM)

n.population <- 100000
n.normsample <- 1000

m1 <- 12
sd1 <- 3
percentage1 <- .8

m2 <- 8
sd2 <- 3
percentage2 <- .2


# now, let's role the dice
# generate a large representative sample with 100 000 cases
n1 <- n.population*percentage1
sample1 <- data.frame(m=rep(m1, length.out=n1), percentile=seq(from=(.5/n1), to=(n1-.5) / n1, length.out=n1), weight=percentage1*100)
sample1$z <- qnorm(sample1$percentile, m1, sd1)

n2 <- n.population*percentage2
sample2 <- data.frame(m=rep(m2, length.out=n2), percentile=seq(from=(.5/n2), to=(n2-.5) / n2, length.out=n2), weight=percentage2*100)
sample2$z <- qnorm(sample2$percentile, m2, sd2)

totalSample <- rbind(sample1, sample2)
totalSample$percentileTotal <- rank(totalSample$z)/(n1+n2)
totalSample$zTotal <- qnorm(totalSample$percentileTotal)
totalSample <- totalSample[order(totalSample$zTotal),]
totalSample$raw <- totalSample$z

# draw unstratified sample, groups occur with equal share
n.b <- n.normsample
sample1.b <- data.frame(m=rep(m1, length.out=n.b/2), z=rnorm(n.b/2, m1, sd1), weight=n.b/2)
sample1.b$percentile <- pnorm((sample1.b$z - m1)/sd1)
sample2.b <- data.frame(m=rep(m2, length.out=n.b/2), z=rnorm(n.b/2, m2, sd2), weight=n.b/2)
sample2.b$percentile <- pnorm((sample2.b$z - m2)/sd2)
total2.b <- rbind(sample1.b, sample2.b)
total2.b <- total2.b[order(total2.b$z),]
totalSample2 <- rankByGroup(total2.b, raw="z", group=FALSE)

# draw unstratified sample, but use weighting
n.c <- n.normsample
sample1.c <- data.frame(m=rep(m1, length.out=n.c/2), z=rnorm(n.c/2, m1, sd1), weight=100 * percentage1)
sample1.c$percentile <- pnorm((sample1.c$z - m1)/sd1)
sample2.c <- data.frame(m=rep(m2, length.out=n.c/2), z=rnorm(n.c/2, m2, sd2), weight=100 * percentage2)
sample2.c$percentile <- pnorm((sample2.c$z - m2)/sd2)
total2.c <- rbind(sample1.c, sample2.c)
total2.c <- total2.c[order(total2.c$z),]
totalSample3 <- rankByGroup(total2.c, raw="z", group=FALSE, weights = "weight")

# plot
plot(totalSample$raw, totalSample$percentileTotal, type="l")
plot(totalSample$raw, totalSample$percentileTotal, type="l")
lines(totalSample2$z, totalSample2$percentile, col="red")
lines(totalSample3$z, totalSample3$percentile, col="blue")
legend(1, 1, legend=c("Population", "unweighted percentile", "weighting function"), col=c("black", "red", "blue"), lty=1:3, cex=0.8)

