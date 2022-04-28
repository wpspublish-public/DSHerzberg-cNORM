library(cNORM)
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))

# Use of the data set 'ppvt' and calculation of the manifest percentiles
# and normal scores via 'sliding window'. The size of the window is 1.5
# years. The function adds information on sample size and distribution
# parameters to each case.

data.ppvt <- rankBySlidingWindow(ppvt, age="age", raw="raw", width=1.5)


# Calculation of powers and interactions of location and
# continuous age variable

data.ppvt <- computePowers(data.ppvt, age="age")


# Determination of a model with an arbitrarily chosen number of seven terms
# The Output shows a rather high R2 of 0.9942.

model.ppvt <- bestModel(data.ppvt, terms=7)


# Illustration of R2 and information criterion Mallow's Cp

plotSubset(model.ppvt, type=1)


# Checking the limits of model validity via first order derivative
# to location outside the age range of the test (= horizontal interpolation)
# The gradient should not fall below zero.

plotDerivative(model.ppvt, minAge=2, maxAge=20, minNorm=20, maxNorm=80)

# Numerical check of model validity and inconsistent percentile curves
checkConsistency(model.ppvt)

# Result: Gradient above zero throughout the actual age range.

# All further steps are performed in the same way as in example 1

plotPercentiles(data.ppvt, model.ppvt)
normTable(10.2, model.ppvt, step=1, minNorm=25, maxNorm=75) # Age 10.2



