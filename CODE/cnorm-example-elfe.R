library(cNORM)
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))

# The 'elfe' data set is already included

data.elfe <- elfe


# Calculation of the manifest percentiles and subsequent
# normal rank transformation to determine the location

data.elfe <- rankByGroup(data.elfe)


# Calculation of powers and interactions of
# location and grouping variable

data.elfe <- computePowers(data.elfe)


# Determining the best model with specified R2

model.elfe <- bestModel(data.elfe, R2=.99)


# Numerical check of the bijectivity between raw score and normal score

checkConsistency(model.elfe)


# Illustration of R2 by number of predictors

plotSubset(model.elfe, type=0)

# Illustration of percentile curves of the identified best model

plotPercentiles(data.elfe, model.elfe)


# Alternatively, a whole series of charts can be generated, whereby
# the number of predictors is incremented by one at a time.
# If no further information is given, the chart is set to actually
# occurring raw scores and age groups of the original data set.

plotPercentileSeries(data.elfe, model.elfe)

# Transforms a specified series of normal scores into raw scores for
# third graders, ninth month of school year (explanatory variable = 3.75)

normTable(3.75, model.elfe, step=1, minNorm=25, maxNorm=75)


# Alternative: Output of T-scores for a series of raw scores

rawTable(3.75, model.elfe, step=1, minNorm=25, maxNorm=75, minRaw=0, maxRaw=28)



