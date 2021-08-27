library(cNORM)
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))

data_ANT <-
  suppressMessages(read_csv(here("INPUT-FILES/ANTraw_STAND.csv"))) %>%
  select(ID, agestrat, ANT_total) %>%
  mutate(across(
    agestrat,
    ~ case_when(
      .x == 5.0 ~ 60,
      .x == 5.3 ~ 63,
      .x == 5.6 ~ 66,
      .x == 5.9 ~ 69,
      .x == 6.0 ~ 72,
      .x == 6.3 ~ 75,
      .x == 6.6 ~ 78,
      .x == 6.9 ~ 81,
      .x == 7.0 ~ 84,
      .x == 7.3 ~ 87,
      .x == 7.6 ~ 90,
      .x == 7.9 ~ 93,
      .x == 8.0 ~ 96,
      .x == 8.6 ~ 102,
      .x == 9.0 ~ 108,
      .x == 9.6 ~ 114,
      .x == 10.0 ~ 120,
      .x == 10.6 ~ 126,
      .x == 11.0 ~ 132,
      .x == 11.6 ~ 138,
      .x == 12.0 ~ 144,
      .x == 12.6 ~ 150,
      .x == 13.0 ~ 156,
      .x == 14.0 ~ 168,
      .x == 15.0 ~ 180,
      .x == 1618.0 ~ 192,
      .x == 1921.0 ~ 228,
      TRUE ~ NA_real_
    )
  )) %>%
  rename(personID = ID,
         group = agestrat,
         raw = ANT_total)

# Calculation of the manifest percentiles and subsequent
# normal rank transformation to determine the location

data_ANT <- rankByGroup(data_ANT, scale = "IQ")


# Calculation of powers and interactions of
# location and grouping variable

data_ANT <- computePowers(data_ANT)


# Determining the best model with specified R2

model_ANT <- bestModel(data_ANT, terms = 3)


# Numerical check of the bijectivity between raw score and normal score

checkConsistency(model_ANT)


# Illustration of R2 by number of predictors

plotSubset(model_ANT, type= 0)

# Checking the limits of model validity via first order derivative
# to location outside the age range of the test (= horizontal interpolation)
# The gradient should not fall below zero.
# plotDerivative(model_ANT, minAge=60, maxAge=228, minNorm=50, maxNorm=150)



# Illustration of percentile curves of the identified best model

plotPercentiles(data_ANT, model_ANT)


# Alternatively, a whole series of charts can be generated, whereby
# the number of predictors is incremented by one at a time.
# If no further information is given, the chart is set to actually
# occurring raw scores and age groups of the original data set.

plotPercentileSeries(data_ANT, model_ANT)

# Transforms a specified series of normal scores into raw scores for
# third graders, ninth month of school year (explanatory variable = 3.75)

# normTable(3.75, model_ANT, step=1, minNorm=25, maxNorm=75)


# Alternative: Output of standard scores for a series of raw scores

norms_list <- rawTable(
  unique(data_ANT$group), 
  model_ANT, 
  step = 1, 
  minNorm = 40, 
  maxNorm = 160, 
  minRaw = 0, 
  maxRaw = 61
  )



