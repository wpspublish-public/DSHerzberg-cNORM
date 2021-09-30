library(cNORM)
library(readr)

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-TOD-R/master/INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
input_file_name <- "input-TOD-cNORM-reprex1.csv"

input <- suppressMessages(read_csv(url(
  paste0(urlRemote_path, github_path, input_file_name)
)))

# rankByGroup() returns a warning that one of the age groups has less than 30
# cases.
input <- rankByGroup(input, scale = "IQ")

# R2 between age group and raw score is .3, lower than I would have thought.
input <- computePowers(input)

# with k = 3, model() returns a plot showing intersecting percentile curves in
# the upper age ranges.
model <- bestModel(input, terms = 3)

# checkConsistency() warns of violations of monotonicity at ages 7 and 8, which
# seems consistent with the plot of percentile curves.
checkConsistency(model)

plotSubset(model, type= 0)

# When we plot the series of percentiles across different values of k, the plot
# with nine predictors "looks" best, in terms of solving the problem of
# intersecting curves.
plotPercentileSeries(input, model)

# Alex Lenhard's added code below. Key points:

# Use the all-in-one cnorm(), that combines several important functions from my
# code above

# "group" var is used by cnorm() to determine percentile ranks. Value for this
# var should be the mean value of chronological age for cases with each group
# (age strata), or some close approximation of this value. Other values (e.g.,
# lower-bound of age group) introduce error.

# The two key diagnostics are plotPercentileSeries() and checkConsitency(). Both
# target the same problem: violations of monotonicty, or intersecting percentile
# curves. With plotPercntileSeries(), you can use "end" argument to set upper
# limit of predictors, and "percentiles" to provide a vector of %ile scores to
# model.

# Alex recoded the input age groupings into "group2", collapsing existing age
# strata to get larger groups. She recommends n >= 100 for age strata, so
# collapse groups to get to this number.

library(tidyverse)
library(here)

TOD <- suppressMessages(read_csv(here(
  "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/TOD-AL-recode-group.csv"
))) %>% 
  # drop age outlier
  filter(ID != 210039) %>% left_join(
    suppressMessages(read_csv(here(
      "INPUT-FILES/NORMS/TODE_8.27.21_fornorms/TOD-AL-age-contin.csv"
    ))),
    by = "ID"
  ) %>% 
  drop_na()
  

#modelTOD
cnorm.TOD <- cnorm(raw=TOD$raw, group=TOD$group2, k=5, t=2, terms=5, scale = "IQ")
plot(cnorm.TOD, "series", end = 10)
plot(cnorm.TOD, "subset")
plot(cnorm.TOD, "percentiles")
plotDerivative(cnorm.TOD)
checkConsistency(cnorm.TOD)
plotPercentiles(cnorm.TOD, percentiles=c(0.001, .5, .999))
normTable(cnorm.TOD, A = 5.5)
rawTable(cnorm.TOD, A=9.25)

#alternative modelling with exact age = date_eval minus DOB, expressed as a decimal.
cnorm.TOD <- cnorm(raw=TOD$raw, group=TOD$group2, age=TOD$age, k=5, t=2, terms=5, scale = "IQ")
