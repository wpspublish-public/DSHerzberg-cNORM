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
plotPercentileSeries(input, model, end = 10)

