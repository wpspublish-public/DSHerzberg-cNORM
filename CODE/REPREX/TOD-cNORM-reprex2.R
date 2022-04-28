library(cNORM)
library(readr)

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-TOD-R/master/INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
input_file_name <- "input-TOD-cNORM-reprex2.csv"

input <- suppressMessages(read_csv(url(
  paste0(urlRemote_path, github_path, input_file_name)
)))

# The collapse age groups have adequate sample sizes
input <- rankByGroup(input, scale = "IQ")

# R2 between age group and raw score is .3, lower than I would have thought.
input <- computePowers(input)

# with k = 5, model() returns a plot with non-intersection percentile curves. It
# also returns a warning that 10 linear dependencies were found.
model <- bestModel(input, terms = 5)

# checkConsistency() returns no violations of model consistency.
checkConsistency(model)

plotSubset(model, type= 0)

plotPercentileSeries(input, model)

plotDerivative(model)

