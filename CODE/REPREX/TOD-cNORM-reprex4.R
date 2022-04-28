library(cNORM)
library(tidyverse)

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-TOD-R/master/INPUT-FILES/NORMS/TODE_8.27.21_fornorms/"
data_file_name <- "TOD-AL-recode-group.csv"
age_file_name <- "TOD-AL-age-contin-getGroup.csv"

TOD <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, data_file_name)
))) %>%
  left_join(suppressMessages(read_csv(url(
    str_c(urlRemote_path, github_path, age_file_name),
  ))),
  by = "ID") %>%
  drop_na()

#modelTOD
cnorm.TOD <- cnorm(raw=TOD$raw, group=TOD$getGroup, k=5, t=2, terms=5, scale = "IQ")
plot(cnorm.TOD, "series", end = 10)
plot(cnorm.TOD, "subset")
plot(cnorm.TOD, "percentiles")
plotDerivative(cnorm.TOD)
checkConsistency(cnorm.TOD)
plotPercentiles(cnorm.TOD, percentiles=c(0.001, .5, .999))
normTable(cnorm.TOD, A = 5.5)
rawTable(cnorm.TOD, A=9.25)

#alternative modelling with exact age = date_eval minus DOB, expressed as a decimal.
cnorm.TOD <- cnorm(raw=TOD$raw, group=TOD$getGroup, age=TOD$age, k=5, t=2, terms=5, scale = "IQ")
