suppressMessages(library(cNORM))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(here))
library(writexl)
suppressMessages(library(lubridate))

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish-public/DSHerzberg-cNORM/master/INPUT-FILES/"
data_file_name <- "cNORM-demo-TOD-input-data.csv"

input_original <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, data_file_name)
)))

scores <- c("iws_sum", "bln_sum", "seg_sum")
score_to_norm_stem <- "iws_sum"
score_to_norm_file_name <- str_c(score_to_norm_stem, "-norms-input.csv")
score_to_norm_max_raw <- data.frame(test = score_to_norm_stem) %>%
  mutate(max_raw = case_when(
    str_detect(test, "iws_sum") ~ 44,
    str_detect(test, "bln_sum") ~ 29,
    str_detect(test, "seg_sum") ~ 29
  )) %>%
  pull(max_raw)

age_contin <- input_original %>% 
  mutate(
    across(
      c(DOB, admin_date),
      ~
        mdy(.x)
    ),
    age = (DOB %--% admin_date) / years (1)
  ) %>%
  bind_cols(getGroups(.$age)) %>% 
  rename(group = ...14) %>% 
  select(ID, age, group)

map(
  scores,
  ~
    input_original %>%
    select(ID,!!sym(.x)) %>%
    drop_na(!!sym(.x)) %>%
    left_join(age_contin, by = "ID") %>%
    rename(raw = !!sym(.x)) %>%
    select(ID, age, group, raw)
) %>%
  set_names(scores) %>%
  map2(scores,
       ~
         write_csv(.x,
                     here(
                       str_c("OUTPUT-FILES/", .y, "-norms-input.csv")
                     ))) %>%
  invisible(.)

input <- suppressMessages(read_csv(here(
  str_c("OUTPUT-FILES/", score_to_norm_file_name)
)))

model <- cnorm(
  raw = input$raw,
  group = input$group,
  k = 4,
  terms = 4,
  scale = "IQ"
)

plot(model, "series", end = 8)
checkConsistency(model)

tab_names <- c(
  "6.0-6.3",
  "6.4-6.7",
  "6.8-6.11",
  "7.0-7.3",
  "7.4-7.7",
  "7.8-7.11",
  "8.0-8.5",
  "8.6-8.11",
  "9.0-9.5",
  "9.6-9.11",
  "10.0-10.5",
  "10.6-10.11",
  "11.0-11.5",
  "11.6-11.11",
  "12.0-12.5",
  "12.6-12.11",
  "13.0-13.11",
  "14.0-14.11",
  "15.0-16.11",
  "17.0-18.11"
)

# Prepare a list of data frames, each df is raw-to-ss lookup table for an age
# group.
norms_list <- rawTable(
  c(
    6.167,
    6.5,
    6.833,
    7.167,
    7.5,
    7.833,
    8.25,
    8.75,
    9.25,
    9.75,
    10.25,
    10.75,
    11.25,
    11.75,
    12.25,
    12.75,
    13.5,
    14.5,
    16,
    18.0
  ),
  model,
  step = 1,
  minNorm = 40,
  maxNorm = 130,
  minRaw = 1,
  maxRaw = score_to_norm_max_raw,
  pretty = FALSE
) %>%
  set_names(tab_names) %>%
  map(~
        select(.x, raw, norm) %>%
        summarize(raw = raw,
                  ss = round(norm, 0)))

reversal_report <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names) %>%
  pivot_longer(-raw, names_to = "agestrat", values_to = "ss") %>%
  group_by(raw) %>%
  mutate(reversal = case_when(lag(ss) < ss ~ 1)) %>%
  filter(reversal == 1) %>%
  select(raw, agestrat) %>%
  write_csv(here(
    str_c("OUTPUT-FILES/", score_to_norm_stem, "-reversal-report-age.csv")
  ))

write_xlsx(norms_list,
           here(
             str_c(
               "OUTPUT-FILES/", 
               score_to_norm_stem,
               "-raw-ss-lookup-tabbed-age.xlsx"
             )
           ))

# write raw-to-ss-lookups to single-sheet table
table <- norms_list %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw", tab_names)

write_csv(table, 
          here(
  str_c("OUTPUT-FILES/", score_to_norm_stem, "-raw-ss-lookup-table-age.csv")
))

# write model summary to text file, so you can replicate model later.
capture.output(
  str_c(score_to_norm_stem, " model summary"), 
  summary(model),
  file = here(
    str_c("OUTPUT-FILES/", score_to_norm_stem, "-model-summ-age.txt")  )
)



