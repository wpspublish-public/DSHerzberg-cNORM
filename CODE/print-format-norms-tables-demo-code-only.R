suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(writexl))

input_test_names <- c("lske", "lswe", "rhme", "rlne", "sege", "snwe")
output_test_names <- c("LSK-E", "LSW-E", "RHY-E", "RLN-E", "SEG-E", "SPW-E")
tod_form <- "TOD-E"
norm_type <- "age"
input_file_path <- "INPUT-FILES/PRINT-FORMAT-NORMS-TABLES/"
output_file_path <- "OUTPUT-FILES/PRINT-FORMAT-NORMS-TABLES/"

input_files_ta <- map(
  input_test_names,
  ~
  suppressMessages(read_csv(here(str_c(
  input_file_path, .x, "-", norm_type, ".csv"
))))
) %>% 
  set_names(input_test_names)

perc_ss_cols <- suppressMessages(read_csv(here(str_c(
  input_file_path, "perc-ss-cols.csv"
))))

age_strat <- input_files_ta[[1]] %>% 
  select(-raw) %>% 
  names()

print_lookups_ta <- input_files_ta %>%
  map(~
        .x %>% 
  pivot_longer(contains("-"), names_to = "age_strat", values_to = "ss") %>%
  arrange(age_strat) %>%
  group_by(age_strat) %>%
  complete(ss = 40:130) %>%
  group_by(age_strat, ss) %>%
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  summarize(raw = str_c(raw, collapse = '--')) %>%
  mutate(across(raw, ~ case_when(is.na(.x) ~ '-', TRUE ~ .x))) %>%
  arrange(age_strat, desc(ss)) %>%
  pivot_wider(names_from = age_strat,
              values_from = raw) %>%
  filter(!is.na(ss)) %>%
  right_join(perc_ss_cols, by = "ss") %>%
  relocate(perc, .before = "ss")
) %>% 
  set_names(input_test_names)

age_strat_cols_ta <-  print_lookups_ta %>%
  map( ~
         map(age_strat,
             ~
               .y %>%
               select(perc, ss,!!sym(.x)), .y = .x) %>%
         set_names(age_strat))

age_test_names_flat <- cross2(age_strat, input_test_names) %>% 
  map_chr(str_c, collapse = "_")

age_test_cols_flat <- flatten(age_strat_cols_ta) %>% 
  set_names(age_test_names_flat)

# age_test_cols_at reconstitutes a new two-level list from the flattened
# as_tn_dfs. Whereas in age_strat_cols_ta, the hierarchy was test --> age_strat,
# in age_test_cols_at, that hierarchy is reversed to age_strat --> test, which
# is the hierarchy required for the final output. Now we have a list of lists in
# which the top level is a list for each age_strat, and within those lists are
# the list of lookup tables for that age_strat, for all six tests. This
# transformation is accomplished via purrr::keep(), which subsets list elements.
# Using map(), we subset age_test_cols_flat 9 times, mapping across the 9
# elements of age_strat. str_detect() matches the pattern supplied by .x (here a
# particular age_strat) against the names of the flattened age_test_cols_flat,
# and returns list elements whose names contain a match for the the pattern
# (i.e., a particular age-strat). These retained elements are complete sets of
# test-specific lookup columns, one set for each agestrat, giving the new
# two-level list the desired output hierarchy (age_strat --> test).
age_test_cols_at <- map(
  age_strat,
  ~
  keep(age_test_cols_flat, str_detect(names(age_test_cols_flat), .x))
)

# print_lookups_at is a transformation of age_test_cols_at into a list suitable
# for writing out as a tabbed .xlsx file. Recall that each element of
# age_test_cols_at is a list of the test-wise look up dfs for each
# age_strat. reduce() joins the list of dfs into a single df with right-ward raw
# score lookup cols for each test within each age_strat. The call of rename_with
# renames the raw score cols of the newly joined dfs, taking advantage of a
# shorthand in which the renaming function (the first argument of rename_with is
# specified as the vector or new) is simple a vector containing the new col
# names, and the second argument is a vector of equal length naming the cols to
# be renamed (here those cols are collected using the tidy select helper
# contains()). We then name the nine new age-strat specific lookup tables with
# the names of their corresponding age_strats, using set_names. Naming the list
# elements here is crucial for output, as these names become the tabbed names on
# the .xlsx output file.
print_lookups_at <- age_test_cols_at %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>% 
      rename_with(~ output_test_names, contains("-"))
  ) %>% 
  set_names(age_strat)

# Write raw-to-ss lookups by agestrat into tabbed, xlsx workbook. To create
# named tabs, supply writexl::write_xlsx() with a named list of dfs for each
# tab, tab names will be the names of the list elements
write_xlsx(print_lookups_at,
           here(
             str_c(
               output_file_path, tod_form, "-print-lookup-tables-", norm_type, ".xlsx"
             ))
           )




