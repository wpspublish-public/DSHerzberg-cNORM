suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(writexl))

input_test_names <- c("lske", "lswe", "rhme", "rlne", "sege", "snwe")
output_test_names <- c("LSK-E", "LSW-E", "RHY-E", "RLN-E", "SEG-E", "SPW-E")
tod_form <- "TOD-E"
norm_type <- "age"
urlRemote_path  <- "https://raw.github.com/"
github_input_path <- "wpspublish-public/DSHerzberg-cNORM/master/INPUT-FILES/PRINT-FORMAT-NORMS-TABLES/"

input_files_ta <- map(
  input_test_names,
  ~
  suppressMessages(read_csv(url(str_c(
  urlRemote_path, github_input_path, .x, "-", norm_type, ".csv"
))))
) %>% 
  set_names(input_test_names)

perc_ss_cols <- suppressMessages(read_csv(url(str_c(
  urlRemote_path, github_input_path, "perc-ss-cols.csv"
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

age_test_cols_at <- map(
  age_strat,
  ~
  keep(age_test_cols_flat, str_detect(names(age_test_cols_flat), .x))
)

print_lookups_at <- age_test_cols_at %>% 
  map(
    ~
      .x %>% 
      reduce(left_join, by = c("perc", "ss")) %>% 
      rename_with(~ output_test_names, contains("-"))
  ) %>% 
  set_names(age_strat)

write_xlsx(print_lookups_at,
           here(
             str_c(
               [INSERT LOCAL FILE PATH], "-print-lookup-tables-", norm_type, ".xlsx"
             ))
)


