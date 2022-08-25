age_strat <-   c(3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 
                 11.5, 12.5, 13.5, 14.5, 15.5, 16.5)
norms_list_weighted <- rawTable(
  age_strat,
  model_weighted,
  step = 1,
  minNorm = 40,
  maxNorm = 130,
  minRaw = 7,
  maxRaw = 221,
  pretty = FALSE
) %>%
  map(~
        select(.x, raw, norm) %>%
        summarize(raw = raw,
                  ss = round(norm, 0)))

table_weighted <- norms_list_weighted %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw",
            str_c(age_strat, "_w"))

norms_list_unweighted <- rawTable(
  age_strat,
  model_unweighted,
  step = 1,
  minNorm = 40,
  maxNorm = 130,
  minRaw = 7,
  maxRaw = 221,
  pretty = FALSE
) %>%
  map(~
        select(.x, raw, norm) %>%
        summarize(raw = raw,
                  ss = round(norm, 0)))

table_unweighted <- norms_list_unweighted %>%
  reduce(left_join,
         by = "raw") %>%
  set_names("raw",
            str_c(age_strat, "_uw"))


comp_w_uw <-full_join(
  table_weighted,
  table_unweighted,
  by = "raw"
) %>% 
  select(-raw)

chi <- map_dbl(seq_len(nrow(comp_w_uw)),
                ~
                  chisq.test(matrix(as.numeric(comp_w_uw[.x, 1:ncol(comp_w_uw)]), nrow = 2, ncol(comp_w_uw)/2, 2))$statistic)
comp_w_uw$chi_square <- chi

chi_sq_max <- max(comp_w_uw$chi_square)
df <- length(age_strat) - 1

if_else(chi_sq_max < chi_sq_crit,
        "no diff",
        "diff")

# write the function to get chi-square statistics
chi_sq_outcome <- function(x) {
  chi_sq_max <- max(comp_w_uw$chi_square)
  df <- length(x) - 1
  chi_sq_crit <- qchisq(.05, df)
  if_else(chi_sq_max < chi_sq_crit,
          "Weighted, unweighted lookup tables DO NOT DIFFER significantly (per chi-square test).",
          "Weighted, unweighted lookup tables DIFFER significantly (per chi-square test)")
}
