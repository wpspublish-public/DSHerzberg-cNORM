temp1 <- input_files_ta[[6]] %>%
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
        filter(!is.na(ss))
        

temp2 <- temp1[80:95, ]

print(temp2, n = nrow(temp2))
