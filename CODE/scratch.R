temp1 <- input_files_ta[[6]] %>%
        pivot_longer(contains("-"), names_to = "age_strat", values_to = "ss") %>%
        arrange(age_strat) %>%
        group_by(age_strat) %>%
        complete(ss = 40:130) %>%
        group_by(age_strat, ss) 

temp2 <- temp1[80:95, ]

print(temp2, n = nrow(temp2))
