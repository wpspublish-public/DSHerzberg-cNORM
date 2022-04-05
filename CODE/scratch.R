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


raw  ss
 1  93
 2  96
 3  98
 4 101
 5 104
 6 106
 7 109
 8 112
 9 114
10 117
11 120
12 122
13 125
14 128
15 130
16 130
17 130
18 130
19 130
20 130
21 130
22 130
23 130
24 130
25 130
26 130
27 130
28 130
29 130
30 130
31 130
32 130
33 130
34 130
35 130
36 130
37 130
38 130
39 130
40 130
41 130
42 130
43 130
44 130
