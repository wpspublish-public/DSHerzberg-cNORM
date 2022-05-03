norm.data.split <- norm.data %>% group_by(sex, migration) %>% summarize(weights = unique(weights))
norm.data.splitted <- norm.data %>% group_by(sex, migration) %>% summarize_at(vars(weights),.funs = "unique")
