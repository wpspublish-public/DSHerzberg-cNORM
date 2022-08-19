norm_data1 <- norm_data %>% 
  mutate(weights = computeWeights(data = norm_data, population.margins = marginals_ppvt))
