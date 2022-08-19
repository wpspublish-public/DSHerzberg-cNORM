norm_data <- norm_data %>% 
  bind_cols(
    computeWeights(data = norm_data, population.margins = marginals_ppvt)
    )

weights_ppvt <- computeWeights(data = norm_data, population.margins = marginals_ppvt) %>% 
  set_names("weights" =)

norm_data <- norm_data %>% 
  bind_cols(
    weights_ppvt
  )

norm_data <- norm_data %>% 
  add_column("weights" = NA) %>% 
     mutate(
         across(
             weights,
             ~
                 computeWeights(data = norm_data, population.margins = marginals_ppvt)
           )
       )
