################################################################################
#' Tutorial on the usage of raking weights in regression-based norming
#' with cNORM
#'
#'The script contains a step-by-step tutorial for the usage of raking weights
#'in regression-based norming with R package cNORM based on the norm sample
#'of the Peabody Picture Vocabulary Test (PPVT).
#'
################################################################################

# Load data set ----------------------------------------------------------------
suppressMessages(library(cNORM))
suppressMessages(suppressWarnings(library(tidyverse)))

# Firstly, the data set is loaded and assigned to the variable norm.data
norm_data <- ppvt
View(norm_data)

# Secondly, assigning population marginals to variable marginals.ppvt
# Note: The marginals have to be in the shown format, especially the order
# of the columns must be the variables names first, secondly, the single levels
# of the variables, and finally the single proportions.
# Each level of each factor needs its own row.
marginals_ppvt <- data.frame(var = c("sex", "sex", "migration", "migration"),
                             level = c(1,2,0,1),
                             prop = c(0.5100, 0.4900, 0.6500, 0.3500))
View(marginals_ppvt)


# In comparison: The actual composition ot the sample regarding sex and
# migration status (just for demonstration purposes)
prop.table(xtabs(~sex, data = norm_data))
prop.table(xtabs(~migration, data = norm_data))
prop.table(xtabs(~sex + migration, data = norm_data))

# Step 1: Compute and standardize raking weights -------------------------------

# Calculate standardized raking weights using computeWeights()
norm_data <- norm_data %>% 
  mutate(weights = computeWeights(data = norm_data, population.margins = marginals_ppvt))

# Let's check, which weights resulted (just for demo; not necessary)
norm_data_weights <- norm_data %>%
  group_by(sex, migration) %>%
  summarize(weights = unique(weights))
View(norm_data_weights)



# Step 2: ranking and modeling is done in a single step ------------------------
model_weighted <- cnorm(raw = norm_data$raw,
                       group = norm_data$group,
                       weights = norm_data$weights)

model_unweighted <- cnorm(raw = norm_data$raw,
                       group = norm_data$group)

# further steps like model selection
plot(model.ppvt, "subset")
normTable(c(5, 6, 7), model.ppvt)

