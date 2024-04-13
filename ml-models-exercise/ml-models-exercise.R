#---------------------------------------------------------------

# Load required libraries
library(here)
library(readr)
library(tidymodels)
library(ggplot2)
library(GGally)
library(dplyr)
library(ranger)
library(glmnet)
library(yardstick)
library(patchwork)
library(knitr)
library(kableExtra)

#---------------------------------------------------------------

# PRELIMINARIES
# Load in cleaned data 
data <- readRDS(here("ml-models-exercise", "cleandata.rds"))

# Set value for rndseed
rngseed <- 1234

#---------------------------------------------------------------

# MORE PROCESSING
# Replace 7 and 88 with 3 for RACE variable 
levels(data$RACE)[levels(data$RACE) == "7" | levels(data$RACE) == "88"] <- "3"

#---------------------------------------------------------------

# PAIRWISE CORRELATIONS
# Select only continuous variables
data_continuous <- data[, c("DOSE", "AGE", "WT", "HT")]

# Create a pairwise correlation plot
pairwise_plot <- ggpairs(data_continuous[, c("DOSE", "AGE", "WT", "HT")],
                         upper = list(continuous = wrap("cor", size = 4)),
                         lower = list(continuous = wrap("points", size = 1, alpha = 0.5, colour = "#5c88da")),
                         diag = list(continuous = wrap("barDiag", fill = "#ffcd00"))
) +
  theme_light() +  
  ggtitle("Pairwise Correlation Plot") +  
  theme(plot.title = element_text(hjust = 0.5),  
        text = element_text(size = 12),  
        axis.text.x = element_text(angle = 45, hjust = 1))  

print(pairwise_plot)

#---------------------------------------------------------------

# FEATURE ENGINEERING
# Creating dataframe data_wBMI
data_wBMI <- data

# Calculating BMI based on WT (kg) and HT (m) variables
data_wBMI$BMI <- data$WT / (data$HT^2)

#---------------------------------------------------------------


# MODEL BUILDING

# Defining models
# Linear model with all predictors 
linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# LASSO regression model
lasso_model <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Random forest model
random_forest_model <- rand_forest() %>%
  set_engine("ranger", seed = rngseed) %>%
  set_mode("regression")

#------------------------

# Creating workflows
# Define recipes
recipe <- recipe(Y ~ ., data = data_wBMI)
recipe_lasso <- recipe(Y ~ ., data = data_wBMI) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Linear model workflow
linear_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(linear_model)

# LASSO model workflow
lasso_workflow <- workflow() %>%
  add_recipe(recipe_lasso) %>%
  add_model(lasso_model)

# Random forest model workflow
rf_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(random_forest_model)

#------------------------

# Fitting the models
# Fit the linear model
linear_fit <- fit(linear_workflow, data = data_wBMI)

# Fit the LASSO model
lasso_fit <- fit(lasso_workflow, data = data_wBMI)

# Fit the random forest model
rf_fit <- fit(rf_workflow, data = data_wBMI)

#------------------------

# Predictions
# Predictions for the linear model
linear_preds <- predict(linear_fit, new_data = data_wBMI) %>%
  bind_cols(data_wBMI) %>%
  rename(predicted = .pred)

# Predictions for the LASSO model
lasso_preds <- predict(lasso_fit, new_data = data_wBMI) %>%
  bind_cols(data_wBMI) %>%
  rename(predicted = .pred)

# Predictions for the random forest model
rf_preds <- predict(rf_fit, new_data = data_wBMI) %>%
  bind_cols(data_wBMI) %>%
  rename(predicted = .pred)

#------------------------

# Calculating RMSE for each model
linear_rmse <- rmse(linear_preds, truth = Y, estimate = predicted) # Linear Model
lasso_rmse <- rmse(lasso_preds, truth = Y, estimate = predicted) # LASSO Model
rf_rmse <- rmse(rf_preds, truth = Y, estimate = predicted) # Random Forest Model

#------------------------

# Plotting first fit
# Define colors and shapes for the legend
colors <- c("Predicted" = "#5c88da", "Observed" = "#ffcd00", "Reference Line" = "gray")

# Linear Model
p_linear <- ggplot(data_wBMI, aes(x = Y)) +
  geom_point(aes(y = linear_preds$predicted, color = "Predicted"), show.legend = FALSE) +
  geom_point(aes(y = Y, color = "Observed"), show.legend = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = colors["Reference Line"]) +
  labs(title = "Linear Model", y = "Predicted", x = "Observed") +
  scale_color_manual(values = colors) +
  theme_minimal()

# LASSO Model
p_lasso <- ggplot(data_wBMI, aes(x = Y)) +
  geom_point(aes(y = lasso_preds$predicted, color = "Predicted")) +
  geom_point(aes(y = Y, color = "Observed")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = colors["Reference Line"]) +
  labs(title = "LASSO Model", y = "Predicted", x = "Observed", color = "Legend") +
  scale_color_manual(values = colors) +
  theme_minimal()

# Random Forest Model
p_rf <- ggplot(data_wBMI, aes(x = Y)) +
  geom_point(aes(y = rf_preds$predicted, color = "Predicted"), show.legend = FALSE) +
  geom_point(aes(y = Y, color = "Observed"), show.legend = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = colors["Reference Line"]) +
  labs(title = "Random Forest Model", y = "Predicted", x = "Observed") +
  scale_color_manual(values = colors) +
  theme_minimal()

# Combine plots
p_combined <- (p_linear + p_lasso + p_rf) + plot_layout(ncol = 1)
p_combined + 
  plot_annotation(title = 'First Fit - Predicted vs Observed Values',
                  theme = theme(plot.title = element_text(size = 10))) & 
  theme(text = element_text('mono'))
print(p_combined)

# Create a data frame with your RMSE values
rmse_data <- data.frame(
  Model = c("Linear Regression", "Lasso Regression", "Random Forest"),
  RMSE = c(linear_rmse$.estimate, lasso_rmse$.estimate, rf_rmse$.estimate)
)

# Print the table using kable and kableExtra
kable(rmse_data, "html", align = "c", caption = "RMSE Values for Different Models") %>%
  kable_styling(full_width = FALSE)

# You should find that linear model and the LASSO give pretty much the same results. 
#Can you explain why that is?

#---------------------------------------------------------------


# TUNING THE MODELS (BAD)

#LASSO Model

# Define a grid of penalty values correctly
penalty_grid <- grid_regular(penalty(range = c(log10(1E-5), log10(1E2))), levels = 50)

# Adjust the LASSO model specification for tuning
lasso_model_tune <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Create workflow
lasso_workflow_tune <- workflow() %>%
  add_recipe(recipe_lasso) %>%  # Make sure recipe_lasso is defined correctly
  add_model(lasso_model_tune)

# Create an apparent resamples object for the apparent error rate
apparent_resamples <- apparent(data_wBMI)

# Tune the model
tuning_results <- tune_grid(
  object = lasso_workflow_tune,
  resamples = apparent_resamples,
  grid = penalty_grid
)


# Visualize the tuning process
autoplot(tuning_results)

#------------------------

# Random Forest Model

# Define the random forest model with tunable parameters
random_forest_model_tune <- rand_forest(trees = 300, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger", seed = rngseed) %>%
  set_mode("regression")

# Create a tuning grid
tuning_grid <- grid_regular(
  mtry(range = c(1, 7)),
  min_n(range = c(1, 21)),
  levels = 7
)

# Create workflow
rf_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(random_forest_model_tune)

# Tune random forest model
tune_res <- tune_grid(
  rf_workflow,
  resamples = vfold_cv(data_wBMI, v = 5), # Adjust the number of folds if necessary
  grid = tuning_grid
)



autoplot(tune_res)

#---------------------------------------------------------------


# TUNING THE MODELS WITH CV

# LASSO Model

# Define a grid of penalty values correctly
penalty_grid <- grid_regular(penalty(range = c(log10(1E-5), log10(1E2))), levels = 50)

# Adjust the LASSO model specification for tuning
lasso_model_tune_cv <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Adjust the workflow for the tunable model
lasso_workflow_tune_cv <- workflow() %>%
  add_recipe(recipe_lasso) %>%  # Make sure recipe_lasso is defined correctly
  add_model(lasso_model_tune_cv)

# Create the cross-validation resamples
cv_resamples <- vfold_cv(data_wBMI, v = 5, repeats = 5)

# Tune LASSO model using cross-validation 
tuning_results_lasso_cv <- tune_grid(
  object = lasso_workflow_tune_cv,
  resamples = cv_resamples,
  grid = penalty_grid
)

# Plot the tuning results
autoplot(tuning_results_lasso_cv)

#------------------------

# Random Forest Model

# Define the random forest model with tunable parameters
random_forest_model_tune_cv <- rand_forest(trees = 300, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger", seed = rngseed) %>%
  set_mode("regression")

# Create a tuning grid
tuning_grid <- grid_regular(
  mtry(range = c(1, 7)),
  min_n(range = c(1, 21)),
  levels = 7
)

# Create workflow
rf_workflow_cv <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(random_forest_model_tune_cv)

# Tune the random forest model
tune_res_cv <- tune_grid(
  rf_workflow_cv,
  resamples = cv_resamples,
  grid = tuning_grid
)

# Plot the tuning results
autoplot(tune_res_cv)


