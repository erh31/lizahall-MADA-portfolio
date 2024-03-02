
# Load here and readr for data reading
library(here)
library(readr)

# Use the here function to specify the file path
# Assuming the filename is 'Mavoglurant_A2121_nmpk.csv' and it's located in the current working directory
data_path <- here("/Users/lizahall/Desktop/School/Spring 2024/Applied Data Analysis/GitHub/lizahall-MADA-portfolio/fitting-exercise/Mavoglurant_A2121_nmpk.csv")

# Load the data
data <- read_csv(data_path)

# Inspect the first few rows of the data to ensure it's loaded correctly
head(data)


# Load ggplot2 for plotting
library(ggplot2)

# Define colors
colors <- c("#5c88da", "#84bd00", "#ffcd00")

ggplot(data, aes(x = TIME, y = DV, group = ID, color = as.factor(DOSE))) +
  geom_line() +
  scale_color_manual(values = colors) +  
  facet_wrap(~ DOSE, scales = "free", ncol = 1) +  
  labs(title = "DV vs. Time Stratified by DOSE", x = "Time", y = "DV", color = "DOSE") +
  theme_minimal()


# Load dplyr for data malipulation
library(dplyr)

# Filter the DataFrame to keep only rows where OCC equals 1
data1 <- subset(data, OCC == 1)

# Print the first few lines of the filtered dataframe
print(head(data1))

# Filter out observations where TIME is not equal to 0
data_filtered <- filter(data1, TIME != 0)

# Compute the sum of the DV variable for each individual
Y <- data_filtered %>%
  group_by(ID) %>%
  summarize(Y = sum(DV))

# Create a dataframe containing only the observations where TIME equals 0
data_TIME_0 <- filter(data1, TIME == 0)

# Combine the two dataframes using the appropriate join function
combined_data <- inner_join(Y, data_TIME_0, by = "ID")

# Convert RACE and SEX to factor variables
combined_data <- combined_data %>%
  mutate(RACE = factor(RACE),
         SEX = factor(SEX))

# Select only the desired variables
selected_data <- combined_data %>%
  select(Y, DOSE, AGE, SEX, RACE, WT, HT)

## EDA

# Load tidyr, knitr, ggsci, and corrplot for data exploration and visualization
library(tidyr)
library(knitr)
library(ggsci)
library(corrplot)


# I wanted to get a better overview of the cleaned data, so I first made a summary table of all variables. 

# Summary table for all variables
summary_data <- summary(selected_data[, c("Y", "DOSE", "AGE", "SEX", "RACE", "WT", "HT")])
print(summary_data)


# After seeing the summary, made some boxplots to further visualize

# Define colors
colors <- c("#cc1c00", "#5c88da", "#84bd00", "#ffcd00")

# Create the scatterplot of Y vs. DOSE 
ggplot(selected_data, aes(x = DOSE, y = Y)) +
  geom_point(aes(color = DOSE), alpha = 0.6, size = 3) +
  scale_color_gradientn(colors = colors) +
  labs(title = "Y vs. DOSE", x = "Dose", y = "Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(face="bold", size = 14),
        axis.title.y = element_text(face="bold", size = 14),
        legend.title = element_blank())

# Scatterplot of Y vs. AGE
ggplot(selected_data, aes(x = AGE, y = Y)) +
  geom_point(aes(color = AGE), alpha = 0.6, size = 3) +
  scale_color_gradient(low = "#5c88da", high = "#ffcd00") + 
  labs(title = "Y vs. AGE", x = "Age", y = "Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(face="bold", size = 14),
        axis.title.y = element_text(face="bold", size = 14),
        legend.title = element_text(size = 14))

# Boxplot of Y vs. SEX 
ggplot(selected_data, aes(x = as.factor(SEX), y = Y, fill = as.factor(SEX))) +
  geom_boxplot() +
  scale_fill_startrek() + 
  labs(title = "Y vs. SEX", x = "Sex", y = "Y") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))

# Boxplot of Y vs. RACE
ggplot(selected_data, aes(x = as.factor(RACE), y = Y, fill = as.factor(RACE))) +
  geom_boxplot() +
  scale_fill_startrek() + 
  labs(title = "Y vs. RACE", x = "Race", y = "Y") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))

# Distribution of WT with density curve
ggplot(selected_data, aes(x = WT)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#5c88da", color = "black") + 
  geom_density(alpha = 0.5, color = "black") + 
  theme_minimal() + 
  ggtitle("Distribution of WT")

# Distribution of HT with density curve
ggplot(data, aes(x = HT)) + 
  geom_histogram(aes(y = ..density..), fill = "#ffcd00", color = "black") + 
  geom_density(alpha = 0.5, color = "black") + 
  theme_minimal() + 
  ggtitle("Distribution of HT")


# Convert variables_of_interest to numeric
variables_of_interest <- as.data.frame(sapply(selected_data, as.numeric))

# Calculate correlation matrix for the variables of interest
corr_matrix <- cor(variables_of_interest, use = "complete.obs")

# Your custom colors
my_colors <- colorRampPalette(c("#5c88da", "white", "#ffcd00"))(200) 

# Plotting the correlation matrix with custom colors
corrplot(corr_matrix, method = "color", col = my_colors,
         tl.col="black", tl.srt=45) # Text label color and rotation


# MODEL FITTING

# Load tidymodels for model fitting 
library(tidymodels)

#LINEAR MODELS
# Define the model specification
linear_spec <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

# Model with DOSE as predictor
# Fit the model
model_dose <- linear_spec %>% 
  fit(Y ~ DOSE, data = selected_data)

# Summarize the model
summary(model_dose$fit)


# Model with all predictors
# Fit the model
model_all <- linear_spec %>% 
  fit(Y ~ ., data = selected_data)

# Summarize the model
summary(model_all$fit)

#CALCULATE RMSE AND RSQUARED
# RMSE and R-squared for model with DOSE
rmse_dose <- model_dose %>% 
  predict(new_data = selected_data) %>% 
  bind_cols(selected_data) %>% 
  metrics(truth = Y, estimate = .pred) %>% 
  filter(.metric %in% c("rmse", "rsq"))

# RMSE and R-squared for model with all predictors
rmse_all <- model_all %>% 
  predict(new_data = selected_data) %>% 
  bind_cols(selected_data) %>% 
  metrics(truth = Y, estimate = .pred) %>% 
  filter(.metric %in% c("rmse", "rsq"))

print(rmse_dose)
print(rmse_all)


#LOGISTICS MODELS
# Define the logistic model specification
logistic_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# Model with DOSE as predictor
# Fit the model
logistic_dose <- logistic_spec %>% 
  fit(SEX ~ DOSE, data = selected_data)

# Model with all predictors
# Fit the model
logistic_all <- logistic_spec %>% 
  fit(SEX ~ ., data = selected_data)


# ACCURACY AND ROC-AUC
# Accuracy and ROC-AUC for logistic model with DOSE
acc_dose <- logistic_dose %>% 
  predict(new_data = selected_data, type = "prob") %>% 
  bind_cols(selected_data) %>% 
  roc_auc(truth = SEX, .pred_1) 
# Assuming SEX has two levels, with the second level being the event of interest

# Accuracy and ROC-AUC for logistic model with all predictors
acc_all <- logistic_all %>% 
  predict(new_data = selected_data, type = "prob") %>% 
  bind_cols(selected_data) %>% 
  roc_auc(truth = SEX, .pred_1)

print(acc_dose)
print(acc_all)


# K-NEAREST NEIGHBOR

library(kknn)

# K-Nearest Neighbors Model for Continuous Outcome (Y)
# Define KNN model specification for regression
knn_spec_regression <- nearest_neighbor(neighbors = 5) %>%  # You can adjust the number of neighbors
  set_engine("kknn") %>% 
  set_mode("regression")

# Fit KNN model for Y with DOSE as the predictor
knn_fit_Y_DOSE <- knn_spec_regression %>% 
  fit(Y ~ DOSE, data = selected_data)

# Fit KNN model for Y with all predictors
knn_fit_Y_all <- knn_spec_regression %>% 
  fit(Y ~ ., data = selected_data)

# Assuming you have a test dataset or you split your selected_data into training and testing
set.seed(123)  # For reproducibility
data_split <- initial_split(selected_data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Predictions
predictions_Y_DOSE <- predict(knn_fit_Y_DOSE, new_data = test_data) %>% 
  bind_cols(test_data)

predictions_Y_all <- predict(knn_fit_Y_all, new_data = test_data) %>% 
  bind_cols(test_data)

# Compute RMSE and R-squared for both models
metrics_Y_DOSE <- metrics(predictions_Y_DOSE, truth = Y, estimate = .pred)
metrics_Y_all <- metrics(predictions_Y_all, truth = Y, estimate = .pred)

print(metrics_Y_DOSE)
print(metrics_Y_all)


# K-Nearest Neighbors Model for Categorical Outcome (SEX)
# Define KNN model specification for classification
knn_spec_classification <- nearest_neighbor(neighbors = 5) %>%  # Adjust neighbors as needed
  set_engine("kknn") %>% 
  set_mode("classification")

# Fit KNN model for SEX with DOSE as the predictor
knn_fit_SEX_DOSE <- knn_spec_classification %>% 
  fit(SEX ~ DOSE, data = selected_data)

# Fit KNN model for SEX with all predictors
knn_fit_SEX_all <- knn_spec_classification %>% 
  fit(SEX ~ ., data = selected_data)

# Predictions
predictions_SEX_DOSE <- predict(knn_fit_SEX_DOSE, new_data = test_data, type = "prob") %>% 
  bind_cols(test_data)

predictions_SEX_all <- predict(knn_fit_SEX_all, new_data = test_data, type = "prob") %>% 
  bind_cols(test_data)

# Compute Accuracy and ROC-AUC for both models
metrics_SEX_DOSE <- roc_auc(predictions_SEX_DOSE, truth = SEX, .pred_1) # Adjust based on factor levels
metrics_SEX_all <- roc_auc(predictions_SEX_all, truth = SEX, .pred_1)

print(metrics_SEX_DOSE)
print(metrics_SEX_all)
print(metrics_Y_DOSE)
print(metrics_Y_all)
print(acc_dose)
print(acc_all)
print(rmse_dose)
print(rmse_all)


