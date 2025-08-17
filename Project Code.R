# R Script for Data Science Salary Analysis and Prediction
#
# This script combines all the data processing and modeling steps
# from the R Markdown report.

# ===================================================================
# 1. SETUP - LOAD LIBRARIES
# ===================================================================
# Make sure all these packages are installed first using install.packages("package_name")

# For data manipulation and visualization
library(tidyverse)
# For modeling and data splitting
library(caret)
# For Ridge, Lasso, and Elastic Net models
library(glmnet)
# For the XGBoost model
library(xgboost)


# ===================================================================
# 2. DATA LOADING AND CLEANING
# ===================================================================
# Load the dataset (ensure the CSV is in the same directory)
salary_data <- read.csv("Data_Science_Fields_Salary_Categorization.csv")

# Clean column names
colnames(salary_data) <- c("id", "working_year", "designation", "experience",
                           "employment_status", "salary_in_rupees", "employee_location",
                           "company_location", "company_size", "remote_working_ratio")

# Remove the unnecessary ID column
salary_data <- salary_data %>% select(-id)

# Clean the salary column and convert to numeric
salary_data$salary <- as.numeric(gsub(",", "", salary_data$salary_in_rupees))
salary_data <- salary_data %>% select(-salary_in_rupees)

# Convert character and relevant numeric columns to factors
salary_data <- salary_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(working_year = as.factor(working_year),
         remote_working_ratio = as.factor(remote_working_ratio))


# ===================================================================
# 3. OUTLIER HANDLING
# ===================================================================
# Calculate IQR bounds to identify outliers
Q1 <- quantile(salary_data$salary, 0.25)
Q3 <- quantile(salary_data$salary, 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR

# Create a new dataframe without outliers
trimmed_data <- salary_data %>% filter(salary <= upper_bound)


# ===================================================================
# 4. DATA PREPARATION FOR MODELING
# ===================================================================
# Scale the target variable (salary) and add it as a new column
trimmed_data$salary_scaled <- scale(trimmed_data$salary)

# Set a seed for reproducibility
set.seed(123)

# Create a stratified split of the data into training and testing sets
train_indices <- createDataPartition(trimmed_data$salary_scaled, p = 0.8, list = FALSE)
train_data <- trimmed_data[train_indices, ]
test_data  <- trimmed_data[-train_indices, ]

# One-Hot Encode the categorical variables
# We define the encoding scheme based on the training data
dummy_model <- dummyVars(salary_scaled ~ . - salary, data = train_data)

# Apply the encoding to the training and test data
train_processed <- as.data.frame(predict(dummy_model, newdata = train_data))
test_processed <- as.data.frame(predict(dummy_model, newdata = test_data))

# Add the target variable back to the processed dataframes
train_processed$salary_scaled <- train_data$salary_scaled
test_processed$salary_scaled <- test_data$salary_scaled


# ===================================================================
# 5. MODEL TRAINING
# ===================================================================
# Set up 10-fold cross-validation for all models
ctrl <- trainControl(method = "cv", number = 10)

# --- Model 1: Baseline Linear Regression (LM) ---
set.seed(123)
lm_model <- train(salary_scaled ~ ., data = train_processed, method = "lm", trControl = ctrl)
print("Linear Model Training Complete")

# --- Model 2: Ridge Regression (Alpha = 0) ---
set.seed(123)
ridge_model <- train(salary_scaled ~ ., data = train_processed, method = "glmnet", trControl = ctrl, tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-3, 1, length = 50)))
print("Ridge Model Training Complete")

# --- Model 3: Lasso Regression (Alpha = 1) ---
set.seed(123)
lasso_model <- train(salary_scaled ~ ., data = train_processed, method = "glmnet", trControl = ctrl, tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-4, -1, length = 50)))
print("Lasso Model Training Complete")

# --- Model 4: Elastic Net Regression ---
set.seed(123)
elastic_model <- train(salary_scaled ~ ., data = train_processed, method = "glmnet", trControl = ctrl, tuneLength = 10)
print("Elastic Net Model Training Complete")

# --- Model 5: XGBoost ---
set.seed(123)
xgb_model <- train(salary_scaled ~ .,
                   data = train_processed,
                   method = "xgbTree",
                   trControl = ctrl,
                   tuneLength = 4)
print("XGBoost Model Training Complete")

# --- Model 6: Improved Lasso (with Interaction Terms) ---
# Note: This formula uses backticks (`) around `designation.Data Scientist` to handle the space in the column name.
set.seed(123)
improved_lasso_model <- train(
  salary_scaled ~ . + experience.SE:`designation.Data Scientist`,
  data = train_processed,
  method = "glmnet",
  trControl = ctrl,
  tuneGrid = expand.grid(alpha = 1,
                         lambda = 10^seq(-4, -1, length = 50))
)
print("Improved Lasso Model Training Complete")


# ===================================================================
# 6. FINAL MODEL EVALUATION
# ===================================================================
# Make predictions on the test set for all models
pred_lm <- predict(lm_model, newdata = test_processed)
pred_ridge <- predict(ridge_model, newdata = test_processed)
pred_lasso <- predict(lasso_model, newdata = test_processed)
pred_elastic <- predict(elastic_model, newdata = test_processed)
pred_xgb <- predict(xgb_model, newdata = test_processed)
pred_improved_lasso <- predict(improved_lasso_model, newdata = test_processed)

# Create a final, comprehensive metrics table
final_metrics <- data.frame(
  Model = c("Linear Regression", "Ridge", "Lasso", "Elastic Net",
            "Improved Lasso (Interactions)", "XGBoost"),
  RMSE = c(RMSE(pred_lm, test_processed$salary_scaled),
           RMSE(pred_ridge, test_processed$salary_scaled),
           RMSE(pred_lasso, test_processed$salary_scaled),
           RMSE(pred_elastic, test_processed$salary_scaled),
           RMSE(pred_improved_lasso, test_processed$salary_scaled),
           RMSE(pred_xgb, test_processed$salary_scaled)),
  Rsquared = c(R2(pred_lm, test_processed$salary_scaled),
               R2(pred_ridge, test_processed$salary_scaled),
               R2(pred_lasso, test_processed$salary_scaled),
               R2(pred_elastic, test_processed$salary_scaled),
               R2(pred_improved_lasso, test_processed$salary_scaled),
               R2(pred_xgb, test_processed$salary_scaled))
)

# Print the sorted results table to the console
print("--- Final Model Performance Comparison ---")
print(final_metrics %>% arrange(RMSE))








