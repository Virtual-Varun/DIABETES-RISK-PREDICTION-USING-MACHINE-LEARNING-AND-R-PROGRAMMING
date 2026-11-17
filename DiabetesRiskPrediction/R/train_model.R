# ============================================================
# train_model.R
# Diabetes Risk Prediction
# Machine Learning Model Training
# ============================================================

library(caret)
library(randomForest)

source("R/Preprocessing.R")


# ============================================================
# 1. Create model and output folders
# ============================================================

if (!dir.exists("model")) {
  dir.create("model")
}

if (!dir.exists("output")) {
  dir.create("output")
}


# ============================================================
# 2. Load Dataset
# ============================================================

data <- load_diabetes_data(
  "data/diabetes.csv"
)

cat("\n====================================\n")
cat("DATASET INFORMATION\n")
cat("====================================\n")

cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")

print(head(data))


# ============================================================
# 3. Check Target Distribution
# ============================================================

cat("\n====================================\n")
cat("DIABETES DISTRIBUTION\n")
cat("====================================\n")

print(table(data$diabetes))


# ============================================================
# 4. Train/Test Split
# ============================================================

set.seed(123)

train_index <- createDataPartition(
  data$diabetes,
  p = 0.80,
  list = FALSE
)

train_data <- data[
  train_index,
]

test_data <- data[
  -train_index,
]


cat("\n====================================\n")
cat("DATA SPLIT\n")
cat("====================================\n")

cat(
  "Training samples:",
  nrow(train_data),
  "\n"
)

cat(
  "Testing samples:",
  nrow(test_data),
  "\n"
)


# ============================================================
# 5. Logistic Regression Model
# ============================================================

cat("\n====================================\n")
cat("TRAINING LOGISTIC REGRESSION\n")
cat("====================================\n")

logistic_model <- glm(
  diabetes ~
    gender +
    age +
    hypertension +
    heart_disease +
    smoking_history +
    bmi +
    HbA1c_level +
    blood_glucose_level,
  
  data = train_data,
  
  family = binomial
)

cat(
  "Logistic Regression training completed.\n"
)


# ============================================================
# 6. Random Forest Model
# ============================================================

cat("\n====================================\n")
cat("TRAINING RANDOM FOREST\n")
cat("====================================\n")

set.seed(123)

random_forest_model <- randomForest(
  
  diabetes ~
    gender +
    age +
    hypertension +
    heart_disease +
    smoking_history +
    bmi +
    HbA1c_level +
    blood_glucose_level,
  
  data = train_data,
  
  ntree = 500,
  
  importance = TRUE
)

cat(
  "Random Forest training completed.\n"
)


# ============================================================
# 7. Display Random Forest Model
# ============================================================

cat("\n====================================\n")
cat("RANDOM FOREST MODEL\n")
cat("====================================\n")

print(random_forest_model)


# ============================================================
# 8. Save Random Forest Model
# ============================================================

saveRDS(
  random_forest_model,
  "model/diabetes_model.rds"
)

cat(
  "\nRandom Forest model saved:\n"
)

cat(
  "model/diabetes_model.rds\n"
)


# ============================================================
# 9. Save Logistic Regression Model
# ============================================================

saveRDS(
  logistic_model,
  "model/logistic_model.rds"
)

cat(
  "Logistic Regression model saved:\n"
)

cat(
  "model/logistic_model.rds\n"
)


# ============================================================
# 10. Save Test Dataset
# ============================================================

saveRDS(
  test_data,
  "model/test_data.rds"
)

cat(
  "Test dataset saved.\n"
)


# ============================================================
# 11. Feature Importance
# ============================================================

importance_data <- as.data.frame(
  importance(random_forest_model)
)

importance_data$Feature <- rownames(
  importance_data
)

rownames(importance_data) <- NULL


# ============================================================
# 12. Save Feature Importance
# ============================================================

write.csv(
  importance_data,
  "output/feature_importance.csv",
  row.names = FALSE
)


# ============================================================
# 13. Final Message
# ============================================================

cat("\n====================================\n")
cat("MODEL TRAINING COMPLETED SUCCESSFULLY\n")
cat("====================================\n")

cat("\nFiles created in model folder:\n")

print(
  list.files("model")
)

cat("\nFiles created in output folder:\n")

print(
  list.files("output")
)