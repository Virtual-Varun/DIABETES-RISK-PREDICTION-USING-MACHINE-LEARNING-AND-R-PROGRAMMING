# ============================================================
# Predict.R
# Diabetes Risk Prediction using Random Forest
# Supports both Single-Patient and Batch Dataset Predictions
# ============================================================

library(randomForest)

# Global model cache to avoid reloading on repeated requests
.cached_rf_model <- NULL
.cached_model_path <- ""

# Helper to get or load cached model
get_rf_model <- function(model_path = "../model/diabetes_model.rds") {
  if (!file.exists(model_path)) {
    # Try alternative relative paths
    alt_paths <- c(
      model_path,
      file.path("model", "diabetes_model.rds"),
      file.path("..", "model", "diabetes_model.rds"),
      file.path(getwd(), "model", "diabetes_model.rds")
    )
    found <- FALSE
    for (p in alt_paths) {
      if (file.exists(p)) {
        model_path <- p
        found <- TRUE
        break
      }
    }
    if (!found) {
      stop(paste("Random Forest model file not found at:", model_path))
    }
  }
  
  if (is.null(.cached_rf_model) || .cached_model_path != model_path) {
    model <- readRDS(model_path)
    if (!inherits(model, "randomForest")) {
      stop("Loaded object is not a valid randomForest model.")
    }
    .cached_rf_model <<- model
    .cached_model_path <<- model_path
  }
  
  return(.cached_rf_model)
}


# ============================================================
# 1. SINGLE PATIENT PREDICTION (Preserved & Enhanced)
# ============================================================

predict_diabetes_risk <- function(
    input_data,
    model_path = "../model/diabetes_model.rds"
) {
  rf_model <- get_rf_model(model_path)
  
  required_columns <- c(
    "gender",
    "age",
    "hypertension",
    "heart_disease",
    "smoking_history",
    "bmi",
    "HbA1c_level",
    "blood_glucose_level"
  )
  
  missing_columns <- setdiff(required_columns, names(input_data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required input features:", paste(missing_columns, collapse = ", ")))
  }
  
  # Strictly select only the 8 input features — target column is NEVER used as input feature
  input_data <- input_data[, required_columns, drop = FALSE]
  
  # Format categorical features matching model factor levels
  gender_levels <- rf_model$forest$xlevels$gender
  if (is.null(gender_levels)) gender_levels <- c("Female", "Male", "Other")
  
  input_gender <- trimws(as.character(input_data$gender))
  match_g <- match(tolower(input_gender), tolower(gender_levels))
  if (is.na(match_g)) {
    stop(paste("Invalid gender. Allowed values:", paste(gender_levels, collapse = ", ")))
  }
  input_data$gender <- factor(gender_levels[match_g], levels = gender_levels)
  
  smoking_levels <- rf_model$forest$xlevels$smoking_history
  if (is.null(smoking_levels)) smoking_levels <- c("current", "ever", "former", "never", "No Info", "not current")
  
  input_smk <- gsub("_", " ", trimws(as.character(input_data$smoking_history)))
  match_s <- match(tolower(input_smk), tolower(smoking_levels))
  if (is.na(match_s)) {
    stop(paste("Invalid smoking history. Allowed values:", paste(smoking_levels, collapse = ", ")))
  }
  input_data$smoking_history <- factor(smoking_levels[match_s], levels = smoking_levels)
  
  # Format binary features (0 or 1)
  hyp_val <- tolower(trimws(as.character(input_data$hypertension)))
  input_data$hypertension <- if (hyp_val %in% c("1", "yes", "y", "true")) 1 else 0
  
  hd_val <- tolower(trimws(as.character(input_data$heart_disease)))
  input_data$heart_disease <- if (hd_val %in% c("1", "yes", "y", "true")) 1 else 0
  
  # Format numeric features
  input_data$age <- as.numeric(input_data$age)
  input_data$bmi <- as.numeric(input_data$bmi)
  input_data$HbA1c_level <- as.numeric(input_data$HbA1c_level)
  input_data$blood_glucose_level <- as.numeric(input_data$blood_glucose_level)
  
  if (any(is.na(input_data))) {
    stop("Input contains invalid or missing values after preprocessing.")
  }
  
  # Predict probability
  probability <- predict(rf_model, newdata = input_data, type = "prob")
  
  model_classes <- colnames(probability)
  pos_col <- if ("Yes" %in% model_classes) "Yes" else if ("1" %in% model_classes) "1" else model_classes[2]
  
  yes_probability <- as.numeric(probability[, pos_col])
  
  prediction <- ifelse(yes_probability >= 0.5, "Diabetes", "No Diabetes")
  risk_percentage <- round(yes_probability * 100, 2)
  
  risk_level <- ifelse(
    risk_percentage >= 70,
    "High",
    ifelse(risk_percentage >= 40, "Moderate", "Low")
  )
  
  return(list(
    prediction = prediction,
    predicted_class = prediction,
    probability = round(yes_probability, 4),
    risk_percentage = risk_percentage,
    risk_level = risk_level,
    model_type = "Random Forest (500 trees)",
    disclaimer = "Statistical estimate only — not a confirmed medical diagnosis."
  ))
}


# ============================================================
# 2. BATCH DATASET PREDICTION (For Uploaded CSV Datasets)
# ============================================================

predict_diabetes_batch <- function(
    input_data,
    model_path = "../model/diabetes_model.rds",
    low_threshold = 40,
    high_threshold = 70
) {
  rf_model <- get_rf_model(model_path)
  
  required_columns <- c(
    "gender",
    "age",
    "hypertension",
    "heart_disease",
    "smoking_history",
    "bmi",
    "HbA1c_level",
    "blood_glucose_level"
  )
  
  # Check required columns
  missing_columns <- setdiff(required_columns, names(input_data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns in dataset:", paste(missing_columns, collapse = ", ")))
  }
  
  # Extract features only — NEVER include 'diabetes' target feature
  features_df <- input_data[, required_columns, drop = FALSE]
  
  # Clean & format gender
  gender_levels <- rf_model$forest$xlevels$gender
  if (is.null(gender_levels)) gender_levels <- c("Female", "Male", "Other")
  clean_gender <- trimws(as.character(features_df$gender))
  match_g <- match(tolower(clean_gender), tolower(gender_levels))
  features_df$gender <- factor(gender_levels[match_g], levels = gender_levels)
  
  # Clean & format smoking history
  smoking_levels <- rf_model$forest$xlevels$smoking_history
  if (is.null(smoking_levels)) smoking_levels <- c("current", "ever", "former", "never", "No Info", "not current")
  clean_smk <- gsub("_", " ", trimws(as.character(features_df$smoking_history)))
  match_s <- match(tolower(clean_smk), tolower(smoking_levels))
  features_df$smoking_history <- factor(smoking_levels[match_s], levels = smoking_levels)
  
  # Clean & format binary variables
  hyp_raw <- tolower(trimws(as.character(features_df$hypertension)))
  features_df$hypertension <- ifelse(hyp_raw %in% c("1", "yes", "y", "true"), 1, 0)
  
  hd_raw <- tolower(trimws(as.character(features_df$heart_disease)))
  features_df$heart_disease <- ifelse(hd_raw %in% c("1", "yes", "y", "true"), 1, 0)
  
  # Clean numeric variables
  features_df$age <- suppressWarnings(as.numeric(features_df$age))
  features_df$bmi <- suppressWarnings(as.numeric(features_df$bmi))
  features_df$HbA1c_level <- suppressWarnings(as.numeric(features_df$HbA1c_level))
  features_df$blood_glucose_level <- suppressWarnings(as.numeric(features_df$blood_glucose_level))
  
  # Handle invalid / incomplete rows
  valid_mask <- complete.cases(features_df)
  total_records <- nrow(features_df)
  valid_count <- sum(valid_mask)
  
  if (valid_count == 0) {
    stop("No valid records found in the provided dataset.")
  }
  
  # Predict on valid subset
  valid_features <- features_df[valid_mask, , drop = FALSE]
  probabilities <- predict(rf_model, newdata = valid_features, type = "prob")
  
  model_classes <- colnames(probabilities)
  pos_col <- if ("Yes" %in% model_classes) "Yes" else if ("1" %in% model_classes) "1" else model_classes[2]
  
  yes_probs <- as.numeric(probabilities[, pos_col])
  predicted_classes <- ifelse(yes_probs >= 0.5, "Diabetes", "No Diabetes")
  risk_percentages <- round(yes_probs * 100, 2)
  
  # Dynamic risk level based on user-configurable thresholds
  risk_levels <- ifelse(
    risk_percentages >= high_threshold,
    "High",
    ifelse(risk_percentages >= low_threshold, "Moderate", "Low")
  )
  
  # Prepare prediction results dataframe
  results <- data.frame(
    patient_id = paste0("PT-", sprintf("%05d", seq_len(valid_count))),
    row_number = which(valid_mask),
    gender = as.character(valid_features$gender),
    age = valid_features$age,
    hypertension = valid_features$hypertension,
    heart_disease = valid_features$heart_disease,
    smoking_history = as.character(valid_features$smoking_history),
    bmi = valid_features$bmi,
    HbA1c_level = valid_features$HbA1c_level,
    blood_glucose_level = valid_features$blood_glucose_level,
    predicted_diabetes = predicted_classes,
    probability = round(yes_probs, 4),
    risk_percentage = risk_percentages,
    risk_level = risk_levels,
    stringsAsFactors = FALSE
  )
  
  # If ground truth 'diabetes' is present in original input, include it for comparison
  if ("diabetes" %in% names(input_data)) {
    orig_target <- input_data$diabetes[valid_mask]
    target_clean <- ifelse(
      tolower(trimws(as.character(orig_target))) %in% c("1", "yes", "true"),
      "Diabetes",
      "No Diabetes"
    )
    results$actual_diabetes <- target_clean
  }
  
  # Calculate summary metrics for dashboard
  total_predicted <- nrow(results)
  diabetes_count <- sum(results$predicted_diabetes == "Diabetes")
  nondiabetes_count <- sum(results$predicted_diabetes == "No Diabetes")
  pos_pct <- round((diabetes_count / total_predicted) * 100, 2)
  
  low_risk_count <- sum(results$risk_level == "Low")
  mod_risk_count <- sum(results$risk_level == "Moderate")
  high_risk_count <- sum(results$risk_level == "High")
  
  summary_stats <- list(
    total_records = total_records,
    valid_records = total_predicted,
    invalid_records = total_records - total_predicted,
    predicted_diabetes_cases = diabetes_count,
    predicted_nondiabetes_cases = nondiabetes_count,
    predicted_positive_percentage = pos_pct,
    average_risk_percentage = round(mean(results$risk_percentage), 2),
    low_risk_count = low_risk_count,
    moderate_risk_count = mod_risk_count,
    high_risk_count = high_risk_count,
    avg_age = round(mean(results$age), 1),
    avg_bmi = round(mean(results$bmi), 1),
    avg_hba1c = round(mean(results$HbA1c_level), 2),
    avg_glucose = round(mean(results$blood_glucose_level), 1),
    gender_counts = as.list(table(results$gender)),
    thresholds = list(low = low_threshold, high = high_threshold)
  )
  
  return(list(
    summary = summary_stats,
    predictions = results,
    model_info = list(
      model_type = "Random Forest Classifier",
      trees = 500,
      features_used = required_columns
    ),
    disclaimer = "Model-based predictions only. Not a medical diagnosis."
  ))
}
