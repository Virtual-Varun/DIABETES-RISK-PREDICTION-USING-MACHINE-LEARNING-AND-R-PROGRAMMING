# ============================================================
# Preprocessing.R
# Diabetes Risk Prediction - Data Cleaning and Validation
# ============================================================

# Required input features (8 clinical features)
REQUIRED_COLUMNS <- c(
  "gender",
  "age",
  "hypertension",
  "heart_disease",
  "smoking_history",
  "bmi",
  "HbA1c_level",
  "blood_glucose_level"
)

# Standard factor levels matching the trained Random Forest model
VALID_GENDER_LEVELS <- c("Female", "Male", "Other")
VALID_SMOKING_LEVELS <- c("current", "ever", "former", "never", "No Info", "not current")


# ============================================================
# 1. Clean Diabetes Data (Base Cleaning)
# ============================================================

clean_diabetes_data <- function(data) {
  # Remove leading/trailing spaces from column names
  names(data) <- trimws(names(data))
  
  # Ensure required columns are present
  missing_cols <- setdiff(REQUIRED_COLUMNS, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Normalize gender
  data$gender <- trimws(as.character(data$gender))
  data$gender <- factor(data$gender, levels = VALID_GENDER_LEVELS)
  
  # Normalize smoking history
  data$smoking_history <- trimws(as.character(data$smoking_history))
  data$smoking_history <- gsub("_", " ", data$smoking_history)
  data$smoking_history <- factor(data$smoking_history, levels = VALID_SMOKING_LEVELS)
  
  # Convert numeric columns
  data$age <- suppressWarnings(as.numeric(data$age))
  data$hypertension <- suppressWarnings(as.numeric(data$hypertension))
  data$heart_disease <- suppressWarnings(as.numeric(data$heart_disease))
  data$bmi <- suppressWarnings(as.numeric(data$bmi))
  data$HbA1c_level <- suppressWarnings(as.numeric(data$HbA1c_level))
  data$blood_glucose_level <- suppressWarnings(as.numeric(data$blood_glucose_level))
  
  # Convert target variable if present
  if ("diabetes" %in% names(data)) {
    target_clean <- trimws(as.character(data$diabetes))
    if (all(target_clean %in% c("0", "1", "No", "Yes", "FALSE", "TRUE", "false", "true", ""))) {
      target_num <- ifelse(target_clean %in% c("1", "Yes", "TRUE", "true"), "Yes", "No")
      data$diabetes <- factor(target_num, levels = c("No", "Yes"))
    }
  }
  
  # Remove rows with NA in required features
  data <- na.omit(data)
  
  return(data)
}


# ============================================================
# 2. Comprehensive Dataset Validation
# ============================================================

validate_diabetes_data <- function(data) {
  # Audit structure
  audit <- list(
    valid = TRUE,
    total_rows = nrow(data),
    total_cols = ncol(data),
    columns_present = names(data),
    required_cols_present = TRUE,
    missing_columns = character(0),
    missing_value_counts = list(),
    total_missing_cells = 0,
    rows_with_missing = 0,
    duplicate_rows = 0,
    invalid_rows_count = 0,
    valid_rows_count = 0,
    validation_messages = list(),
    invalid_row_details = list()
  )
  
  if (is.null(data) || nrow(data) == 0) {
    audit$valid <- FALSE
    audit$validation_messages <- c(audit$validation_messages, "The uploaded dataset is empty.")
    return(audit)
  }
  
  # Trim column names
  names(data) <- trimws(names(data))
  audit$columns_present <- names(data)
  
  # Check required columns
  missing_cols <- setdiff(REQUIRED_COLUMNS, names(data))
  if (length(missing_cols) > 0) {
    audit$valid <- FALSE
    audit$required_cols_present <- FALSE
    audit$missing_columns <- missing_cols
    audit$validation_messages <- c(
      audit$validation_messages,
      paste("Missing required column(s):", paste(missing_cols, collapse = ", "))
    )
    return(audit)
  }
  
  # Missing values per column
  missing_per_col <- sapply(data[, REQUIRED_COLUMNS, drop = FALSE], function(col) {
    sum(is.na(col) | trimws(as.character(col)) == "" | trimws(as.character(col)) == "NA")
  })
  audit$missing_value_counts <- as.list(missing_per_col)
  audit$total_missing_cells <- sum(missing_per_col)
  
  # Rows with any missing value in required columns
  has_missing_row <- apply(data[, REQUIRED_COLUMNS, drop = FALSE], 1, function(row) {
    any(is.na(row) | trimws(as.character(row)) == "" | trimws(as.character(row)) == "NA")
  })
  audit$rows_with_missing <- sum(has_missing_row)
  
  # Check duplicates across the 8 input features
  audit$duplicate_rows <- sum(duplicated(data[, REQUIRED_COLUMNS, drop = FALSE]))
  
  # Row-by-row validation of types and clinical ranges
  invalid_flag <- rep(FALSE, nrow(data))
  invalid_reasons <- vector("list", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    reasons <- character(0)
    
    # 1. Gender check
    g <- trimws(as.character(data$gender[i]))
    if (is.na(g) || !tolower(g) %in% tolower(VALID_GENDER_LEVELS)) {
      reasons <- c(reasons, paste0("Invalid gender '", g, "'. Allowed: Female, Male, Other."))
    }
    
    # 2. Age check (0 < age <= 120)
    age_val <- suppressWarnings(as.numeric(data$age[i]))
    if (is.na(age_val) || age_val < 0.1 || age_val > 120) {
      reasons <- c(reasons, paste0("Invalid age '", data$age[i], "'. Must be numeric between 0.1 and 120."))
    }
    
    # 3. Hypertension check (0, 1, or Yes/No)
    hyp_raw <- tolower(trimws(as.character(data$hypertension[i])))
    if (is.na(hyp_raw) || !hyp_raw %in% c("0", "1", "yes", "no", "true", "false")) {
      reasons <- c(reasons, paste0("Invalid hypertension '", data$hypertension[i], "'. Must be 0/1 or Yes/No."))
    }
    
    # 4. Heart Disease check (0, 1, or Yes/No)
    hd_raw <- tolower(trimws(as.character(data$heart_disease[i])))
    if (is.na(hd_raw) || !hd_raw %in% c("0", "1", "yes", "no", "true", "false")) {
      reasons <- c(reasons, paste0("Invalid heart_disease '", data$heart_disease[i], "'. Must be 0/1 or Yes/No."))
    }
    
    # 5. Smoking history check
    smk_raw <- gsub("_", " ", trimws(as.character(data$smoking_history[i])))
    if (is.na(smk_raw) || !tolower(smk_raw) %in% tolower(VALID_SMOKING_LEVELS)) {
      reasons <- c(reasons, paste0("Invalid smoking_history '", smk_raw, "'. Allowed: ", paste(VALID_SMOKING_LEVELS, collapse = ", ")))
    }
    
    # 6. BMI check (5 <= BMI <= 100)
    bmi_val <- suppressWarnings(as.numeric(data$bmi[i]))
    if (is.na(bmi_val) || bmi_val < 5 || bmi_val > 100) {
      reasons <- c(reasons, paste0("Invalid BMI '", data$bmi[i], "'. Must be numeric between 5 and 100."))
    }
    
    # 7. HbA1c check (2.5 <= HbA1c <= 20)
    hba1c_val <- suppressWarnings(as.numeric(data$HbA1c_level[i]))
    if (is.na(hba1c_val) || hba1c_val < 2.5 || hba1c_val > 20) {
      reasons <- c(reasons, paste0("Invalid HbA1c '", data$HbA1c_level[i], "'. Must be numeric between 2.5 and 20."))
    }
    
    # 8. Blood glucose check (30 <= glucose <= 600)
    glu_val <- suppressWarnings(as.numeric(data$blood_glucose_level[i]))
    if (is.na(glu_val) || glu_val < 30 || glu_val > 600) {
      reasons <- c(reasons, paste0("Invalid blood glucose '", data$blood_glucose_level[i], "'. Must be numeric between 30 and 600."))
    }
    
    if (length(reasons) > 0) {
      invalid_flag[i] <- TRUE
      invalid_reasons[[i]] <- reasons
    }
  }
  
  audit$invalid_rows_count <- sum(invalid_flag)
  audit$valid_rows_count <- nrow(data) - sum(invalid_flag)
  
  # Store up to first 25 invalid row details for inspection
  invalid_indices <- which(invalid_flag)
  if (length(invalid_indices) > 0) {
    sample_indices <- head(invalid_indices, 25)
    audit$invalid_row_details <- lapply(sample_indices, function(idx) {
      list(
        row_index = idx,
        issues = invalid_reasons[[idx]]
      )
    })
    audit$validation_messages <- c(
      audit$validation_messages,
      paste0("Found ", audit$invalid_rows_count, " record(s) containing invalid or out-of-range values.")
    )
  }
  
  if (audit$rows_with_missing > 0) {
    audit$validation_messages <- c(
      audit$validation_messages,
      paste0("Found ", audit$rows_with_missing, " row(s) with missing required values.")
    )
  }
  
  if (audit$duplicate_rows > 0) {
    audit$validation_messages <- c(
      audit$validation_messages,
      paste0("Detected ", audit$duplicate_rows, " duplicate record(s) across input features.")
    )
  }
  
  if (audit$invalid_rows_count == 0 && audit$rows_with_missing == 0) {
    audit$validation_messages <- c(
      audit$validation_messages,
      "All patient records passed clinical validation checks successfully."
    )
  }
  
  # Add indicator whether target variable exists
  audit$has_target_column <- "diabetes" %in% names(data)
  
  return(audit)
}


# ============================================================
# 3. Load and Clean Dataset
# ============================================================

load_diabetes_data <- function(file_path = "data/diabetes.csv") {
  if (!file.exists(file_path)) {
    stop(paste("File does not exist:", file_path))
  }
  
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data <- clean_diabetes_data(data)
  return(data)
}
