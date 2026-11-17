# ============================================================
# plumber.R
# Diabetes Risk Prediction - Complete Plumber REST API
# ============================================================

library(plumber)
library(jsonlite)

# Determine working directory context and load modules
if (file.exists("../R/Predict.R")) {
  source("../R/Predict.R")
  source("../R/Preprocessing.R")
  source("../R/evaluate_model.R")
  source("../R/eda.R")
} else if (file.exists("R/Predict.R")) {
  source("R/Predict.R")
  source("R/Preprocessing.R")
  source("R/evaluate_model.R")
  source("R/eda.R")
}

# Resolve model path
find_model_path <- function() {
  candidates <- c(
    "../model/diabetes_model.rds",
    "model/diabetes_model.rds",
    file.path(getwd(), "model", "diabetes_model.rds"),
    file.path(dirname(getwd()), "model", "diabetes_model.rds")
  )
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  return("model/diabetes_model.rds")
}


# ============================================================
# CORS FILTER
# ============================================================

#* CORS filter
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS, PUT, DELETE")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, Accept")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return("")
  }
  
  plumber::forward()
}


# ============================================================
# 1. HEALTH CHECK
# ============================================================

#* Check whether API and model are online
#* @get /health
#* @serializer json
function() {
  m_path <- find_model_path()
  model_exists <- file.exists(m_path)
  
  list(
    status = "healthy",
    service = "Diabetes Risk Prediction System",
    model = "Random Forest Classification (500 Trees)",
    model_loaded = model_exists,
    timestamp = Sys.time()
  )
}


# ============================================================
# 2. SINGLE PATIENT PREDICTION (Preserved & Enhanced)
# ============================================================

#* Predict Diabetes Risk for a Single Patient
#* @post /predict
#* @serializer json
function(req, res,
         gender = NULL,
         age = NULL,
         hypertension = NULL,
         heart_disease = NULL,
         smoking_history = NULL,
         bmi = NULL,
         HbA1c_level = NULL,
         blood_glucose_level = NULL) {
  
  # Support JSON body payload if parameters are passed in body
  body_data <- list()
  if (!is.null(req$postBody) && nchar(req$postBody) > 0) {
    try({
      parsed <- jsonlite::fromJSON(req$postBody)
      if (is.list(parsed) && !is.data.frame(parsed)) {
        body_data <- parsed
      }
    }, silent = TRUE)
  }
  
  # Merge form parameters or JSON body
  g <- if (!is.null(gender)) gender else body_data$gender
  a <- if (!is.null(age)) age else body_data$age
  hyp <- if (!is.null(hypertension)) hypertension else body_data$hypertension
  hd <- if (!is.null(heart_disease)) heart_disease else body_data$heart_disease
  smk <- if (!is.null(smoking_history)) smoking_history else body_data$smoking_history
  bm <- if (!is.null(bmi)) bmi else body_data$bmi
  hb <- if (!is.null(HbA1c_level)) HbA1c_level else body_data$HbA1c_level
  glu <- if (!is.null(blood_glucose_level)) blood_glucose_level else body_data$blood_glucose_level
  
  # Check for missing parameters
  if (is.null(g) || is.null(a) || is.null(hyp) || is.null(hd) || 
      is.null(smk) || is.null(bm) || is.null(hb) || is.null(glu)) {
    res$status <- 400
    return(list(
      error = TRUE,
      message = "Missing required patient parameters. Please provide all 8 input features."
    ))
  }
  
  input_df <- data.frame(
    gender = as.character(g),
    age = as.numeric(a),
    hypertension = as.character(hyp),
    heart_disease = as.character(hd),
    smoking_history = as.character(smk),
    bmi = as.numeric(bm),
    HbA1c_level = as.numeric(hb),
    blood_glucose_level = as.numeric(glu),
    stringsAsFactors = FALSE
  )
  
  m_path <- find_model_path()
  result <- tryCatch({
    predict_diabetes_risk(input_df, model_path = m_path)
  }, error = function(e) {
    res$status <- 500
    list(error = TRUE, message = conditionMessage(e))
  })
  
  return(result)
}


# ============================================================
# 3. BATCH DATASET PREDICTION (For Uploaded Datasets)
# ============================================================

#* Batch Predict Diabetes Risk for an Uploaded Dataset
#* @post /batch_predict
#* @serializer json
function(req, res) {
  if (is.null(req$postBody) || nchar(req$postBody) == 0) {
    res$status <- 400
    return(list(error = TRUE, message = "Empty request payload. Please upload patient records."))
  }
  
  # Parse JSON records or CSV
  data <- NULL
  low_th <- 40
  high_th <- 70
  
  tryCatch({
    parsed <- jsonlite::fromJSON(req$postBody)
    if (is.list(parsed) && "records" %in% names(parsed)) {
      data <- as.data.frame(parsed$records)
      if (!is.null(parsed$low_threshold)) low_th <- as.numeric(parsed$low_threshold)
      if (!is.null(parsed$high_threshold)) high_th <- as.numeric(parsed$high_threshold)
    } else if (is.data.frame(parsed)) {
      data <- parsed
    }
  }, error = function(e) {
    # If JSON parsing fails, attempt parsing raw CSV text
    tryCatch({
      data <<- read.csv(text = req$postBody, stringsAsFactors = FALSE)
    }, error = function(e2) {
      res$status <- 400
      return(list(error = TRUE, message = paste("Failed to parse request data:", conditionMessage(e2))))
    })
  })
  
  if (is.null(data) || nrow(data) == 0) {
    res$status <- 400
    return(list(error = TRUE, message = "No valid records found in payload."))
  }
  
  m_path <- find_model_path()
  
  result <- tryCatch({
    predict_diabetes_batch(
      input_data = data,
      model_path = m_path,
      low_threshold = low_th,
      high_threshold = high_th
    )
  }, error = function(e) {
    res$status <- 500
    list(error = TRUE, message = conditionMessage(e))
  })
  
  return(result)
}


# ============================================================
# 4. DATASET VALIDATION ENDPOINT
# ============================================================

#* Validate an Uploaded CSV Dataset
#* @post /validate
#* @serializer json
function(req, res) {
  if (is.null(req$postBody) || nchar(req$postBody) == 0) {
    res$status <- 400
    return(list(error = TRUE, message = "Empty dataset provided."))
  }
  
  data <- NULL
  tryCatch({
    parsed <- jsonlite::fromJSON(req$postBody)
    if (is.list(parsed) && "records" %in% names(parsed)) {
      data <- as.data.frame(parsed$records)
    } else if (is.data.frame(parsed)) {
      data <- parsed
    }
  }, error = function(e) {
    tryCatch({
      data <<- read.csv(text = req$postBody, stringsAsFactors = FALSE)
    }, error = function(e2) {
      res$status <- 400
      return(list(error = TRUE, message = paste("Invalid CSV or JSON format:", conditionMessage(e2))))
    })
  })
  
  if (is.null(data) || nrow(data) == 0) {
    res$status <- 400
    return(list(error = TRUE, message = "Dataset contains no rows."))
  }
  
  audit <- validate_diabetes_data(data)
  return(audit)
}


# ============================================================
# 5. MODEL PERFORMANCE METRICS
# ============================================================

#* Get Genuine Model Evaluation Results
#* @get /model_performance
#* @serializer json
function() {
  get_genuine_model_metrics()
}


#* Evaluate Model on Uploaded Ground Truth Dataset
#* @post /evaluate_dataset
#* @serializer json
function(req, res) {
  if (is.null(req$postBody) || nchar(req$postBody) == 0) {
    res$status <- 400
    return(list(error = TRUE, message = "Empty dataset payload."))
  }
  
  data <- NULL
  tryCatch({
    parsed <- jsonlite::fromJSON(req$postBody)
    data <- if (is.data.frame(parsed)) parsed else if ("records" %in% names(parsed)) as.data.frame(parsed$records) else NULL
  }, error = function(e) {
    tryCatch({
      data <<- read.csv(text = req$postBody, stringsAsFactors = FALSE)
    }, error = function(e2) {
      res$status <- 400
      return(list(error = TRUE, message = conditionMessage(e2)))
    })
  })
  
  if (is.null(data) || !"diabetes" %in% names(data)) {
    res$status <- 400
    return(list(
      error = TRUE,
      message = "Uploaded dataset must contain the target 'diabetes' column to perform evaluation."
    ))
  }
  
  m_path <- find_model_path()
  result <- tryCatch({
    evaluate_dataset(data, model_path = m_path)
  }, error = function(e) {
    res$status <- 500
    list(error = TRUE, message = conditionMessage(e))
  })
  
  return(result)
}


# ============================================================
# 6. FEATURE IMPORTANCE
# ============================================================

#* Get Random Forest Feature Importance
#* @get /feature_importance
#* @serializer json
function() {
  m_path <- find_model_path()
  rf_model <- get_rf_model(m_path)
  
  imp_mat <- randomForest::importance(rf_model)
  imp_df <- as.data.frame(imp_mat)
  imp_df$Feature <- rownames(imp_df)
  rownames(imp_df) <- NULL
  
  # Clinical descriptions of each feature
  descriptions <- list(
    "HbA1c_level" = "Glycated hemoglobin percentage reflecting 3-month average blood sugar.",
    "blood_glucose_level" = "Circulating plasma glucose concentration (mg/dL).",
    "bmi" = "Body Mass Index (kg/m²), indicating adiposity and metabolic burden.",
    "age" = "Chronological age in years; diabetes incidence rises significantly with age.",
    "heart_disease" = "Presence of cardiovascular comorbidity.",
    "hypertension" = "Diagnosed chronic arterial hypertension.",
    "smoking_history" = "Tobacco smoking status and historical exposure level.",
    "gender" = "Biological sex/gender identity."
  )
  
  imp_df$Description <- sapply(imp_df$Feature, function(f) {
    if (!is.null(descriptions[[f]])) descriptions[[f]] else "Clinical patient covariate."
  })
  
  # Sort in descending order of MeanDecreaseAccuracy
  imp_df <- imp_df[order(-imp_df$MeanDecreaseAccuracy), ]
  
  list(
    features = imp_df,
    note = "Feature importance measures tree purity and classification error increase upon variable permutation. It signifies model dependency within this dataset and should not be confused with direct biological or clinical causation."
  )
}


# ============================================================
# 7. SAMPLE DATASET FOR 1-CLICK DEMO / TESTING
# ============================================================

#* Get Sample Patient Records from data/diabetes.csv
#* @get /sample_data
#* @serializer json
function(count = 500) {
  n <- suppressWarnings(as.numeric(count))
  if (is.na(n) || n <= 0) n <- 500
  if (n > 2000) n <- 2000 # cap for swift network transfer
  
  csv_candidates <- c(
    "data/diabetes.csv",
    "../data/diabetes.csv",
    file.path(getwd(), "data", "diabetes.csv"),
    file.path(dirname(getwd()), "data", "diabetes.csv")
  )
  
  csv_file <- NULL
  for (c in csv_candidates) {
    if (file.exists(c)) { csv_file <- c; break }
  }
  
  if (is.null(csv_file)) {
    return(list(error = TRUE, message = "Default dataset data/diabetes.csv not found."))
  }
  
  sample_df <- read.csv(csv_file, nrows = n, stringsAsFactors = FALSE)
  
  list(
    total_samples = nrow(sample_df),
    columns = names(sample_df),
    records = sample_df
  )
}


# ============================================================
# 8. GENERATE EDA PLOTS
# ============================================================

#* Regenerate EDA Visualizations using ggplot2
#* @post /generate_eda
#* @serializer json
function(req, res) {
  data <- NULL
  if (!is.null(req$postBody) && nchar(req$postBody) > 0) {
    try({
      parsed <- jsonlite::fromJSON(req$postBody)
      if (is.data.frame(parsed)) data <- parsed
      else if ("records" %in% names(parsed)) data <- as.data.frame(parsed$records)
    }, silent = TRUE)
  }
  
  # Default to sample dataset if no data provided
  if (is.null(data) || nrow(data) == 0) {
    csv_candidates <- c("data/diabetes.csv", "../data/diabetes.csv")
    for (c in csv_candidates) {
      if (file.exists(c)) {
        data <- read.csv(c, nrows = 5000, stringsAsFactors = FALSE)
        break
      }
    }
  }
  
  if (is.null(data)) {
    res$status <- 400
    return(list(error = TRUE, message = "No dataset available for EDA generation."))
  }
  
  out_dir <- if (dir.exists("output")) "output" else if (dir.exists("../output")) "../output" else "output"
  plots <- generate_all_eda(data, output_dir = out_dir)
  
  return(list(
    status = "success",
    plots = plots,
    timestamp = Sys.time()
  ))
}
