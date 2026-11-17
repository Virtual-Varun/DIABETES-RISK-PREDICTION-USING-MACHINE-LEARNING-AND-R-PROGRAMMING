# Diabetes Risk Prediction Using Machine Learning and R Programming

An interactive clinical analytics and diabetes risk analysis platform powered by **R**, **Plumber**, and **Random Forest Machine Learning**.

---

## Overview

This system provides automated clinical decision support and exploratory data analysis for diabetes risk stratification. Built upon an ensemble Random Forest model trained on 100,000 patient records, it delivers single-patient clinical risk scoring, high-throughput batch dataset predictions, exploratory data analysis with **ggplot2**, configurable probability threshold tuning, and exportable medical reports.

> **Disclaimer:** All prediction scores and risk classifications are algorithmic machine learning estimates for educational and clinical decision-support purposes only. They do not constitute a confirmed medical diagnosis.

---

## Key Features

- **Clinical Analytics Dashboard**:
  - Real-time patient volume, diabetes positive rate, and mean clinical biomarkers (Age, BMI, HbA1c, Fasting Blood Glucose).
  - Dynamic distribution charts for outcomes, gender representation, age stratifications, and clinical BMI categories.
  - Biomarker risk evaluation against standard clinical diagnostic thresholds (HbA1c $\ge$ 6.5%, Blood Glucose $\ge$ 140 mg/dL).

- **Dataset Upload & Validation**:
  - Drag-and-drop CSV file ingestion and one-click sample loader (1,000 verified study patients).
  - Multi-tier data quality validation: column verification, missing value detection, duplicate checks, and numerical range boundary audits.
  - Zero target leakage: guarantees the target variable is never fed into prediction routines.

- **Exploratory Data Analysis (EDA)**:
  - 8 publication-grade **ggplot2** visualizations generated directly in R.
  - Interactive multi-parameter cohort filtering (Gender, Age Bracket, Smoking History, Hypertension, Heart Disease, Diabetes Status).
  - Live filtered cohort metrics strip with dynamic re-generation of ggplot2 plots.

- **Random Forest Prediction Engine**:
  - **Single Patient Assessment**: Diagnostic input form with BMI classification, clinical biomarker alerts, and visual probability gauge.
  - **Batch Predictions**: High-throughput processing (~1,000 patients in 0.13s) with structured, searchable, filterable, and paginated data tables.

- **Risk Stratification**:
  - Calibratable probability threshold sliders (Low Risk < 40%, Moderate Risk 40–70%, High Risk $\ge$ 70%).
  - Real-time cohort reclassification upon threshold changes.
  - Dedicated High-Risk Priority Cohort table with specific elevated risk factors flagged.

- **Model Performance Evaluation**:
  - Genuine test results evaluated on 20,000 held-out test patients:
    - **Accuracy:** 97.30%
    - **Precision:** 99.66%
    - **Recall (Sensitivity):** 68.41%
    - **Specificity:** 99.98%
    - **F1-Score:** 81.13%
    - **ROC-AUC:** 0.9610
  - Confusion matrix display (TN: 18,296, FP: 4, FN: 537, TP: 1,163).
  - Dynamic test evaluation workflow against newly uploaded datasets with ground-truth outcomes.

- **Feature Importance Analysis**:
  - Ranked feature influence based on Mean Decrease in Accuracy and Mean Decrease in Gini.
  - Explanatory notes distinguishing statistical tree reliance from biological causation.

- **Reports & Downloads**:
  - CSV exports for complete predictions, high-risk cohorts, and model performance metrics.
  - Formatted print-to-PDF clinical executive summary reports.

---

## Dataset Schema

The system uses 8 clinical covariates to predict the binary diabetes status:

| # | Feature | Type | Description / Units |
|---|---|---|---|
| 1 | `gender` | Categorical | Female, Male, Other |
| 2 | `age` | Numeric | Patient age in years |
| 3 | `hypertension` | Binary | 0 = Normal blood pressure, 1 = Diagnosed hypertension |
| 4 | `heart_disease` | Binary | 0 = No history, 1 = Diagnosed heart disease |
| 5 | `smoking_history` | Categorical | never, current, former, ever, not current, No Info |
| 6 | `bmi` | Numeric | Body Mass Index ($\text{kg/m}^2$) |
| 7 | `HbA1c_level` | Numeric | Glycated hemoglobin percentage (%) |
| 8 | `blood_glucose_level` | Numeric | Fasting blood glucose concentration (mg/dL) |
| **Target** | `diabetes` | Binary | 0 = No Diabetes, 1 = Diabetes |

---

## Project Structure

```text
DiabetesRiskPrediction/
├── run_app.bat                      # One-click Windows application launcher
├── app.R                            # Application forwarder
├── README.md                        # Documentation
├── .gitignore                       # Git ignore rules
│
└── DiabetesRiskPrediction/          # Core System Directory
    ├── app.R                        # Plumber application server & static asset router
    ├── DiabetesRiskPrediction.Rproj # RStudio Project file
    ├── api/
    │   ├── plumber.R                # REST API endpoints
    │   └── api.R                    # API forwarder
    ├── data/
    │   └── diabetes.csv             # 100,000 patient dataset
    ├── model/
    │   ├── diabetes_model.rds       # Trained 500-tree Random Forest classifier
    │   ├── test_data.rds            # 20,000 held-out test records
    │   └── logistic_model.rds       # Logistic regression baseline
    ├── output/                      # Generated ggplot2 charts and evaluation metrics
    ├── R/
    │   ├── Preprocessing.R          # Data cleaning and audit functions
    │   ├── Predict.R                # Single & batch prediction routines
    │   ├── evaluate_model.R         # Evaluation metrics & dynamic test scoring
    │   ├── eda.R                    # 8 ggplot2 clinical visualization functions
    │   └── train_model.R            # Model training script
    └── web/                         # Frontend Dashboard
        ├── index.html               # Medical dashboard UI
        ├── style.css                # Clinical white & light-blue theme
        └── script.js                # Frontend state controller
```

---

## Getting Started

### Prerequisites

Ensure [R](https://cloud.r-project.org/) ($\ge$ 4.0) is installed. Install required packages in R:

```r
install.packages(c("plumber", "randomForest", "ggplot2", "caret", "pROC", "jsonlite", "scales"))
```

### Running the Application

#### Option 1: 1-Click Launch (Windows)
Double-click `run_app.bat` in the repository root.

#### Option 2: RStudio
1. Open `DiabetesRiskPrediction/DiabetesRiskPrediction.Rproj` in RStudio.
2. Run `source("app.R")`.
3. Navigate to `http://127.0.0.1:8000` in your web browser.

#### Option 3: Terminal / Command Prompt
```bash
cd DiabetesRiskPrediction
Rscript app.R
```
Open your browser at `http://127.0.0.1:8000`.

---

## License

This project is licensed under the MIT License.
