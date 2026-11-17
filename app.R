# ============================================================
# app.R (Root Forwarder)
# Diabetes Risk Prediction System
# ============================================================

# Check if we are at root or subfolder
if (dir.exists("DiabetesRiskPrediction")) {
  setwd("DiabetesRiskPrediction")
}

source("app.R")
