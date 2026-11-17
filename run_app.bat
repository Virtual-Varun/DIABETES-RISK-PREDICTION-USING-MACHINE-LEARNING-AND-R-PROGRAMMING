@echo off
title Diabetes Risk Prediction & Analytics System
echo ============================================================
echo Starting Diabetes Risk Prediction & Analytics System...
echo ============================================================

cd /d "%~dp0"
if exist "DiabetesRiskPrediction" (
    cd "DiabetesRiskPrediction"
)

"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" app.R
pause
