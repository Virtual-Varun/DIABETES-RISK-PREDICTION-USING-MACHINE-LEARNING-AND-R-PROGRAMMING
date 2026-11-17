# ============================================================
# eda.R
# Diabetes Risk Prediction - Exploratory Data Analysis with ggplot2
# Professional Medical Visualizations
# ============================================================

library(ggplot2)

# Professional healthcare visual theme
theme_healthcare <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#0f172a", margin = margin(b = 6)),
      plot.subtitle = element_text(size = 10.5, color = "#64748b", margin = margin(b = 10)),
      axis.title = element_text(face = "bold", size = 10.5, color = "#334155"),
      axis.text = element_text(size = 9.5, color = "#475569"),
      panel.grid.major = element_line(color = "#f1f5f9"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 9.5, color = "#334155"),
      legend.text = element_text(size = 9.5, color = "#475569")
    )
}

# Medical palette
DIABETES_PALETTE <- c("No" = "#0ea5e9", "Yes" = "#e11d48", "No Diabetes" = "#0ea5e9", "Diabetes" = "#e11d48", "All Patients" = "#0284c7")


# ============================================================
# Generate All EDA Plots
# ============================================================

generate_all_eda <- function(data, output_dir = "output") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  if (is.null(data) || nrow(data) == 0) {
    return(list(error = "No records provided to generate plots."))
  }
  
  # Standardize target if present
  if ("diabetes" %in% names(data)) {
    raw_d <- trimws(as.character(data$diabetes))
    data$DiabetesStatus <- factor(
      ifelse(raw_d %in% c("1", "Yes", "yes", "true", "TRUE", "Diabetes"), "Diabetes", "No Diabetes"),
      levels = c("No Diabetes", "Diabetes")
    )
  } else {
    data$DiabetesStatus <- factor("All Patients")
  }
  
  saved_plots <- list()
  
  # ----------------------------------------------------------
  # 1. Gender-wise Diabetes Distribution
  # ----------------------------------------------------------
  try({
    p_gender <- ggplot(data, aes(x = gender, fill = DiabetesStatus)) +
      geom_bar(position = "dodge", alpha = 0.9, width = 0.6) +
      scale_fill_manual(values = DIABETES_PALETTE, drop = FALSE) +
      labs(
        title = "Gender-Wise Diabetes Distribution",
        subtitle = paste0("Filtered Cohort (N = ", nrow(data), ")"),
        x = "Gender",
        y = "Patient Count",
        fill = "Status"
      ) +
      theme_healthcare()
    
    f_gender <- file.path(output_dir, "gender_distribution.png")
    ggsave(f_gender, p_gender, width = 7, height = 4.5, dpi = 150)
    saved_plots$gender <- "gender_distribution.png"
  }, silent = TRUE)
  
  # ----------------------------------------------------------
  # 2. Age-wise Diabetes Distribution
  # ----------------------------------------------------------
  try({
    p_age <- ggplot(data, aes(x = age, fill = DiabetesStatus)) +
      geom_histogram(bins = min(30, max(5, nrow(data))), alpha = 0.75, position = "identity") +
      scale_fill_manual(values = DIABETES_PALETTE, drop = FALSE) +
      labs(
        title = "Age Distribution by Diabetes Status",
        subtitle = paste0("Patient frequency across age cohorts (N = ", nrow(data), ")"),
        x = "Age (Years)",
        y = "Patient Count",
        fill = "Status"
      ) +
      theme_healthcare()
    
    f_age <- file.path(output_dir, "age_distribution.png")
    ggsave(f_age, p_age, width = 7, height = 4.5, dpi = 150)
    saved_plots$age <- "age_distribution.png"
  }, silent = TRUE)
  
  # ----------------------------------------------------------
  # 3. Hypertension Analysis
  # ----------------------------------------------------------
  try({
    data$HypertensionLabel <- factor(
      ifelse(data$hypertension %in% c(1, "1", "yes", "Yes", "true", "TRUE"), "Hypertension (Yes)", "Normal BP (No)")
    )
    p_hyp <- ggplot(data, aes(x = HypertensionLabel, fill = DiabetesStatus)) +
      geom_bar(position = if (length(unique(data$DiabetesStatus)) > 1) "fill" else "stack", alpha = 0.9, width = 0.55) +
      scale_fill_manual(values = DIABETES_PALETTE, drop = FALSE) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Hypertension vs Diabetes Risk Proportion",
        subtitle = "Proportion of diabetes within hypertensive vs non-hypertensive patients",
        x = "Hypertension Diagnosis",
        y = "Proportion of Patients",
        fill = "Status"
      ) +
      theme_healthcare()
    
    f_hyp <- file.path(output_dir, "hypertension_analysis.png")
    ggsave(f_hyp, p_hyp, width = 7, height = 4.5, dpi = 150)
    saved_plots$hypertension <- "hypertension_analysis.png"
  }, silent = TRUE)
  
  # ----------------------------------------------------------
  # 4. Heart Disease Analysis
  # ----------------------------------------------------------
  try({
    data$HeartDiseaseLabel <- factor(
      ifelse(data$heart_disease %in% c(1, "1", "yes", "Yes", "true", "TRUE"), "Heart Disease (Yes)", "No Heart Disease (No)")
    )
    p_hd <- ggplot(data, aes(x = HeartDiseaseLabel, fill = DiabetesStatus)) +
      geom_bar(position = if (length(unique(data$DiabetesStatus)) > 1) "fill" else "stack", alpha = 0.9, width = 0.55) +
      scale_fill_manual(values = DIABETES_PALETTE, drop = FALSE) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Heart Disease vs Diabetes Prevalence",
        subtitle = "Proportional diabetes outcome comparison",
        x = "Heart Disease Status",
        y = "Proportion of Patients",
        fill = "Status"
      ) +
      theme_healthcare()
    
    f_hd <- file.path(output_dir, "heart_disease_analysis.png")
    ggsave(f_hd, p_hd, width = 7, height = 4.5, dpi = 150)
    saved_plots$heart_disease <- "heart_disease_analysis.png"
  }, silent = TRUE)
  
  # ----------------------------------------------------------
  # 5. Smoking History Analysis
  # ----------------------------------------------------------
  try({
    p_smk <- ggplot(data, aes(x = smoking_history, fill = DiabetesStatus)) +
      geom_bar(position = "dodge", alpha = 0.85, width = 0.7) +
      scale_fill_manual(values = DIABETES_PALETTE, drop = FALSE) +
      labs(
        title = "Smoking History vs Diabetes Outcome",
        subtitle = "Patient counts across reported tobacco use history",
        x = "Smoking History",
        y = "Patient Count",
        fill = "Status"
      ) +
      theme_healthcare() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
    
    f_smk <- file.path(output_dir, "smoking_analysis.png")
    ggsave(f_smk, p_smk, width = 7, height = 4.5, dpi = 150)
    saved_plots$smoking <- "smoking_analysis.png"
  }, silent = TRUE)
  
  # ----------------------------------------------------------
  # 6. BMI Analysis
  # ----------------------------------------------------------
  try({
    p_bmi <- ggplot(data, aes(x = DiabetesStatus, y = bmi, fill = DiabetesStatus)) +
      geom_boxplot(alpha = 0.8, outlier.size = 0.8, outlier.alpha = 0.3, width = 0.5) +
      scale_fill_manual(values = DIABETES_PALETTE, drop = FALSE) +
      labs(
        title = "Body Mass Index (BMI) Distribution",
        subtitle = "Clinical BMI ranges across diabetes outcome groups",
        x = "Status",
        y = "BMI (kg/m²)",
        fill = "Status"
      ) +
      theme_healthcare() +
      guides(fill = "none")
    
    f_bmi <- file.path(output_dir, "bmi_distribution.png")
    ggsave(f_bmi, p_bmi, width = 7, height = 4.5, dpi = 150)
    saved_plots$bmi <- "bmi_distribution.png"
  }, silent = TRUE)
  
  # ----------------------------------------------------------
  # 7. HbA1c Level Analysis
  # ----------------------------------------------------------
  try({
    p_hba1c <- if (nrow(data) >= 5 && length(unique(data$HbA1c_level)) > 2) {
      ggplot(data, aes(x = HbA1c_level, fill = DiabetesStatus)) +
        geom_density(alpha = 0.5) +
        geom_vline(xintercept = 6.5, linetype = "dashed", color = "#b91c1c", linewidth = 0.8) +
        annotate("text", x = 6.6, y = 0.4, label = "Diagnostic Threshold (6.5%)", color = "#b91c1c", hjust = 0, size = 3) +
        scale_fill_manual(values = DIABETES_PALETTE, drop = FALSE) +
        labs(
          title = "Glycated Hemoglobin (HbA1c) Density Profile",
          subtitle = "Comparison against clinical diagnostic boundary (6.5%)",
          x = "HbA1c Level (%)",
          y = "Density",
          fill = "Status"
        ) +
        theme_healthcare()
    } else {
      ggplot(data, aes(x = HbA1c_level, fill = DiabetesStatus)) +
        geom_histogram(bins = 15, alpha = 0.75) +
        geom_vline(xintercept = 6.5, linetype = "dashed", color = "#b91c1c", linewidth = 0.8) +
        scale_fill_manual(values = DIABETES_PALETTE, drop = FALSE) +
        labs(
          title = "Glycated Hemoglobin (HbA1c) Histogram",
          subtitle = paste0("Filtered Cohort (N = ", nrow(data), ")"),
          x = "HbA1c Level (%)",
          y = "Patient Count",
          fill = "Status"
        ) +
        theme_healthcare()
    }
    
    f_hba1c <- file.path(output_dir, "hba1c_distribution.png")
    ggsave(f_hba1c, p_hba1c, width = 7, height = 4.5, dpi = 150)
    saved_plots$hba1c <- "hba1c_distribution.png"
  }, silent = TRUE)
  
  # ----------------------------------------------------------
  # 8. Blood Glucose Level Analysis
  # ----------------------------------------------------------
  try({
    p_glu <- ggplot(data, aes(x = blood_glucose_level, fill = DiabetesStatus)) +
      geom_histogram(bins = min(30, max(5, nrow(data))), alpha = 0.75, position = "identity") +
      geom_vline(xintercept = 140, linetype = "dashed", color = "#b91c1c", linewidth = 0.8) +
      scale_fill_manual(values = DIABETES_PALETTE, drop = FALSE) +
      labs(
        title = "Blood Glucose Level Distribution",
        subtitle = "Patient concentrations relative to pre-diabetes threshold (140 mg/dL)",
        x = "Blood Glucose (mg/dL)",
        y = "Patient Count",
        fill = "Status"
      ) +
      theme_healthcare()
    
    f_glu <- file.path(output_dir, "glucose_distribution.png")
    ggsave(f_glu, p_glu, width = 7, height = 4.5, dpi = 150)
    saved_plots$glucose <- "glucose_distribution.png"
  }, silent = TRUE)
  
  return(saved_plots)
}
