# ============================================================================
# ALZHEIMER'S PREDICTION API - R Plumber
# ============================================================================
# This file creates a REST API that physicians can use to obtain
# Alzheimer's disease predictions based on DNA methylation data
#
# INITIAL SETUP:
# 1. Run your main analysis script first to train the model
# 2. The analysis saves: alzheimers_model.rds and selected_cpgs.rds
# 3. Start the API with: plumber::pr_run(plumber::pr("api.R"), port=8000)
# ============================================================================

library(plumber)
library(jsonlite)
library(dplyr)

# ============================================================================
# LOAD PRE-TRAINED MODEL AND DATA
# ============================================================================
# The main analysis script saves the model as "alzheimers_model.rds"
# We need to prepare the API-compatible version with all necessary data

# Load the model and selected CpGs
model <- readRDS("alzheimers_model.rds")
top_cpgs <- readRDS("selected_cpgs.rds")

# Extract model performance metrics from the model's training data
# Note: The original analysis uses train_data and test_data
# We'll need to recreate these metrics or save them separately

# Function to convert beta values to M-values (from the analysis)
beta_to_m <- function(beta) {
  beta[beta <= 0] <- 0.001
  beta[beta >= 1] <- 0.999
  m_value <- log2(beta / (1 - beta))
  return(m_value)
}


# For now, load the API-ready data (after running the save code above)
tryCatch({
  model_data <- readRDS("alzheimers_model_api.rds")
  
  model <- model_data$model
  top_cpgs <- model_data$top_cpgs
  interpretation <- model_data$interpretation
  beta_to_m <- model_data$beta_to_m
  
  # Performance metrics
  model_performance <- list(
    train_accuracy = model_data$metrics$train_accuracy,
    test_accuracy = model_data$metrics$test_accuracy,
    train_sensitivity = model_data$metrics$train_sensitivity,
    test_sensitivity = model_data$metrics$test_sensitivity,
    train_specificity = model_data$metrics$train_specificity,
    test_specificity = model_data$metrics$test_specificity,
    train_ppv = model_data$metrics$train_ppv,
    test_ppv = model_data$metrics$test_ppv,
    train_npv = model_data$metrics$train_npv,
    test_npv = model_data$metrics$test_npv,
    train_confusion = model_data$metrics$train_confusion,
    test_confusion = model_data$metrics$test_confusion
  )
  
  cat("✓ Loaded complete API data with performance metrics\n")
  
}, error = function(e) {
  # Fallback: Load basic model only
  cat("⚠ Warning: alzheimers_model_api.rds not found\n")
  cat("Loading basic model only. Some endpoints may not work.\n")
  cat("Please run the save code from the analysis script.\n\n")
  
  model <- readRDS("alzheimers_model.rds")
  top_cpgs <- readRDS("selected_cpgs.rds")
  
  # Create dummy metrics (will need to be replaced)
  model_performance <- list(
    test_accuracy = NA,
    test_sensitivity = NA,
    test_specificity = NA,
    test_ppv = NA,
    test_npv = NA
  )
  
  # Create dummy interpretation table
  interpretation <- data.frame(
    Variable = character(0),
    Odds_Ratio = numeric(0),
    P_value = numeric(0)
  )
})

#* @apiTitle Alzheimer's Disease Prediction API
#* @apiDescription API for predicting Alzheimer's disease risk based on DNA methylation data from the GSE66351 analysis
#* @apiVersion 1.0.0

# ============================================================================
# ENDPOINT 1: Health Check
# ============================================================================
#* Check if the API is active and functioning
#* @get /health
#* @response 200 API is functioning properly
#* @serializer unboxedJSON
function() {
  list(
    status = "ok",
    message = "Alzheimer's API is running",
    timestamp = Sys.time(),
    model_loaded = !is.null(model),
    n_cpgs = length(top_cpgs),
    model_class = class(model)[1],
    api_ready = exists("model_performance") && !is.na(model_performance$test_accuracy)
  )
}

# ============================================================================
# ENDPOINT 2: Model Information
# ============================================================================
#* Get detailed information about the model and its performance
#* @get /model-info
#* @response 200 Model information and performance metrics
#* @serializer unboxedJSON
function() {
  list(
    model_type = "Logistic Regression (Binary Classification)",
    outcome = "AD_status (0 = Control, 1 = Alzheimer's Disease)",
    n_predictors = length(top_cpgs) + 4,  # CpGs + age + sex + cell_type + region
    n_cpg_sites = length(top_cpgs),
    dataset = "GSE66351 - Illumina HumanMethylation450 BeadChip",
    
    performance = if (!is.na(model_performance$test_accuracy)) {
      list(
        test_accuracy = round(model_performance$test_accuracy * 100, 2),
        test_sensitivity = round(model_performance$test_sensitivity * 100, 2),
        test_specificity = round(model_performance$test_specificity * 100, 2),
        test_ppv = round(model_performance$test_ppv * 100, 2),
        test_npv = round(model_performance$test_npv * 100, 2),
        
        interpretation = list(
          accuracy = paste0("Model is correct in ", 
                            round(model_performance$test_accuracy * 100, 2), 
                            "% of test cases"),
          sensitivity = paste0("When patient HAS AD, model detects it in ", 
                               round(model_performance$test_sensitivity * 100, 2), 
                               "% of cases"),
          specificity = paste0("When patient DOES NOT have AD, model identifies it in ", 
                               round(model_performance$test_specificity * 100, 2), 
                               "% of cases"),
          ppv = paste0("When model predicts AD, it's correct ", 
                       round(model_performance$test_ppv * 100, 2), 
                       "% of the time"),
          npv = paste0("When model predicts Control, it's correct ", 
                       round(model_performance$test_npv * 100, 2), 
                       "% of the time")
        ),
        
        test_confusion_matrix = lapply(as.data.frame(model_performance$test_confusion), as.list)
        
      )
    } else {
      "Performance metrics not available - please reload API data"
    },
    
    required_inputs = list(
      M_values = paste0("Array of ", length(top_cpgs), " M-values for selected CpG sites"),
      age = "numeric (patient age in years)",
      sex = "factor ('M' or 'F')",
      cell_type = "factor (e.g., 'neuron', 'glia', 'bulk')",
      region = "factor (brain region sampled)"
    ),
    
    cpg_sites = top_cpgs,
    
    interpretation_guide = list(
      "0-30% probability" = "Low AD risk",
      "30-70% probability" = "Moderate risk - additional testing recommended",
      "70-100% probability" = "High AD risk - comprehensive evaluation needed"
    ),
    
    note = "Model uses M-values (log2 methylation ratios), not Beta values"
  )
}

# ============================================================================
# ENDPOINT 3: CpG Site Importance
# ============================================================================
#* Get the most important CpG sites for prediction
#* @get /cpg-importance
#* @response 200 List of most important CpG sites with their effects
#* @serializer unboxedJSON
function() {
  if (nrow(interpretation) == 0) {
    return(list(
      error = "Interpretation data not loaded",
      message = "Please reload API with complete model data"
    ))
  }
  
  # Extract methylation markers (M_1, M_2, etc.)
  m_coefs <- interpretation[grepl("^M_", interpretation$Variable), ]
  
  if (nrow(m_coefs) > 0) {
    # Map M_1, M_2, etc. to actual CpG IDs
    m_coefs$Index <- as.numeric(gsub("M_", "", m_coefs$Variable))
    m_coefs$CpG_Site <- top_cpgs[m_coefs$Index]
    
    # Calculate importance (absolute log odds ratio)
    m_coefs$Importance <- abs(log(m_coefs$Odds_Ratio))
    m_coefs <- m_coefs[order(-m_coefs$Importance), ]
    
    # Interpretation
    m_coefs$Effect_Direction <- ifelse(
      m_coefs$Odds_Ratio > 1,
      "Hypermethylation increases AD risk",
      "Hypomethylation increases AD risk"
    )
    
    m_coefs$Significance <- ifelse(
      m_coefs$P_value < 0.001, "Highly significant (p < 0.001)",
      ifelse(m_coefs$P_value < 0.01, "Very significant (p < 0.01)",
             ifelse(m_coefs$P_value < 0.05, "Significant (p < 0.05)",
                    "Not significant (p >= 0.05)"))
    )
    
    list(
      n_cpg_sites = nrow(m_coefs),
      
      cpg_importance = m_coefs[, c("CpG_Site", "Variable", "Odds_Ratio", 
                                   "P_value", "Importance", "Effect_Direction", 
                                   "Significance")],
      
      interpretation = list(
        message = "These CpG sites show significant methylation differences between AD and controls",
        odds_ratio_meaning = "OR > 1: Higher methylation increases AD risk; OR < 1: Higher methylation decreases AD risk",
        importance_metric = "Absolute value of log(Odds Ratio) - higher = stronger effect",
        clinical_note = "These markers were selected using Bonferroni correction after covariate-controlled linear regression"
      ),
      
      top_5_cpgs = head(m_coefs[, c("CpG_Site", "Odds_Ratio", "Effect_Direction")], 5)
    )
  } else {
    list(
      error = "No CpG coefficients found in interpretation table"
    )
  }
}

# ============================================================================
# ENDPOINT 4: Single Patient Prediction
# ============================================================================
#* Predict Alzheimer's risk for a single patient
#* 
#* **IMPORTANT: Input must use M-values, not Beta values**
#* M-values = log2(Beta / (1 - Beta))
#* 
#* **JSON Format Example:**
#* ```json
#* {
#*   "M_values": [0.5, -1.2, 2.3, 0.8, -0.5, 1.1, ...],
#*   "age": 75,
#*   "sex": "M",
#*   "cell_type": "neuron",
#*   "region": "frontal cortex"
#* }
#* ```
#* 
#* **Field Descriptions:**
#* - `M_values` (array of numbers, REQUIRED): M-values for the selected CpG sites
#*   Must contain exactly as many values as model requires
#*   M-values typically range from -5 to +5
#* - `age` (number, REQUIRED): Patient age in years
#* - `sex` (string, REQUIRED): "M" or "F"
#* - `cell_type` (string, REQUIRED): Cell type (e.g., "neuron", "glia", "bulk")
#* - `region` (string, REQUIRED): Brain region (e.g., "frontal cortex", "temporal cortex")
#*
#* @parser json
#* @post /predict
#* @response 200 Prediction with probability and clinical interpretation
#* @response 400 Missing or invalid data
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    # Parse JSON body
    body <- req$body
    
    # Validate M_values
    if (is.null(body$M_values) || length(body$M_values) != length(top_cpgs)) {
      res$status <- 400
      return(list(
        error = "Invalid M_values",
        details = paste0("Exactly ", length(top_cpgs), " M-values required"),
        required_cpg_sites = top_cpgs,
        note = "Use M-values (not Beta values). M = log2(Beta / (1-Beta))",
        example = list(
          M_values = rep(0, length(top_cpgs)),
          message = "Replace zeros with actual M-values from methylation analysis"
        )
      ))
    }
    
    # Validate clinical covariates
    if (is.null(body$age) || is.null(body$sex) || 
        is.null(body$cell_type) || is.null(body$region)) {
      res$status <- 400
      return(list(
        error = "Missing required clinical data",
        required_fields = list(
          age = "numeric (e.g., 75)",
          sex = "string - 'M' or 'F'",
          cell_type = "string (e.g., 'neuron', 'glia', 'bulk')",
          region = "string (e.g., 'frontal cortex', 'temporal cortex')"
        )
      ))
    }
    
    # Get factor levels from model
    sex_levels <- levels(model$model$sex)
    cell_type_levels <- levels(model$model$cell_type)
    region_levels <- levels(model$model$region)
    
    # Create prediction dataframe
    new_data <- data.frame(
      age = as.numeric(body$age),
      sex = factor(body$sex, levels = sex_levels),
      cell_type = factor(body$cell_type, levels = cell_type_levels),
      region = factor(body$region, levels = region_levels)
    )
    
    # Add M-values (named M_1, M_2, M_3, etc.)
    for (i in 1:length(top_cpgs)) {
      new_data[[paste0("M_", i)]] <- as.numeric(body$M_values[i])
    }
    
    # Check for factor level mismatches
    if (any(is.na(new_data$sex)) || any(is.na(new_data$cell_type)) || 
        any(is.na(new_data$region))) {
      res$status <- 400
      return(list(
        error = "Invalid factor levels",
        valid_levels = list(
          sex = sex_levels,
          cell_type = cell_type_levels,
          region = region_levels
        ),
        provided = list(
          sex = body$sex,
          cell_type = body$cell_type,
          region = body$region
        )
      ))
    }
    
    # Make prediction
    pred_prob <- predict(model, newdata = new_data, type = "response")
    pred_prob <- as.numeric(pred_prob)
    
    pred_class <- ifelse(pred_prob > 0.5, "Alzheimer's Disease", "Control")
    
    # Risk stratification
    risk_level <- if (pred_prob < 0.3) {
      "LOW"
    } else if (pred_prob < 0.7) {
      "MODERATE"
    } else {
      "HIGH"
    }
    
    # Clinical recommendations
    clinical_recommendation <- if (pred_prob < 0.3) {
      "Low AD risk. Standard age-appropriate follow-up recommended."
    } else if (pred_prob < 0.7) {
      "Moderate AD risk. Consider additional diagnostic workup: neuropsychological testing, MRI imaging, CSF biomarkers, or amyloid PET scan."
    } else {
      "High AD risk. Comprehensive neurological evaluation strongly recommended. Consider referral to memory clinic and discussion of preventive interventions."
    }
    
    # Get top contributing factors if interpretation exists
    top_factors <- NULL
    if (nrow(interpretation) > 0) {
      factors <- interpretation[, c("Variable", "Odds_Ratio", "P_value")]
      factors$Abs_Log_OR <- abs(log(factors$Odds_Ratio))
      factors <- factors[order(-factors$Abs_Log_OR), ]
      top_factors <- head(factors[, c("Variable", "Odds_Ratio", "P_value")], 5)
    }
    
    # Return prediction
    result <- list(
      prediction = list(
        probability_AD = round(pred_prob, 4),
        probability_percent = paste0(round(pred_prob * 100, 1), "%"),
        predicted_class = pred_class,
        risk_level = risk_level,
        confidence = if (!is.na(model_performance$test_accuracy)) {
          paste0("Based on model with ", 
                 round(model_performance$test_accuracy * 100, 1), 
                 "% test accuracy")
        } else {
          "Model accuracy not available"
        }
      ),
      
      clinical_interpretation = list(
        recommendation = clinical_recommendation,
        risk_interpretation = paste0(
          "Predicted probability of Alzheimer's Disease: ",
          round(pred_prob * 100, 1), "%"
        )
      ),
      
      input_data = list(
        age = body$age,
        sex = body$sex,
        cell_type = body$cell_type,
        region = body$region,
        n_cpg_sites = length(body$M_values)
      )
    )
    
    if (!is.null(top_factors)) {
      result$top_contributing_factors <- top_factors
    }
    
    result$disclaimer <- paste(
      "IMPORTANT MEDICAL DISCLAIMER:",
      "This prediction is a research tool for clinical decision support only.",
      "It is NOT a diagnostic test and does not replace clinical judgment.",
      "All predictions must be interpreted by qualified healthcare professionals",
      "in the context of complete clinical, cognitive, and imaging assessments.",
      "This model was trained on post-mortem brain tissue samples and",
      "its applicability to living patients requires further validation."
    )
    
    return(result)
    
  }, error = function(e) {
    res$status <- 500
    return(list(
      error = "Prediction failed",
      details = as.character(e),
      message = "Internal server error during prediction"
    ))
  })
}


# ============================================================================
# ENDPOINT 5: Dataset Statistics
# ============================================================================
#* Get statistics about the training dataset (GSE66351)
#* @get /dataset-stats
#* @response 200 Training dataset statistics
#* @serializer unboxedJSON
function() {
  list(
    dataset_info = list(
      name = "GSE66351",
      source = "Gene Expression Omnibus (GEO)",
      platform = "Illumina HumanMethylation450 BeadChip",
      description = "Methylomic profiling of Alzheimer's disease in human brain tissue",
      citation = "Lunnon et al. (2014) Nature Neuroscience",
      total_samples = "~190 post-mortem brain samples",
      total_cpg_tested = "485,577 CpG sites",
      selected_cpg = length(top_cpgs)
    ),
    
    feature_selection = list(
      method = "Covariate-controlled linear regression",
      outcome = "Braak stage (AD progression: 0-6)",
      covariates = c("age", "sex", "cell_type", "brain_region"),
      correction = "Bonferroni (family-wise error rate control)",
      threshold = "p < 0.05 after Bonferroni correction",
      rationale = "Conservative approach for clinical application"
    ),
    
    model_details = list(
      type = "Logistic Regression",
      outcome_variable = "AD_status (binary: 0=Control, 1=AD)",
      predictors = paste0(length(top_cpgs), " CpG M-values + 4 clinical covariates"),
      validation = "70/30 train-test split",
      m_values_used = TRUE,
      m_value_definition = "M = log2(Beta / (1-Beta))"
    ),
    
    selected_cpg_sites = top_cpgs,
    
    note = paste(
      "This model was trained on post-mortem brain tissue.",
      "Validation in living patients or blood/CSF samples is required",
      "before clinical deployment."
    )
  )
}

# ============================================================================
# ENDPOINT 6: Beta to M-value Conversion Helper
# ============================================================================
#* Convert Beta values to M-values
#* 
#* **JSON Format:**
#* ```json
#* {
#*   "beta_values": [0.5, 0.75, 0.25, 0.9, 0.1]
#* }
#* ```
#*
#* @post /convert-beta-to-m
#* @parser json
#* @response 200 Converted M-values
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- req$body
    
    if (is.null(body$beta_values)) {
      res$status <- 400
      return(list(
        error = "Missing beta_values",
        required = "Array of beta values (0 to 1)",
        example = list(beta_values = c(0.5, 0.75, 0.25))
      ))
    }
    
    beta_values <- as.numeric(body$beta_values)
    
    # Validate range
    if (any(beta_values < 0 | beta_values > 1, na.rm = TRUE)) {
      res$status <- 400
      return(list(
        error = "Beta values must be between 0 and 1",
        invalid_values = beta_values[beta_values < 0 | beta_values > 1]
      ))
    }
    
    # Convert using the function from the analysis
    m_values <- beta_to_m(beta_values)
    
    list(
      n_values = length(beta_values),
      beta_values = beta_values,
      m_values = round(m_values, 4),
      formula = "M = log2(Beta / (1 - Beta))",
      note = "Small offset applied to avoid log(0): Beta <= 0 → 0.001, Beta >= 1 → 0.999"
    )
    
  }, error = function(e) {
    res$status <- 500
    list(
      error = "Conversion failed",
      details = as.character(e)
    )
  })
}

# ============================================================================
# ENDPOINT 7: API Documentation (Root)
# ============================================================================
#* @get /
#* @serializer unboxedJSON
function(req) {
  port <- req$SERVER_PORT
  if (is.null(port) || port == "") {
    port <- API_PORT  # Fallback alla porta di default
  }
  
  list(
    api_name = "Alzheimer's Prediction API",
    version = "1.0.0",
    description = "API for predicting Alzheimer's disease risk based on DNA methylation",
    
    available_endpoints = list(
      list(
        method = "GET",
        endpoint = "/health",
        description = "Check API status"
      ),
      list(
        method = "GET",
        endpoint = "/model-info",
        description = "Detailed model information and performance"
      ),
      list(
        method = "GET",
        endpoint = "/cpg-importance",
        description = "Most important CpG sites for prediction"
      ),
      list(
        method = "POST",
        endpoint = "/predict",
        description = "Single patient prediction",
        required_fields = c("M_values", "age", "sex", "cell_type", "region")
      ),
      list(
        method = "GET",
        endpoint = "/dataset-stats",
        description = "Training dataset statistics"
      )
    ),
    
    example_usage = list(
      note = "See individual endpoint documentation for JSON format examples",
      current_port = port,
      curl_example = paste0(
        'curl -X POST http://localhost:', port, '/predict \\\n',
        '  -H "Content-Type: application/json" \\\n',
        '  -d \'{"M_values": [', paste(rep('0.5', min(5, length(top_cpgs))), collapse=', '), 
        '], "age": 75, "sex": "M", "cell_type": "neuron", "region": "frontal cortex"}\''
      )
    ),
    
    
    documentation = "Visit /model-info for model details",
    contact = "For support, contact the bioinformatics team"
  )
}