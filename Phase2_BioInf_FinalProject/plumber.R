library(plumber)

#* @apiTitle Heart Failure Mortality Prediction API
#* @apiDescription This API provides a logistic regression prediction of the probability of death in patients with heart failure, using clinical variables from the Heart Failure Clinical Records dataset.


#load model
model <- readRDS("model/model_logistic_DEATH_EVENT.rds")

#* Health check endpoint
#* @tag System
#* @get /health
#* @serializer json
function() {
  list(status = "ok")
}

#* Predict death probability
#* @param age Age in years (0-120)
#* @param ejection_fraction Ejection fraction in % (0-100)
#* @param serum_creatinine Serum creatinine in mg/dL (0-20)
#* @param serum_sodium Serum sodium in mEq/L (90-200)
#* @post /predict
#* @serializer json
function(age, ejection_fraction, serum_creatinine, serum_sodium, res){
  
  # Convert inputs to numeric
  age <- as.numeric(age)
  ejection_fraction <- as.numeric(ejection_fraction)
  serum_creatinine <- as.numeric(serum_creatinine)
  serum_sodium <- as.numeric(serum_sodium)
  
  # Validate inputs
  if (is.na(age) || age < 0 || age > 120) {
    res$status <- 400
    return(list(error = "age must be between 0 and 120"))
  }
  if (is.na(ejection_fraction) || ejection_fraction < 0 || ejection_fraction > 100) {
    res$status <- 400
    return(list(error = "ejection_fraction must be between 0 and 100"))
  }
  if (is.na(serum_creatinine) || serum_creatinine <= 0 || serum_creatinine > 20) {
    res$status <- 400
    return(list(error = "serum_creatinine must be > 0 and <= 20"))
  }
  if (is.na(serum_sodium) || serum_sodium < 90 || serum_sodium > 200) {
    res$status <- 400
    return(list(error = "serum_sodium must be between 90 and 200"))
  }
  
  # Build dataframe for prediction
  newdata <- data.frame(
    age = age,
    ejection_fraction = ejection_fraction,
    serum_creatinine = serum_creatinine,
    serum_sodium = serum_sodium
  )
  
  #predict probability
  p <- predict(model, newdata = newdata, type = "response")
  
  list(probability = as.numeric(p))
}
