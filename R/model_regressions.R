#' Run Regression Models with Fixed Effects and Clustered Standard Errors
#'
#' This function uses the `fixest` package to fit linear models for a given set of outcomes
#' with an option to include fixed effects and calculate clustered standard errors.
#' @importFrom fixest feols
#' @param outcomes Vector of outcome variable names.
#' @param treatment_var Name of the treatment variable.
#' @param dataset The data frame containing the data.
#' @param fixed_effect_var Name of the fixed effect variable, or NULL if not used.
#' @param cluster_var Name of the clustering variable, or NULL if not used.
#' @return A data frame with regression results.
#' @export
#' @examples
#' \dontrun{
#'   results <- run_regression_models_fixest(depvars, treatment_var, dataset, fixed_effect_var, cluster_var)
#'   print(results)
#' }



impact_regression <- function(outcomes, treatment_var, dataset, fixed_effect_var = NULL, cluster_var = NULL) {
  # Initialize a data frame to store coefficients and confidence intervals
  results_df <- data.frame(Treatment0 = numeric(), 
                           Treatment1 = numeric(), 
                           TreatmentLowerCI = numeric(), 
                           TreatmentUpperCI = numeric(), 
                           Outcome = character())
  
  # Loop over all outcomes
  for(var in outcomes) {
    # Create base formula
    formula <- paste(var, "~", treatment_var)
    
    
    base_model  <- lm(formula, data = dataset)
      
      # Extract the intercept and treatment coefficient
      intercept <- coef(base_model)["(Intercept)"]
    treatment <- coef(base_model)[treatment_var]
    
    
    
    
    # If there is a fixed effect variable, add it to the formula
    if (!is.null(fixed_effect_var)) {
      formula <- paste(formula, "| ", fixed_effect_var)
    }
    
    # Fit the model using feols (from fixest package), accounting for fixed effects and clustering if specified
    if (is.null(cluster_var)) {
      fe_model <- feols(as.formula(formula), data = dataset)
    } else {
      fe_model <- feols(as.formula(formula), data = dataset, cluster = cluster_var)
    }
    

    
    # Compute confidence interval for treatment coefficient, taking into account clustering if specified
    treatment_ci <- confint(fe_model, parm = treatment_var)
    
    # Store in results data frame
    results_df <- rbind(results_df, 
                        data.frame(Treatment0 = intercept,
                                   Treatment1 = treatment,
                                   TreatmentLowerCI = as.numeric(treatment_ci)[1], 
                                   TreatmentUpperCI = as.numeric(treatment_ci)[2],
                                   Outcome = var))
  }
  
  # Return the results data frame
  return(results_df)
}





