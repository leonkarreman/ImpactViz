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



impact_regression <- function(outcomes, treatment_var, dataset, fixed_effect_var = NULL, cluster_var = NULL,
                              control_vars = NULL,
                              level = 5 ) {
  # Initialize a data frame to store results
  results_df <- data.frame(Coefficient = numeric(), 
                           LowerCI = numeric(), 
                           UpperCI = numeric(), 
                           Outcome = character(),
                           Treatment = character())
  
  # Loop over all outcomes
  for(var in outcomes) {
    # Create base formula for lm
    base_formula <- paste(var, "~", treatment_var)
    
    # Fit the base model using lm
    base_model <- lm(as.formula(base_formula), data = dataset)
    
 
    # Create the formula for feols, potentially including fixed effects
    fe_formula <- base_formula
    if (!is.null(fixed_effect_var)) {
      fe_formula <- paste(fe_formula,  control_vars, "|", fixed_effect_var)
    }
    
    # Fit the model using feols, accounting for fixed effects and clustering if specified
    if (is.null(cluster_var)) {
      fe_model <- feols(as.formula(fe_formula), data = dataset)
    } else {
      fe_model <- feols(as.formula(fe_formula), data = dataset, cluster = cluster_var)
    }
    
    print(summary(fe_model))
    # Extract coefficients for the base model
    base_coefs <- coef(base_model)

    
    # Extract coefficients for the fixed effect model
    fe_coefs_all <- coef(fe_model)
    indices <- grep(treatment_var, names(fe_coefs_all))
    fe_coefs <- fe_coefs_all[indices]
    n_coefs <- length(fe_coefs) 
    
    
    # Extract standard errors
    std_errors <- summary(fe_model, robust = TRUE)$se
    
    
    
    # Critical value from the t-distribution for a 95% CI
    # qt() gets the quantile function of the t distribution
    # 0.975 for a 95% CI (two-tailed)
    
    ci_level = 1 - (level/200)
    
    critical_value <- qnorm(ci_level)
    
    # Calculate the confidence intervals
    lower_ci <- fe_coefs - critical_value * std_errors
    upper_ci <- fe_coefs + critical_value * std_errors
    
    comparison_coef <- base_coefs[1]
    
    results_df <- rbind(results_df, 
                        data.frame(Coefficient = comparison_coef,
                                   LowerCI =    NA, 
                                   UpperCI =    NA,
                                   Outcome = var,
                                   Treatment = 0))
    
    
    
    for (coef in 1: (n_coefs)) {
      
      
      results_df <- rbind(results_df, 
                          data.frame(Coefficient =  fe_coefs[coef] + comparison_coef,
                                     LowerCI     =  lower_ci[coef]    + comparison_coef, 
                                     UpperCI     =  upper_ci[coef]    + comparison_coef,
                                     Outcome     = var,
                                     Treatment   = coef))
    }
  }
  
  rownames(results_df) <- NULL
  # Return the results data frame
  return(results_df)
}





