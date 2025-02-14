library(lavaan)
library(tidyverse)
library(MASS)
library(semTools)
library(ggplot2)
library(parallel)
library(data.table)
library(brant)
library(polycor)

# Adjusted function to perform model fitting with regression and Brant test
fit_function <- function(selected_vector, data_input, pheromone_type = "balanced") {
  tryCatch({
    # Regression with the selected items
    # Define the formula for the regression with the selected items
    formula_items <- as.formula(paste("Level_reduced ~", paste(selected_vector, collapse = " + ")))
    
    # Fit the ordinal logistic regression with the items as predictors
    polr_mod <- polr(formula_items, data = data_input, method = "logistic")
    
    # Initialize variables
    brant_min_pvalue <- NA
    brant_value <- NA  # This will be 0 or 1 based on the test result
    phi_polycor <- NA
    phi_CFI <- NA
    phi_RMSEA <- NA
    phi_omega <- NA
    CFI <- NA
    RMSEA <- NA
    omega2 <- NA
    polychoric_corr <- NA
    pheromone <- NA
    
    # Determine if Brant test (= test of proportional odds assumption) should be performed
    perform_brant <- pheromone_type %in% c("balanced", "validity")
    
    if (perform_brant) {
      # Perform the Brant test
      try({
        brant_test <- brant(polr_mod)
        # Extract p-values for individual predictors (excluding Omnibus)
        p_values <- brant_test[,"probability"]
        p_values <- p_values[names(p_values) != "Omnibus"]
        
        # Find the minimal p-value among all items
        min_p_value <- min(p_values, na.rm = TRUE)
        brant_min_pvalue <- min_p_value  # Store for output
        
        if (!is.na(min_p_value)) {
          # If minimal p-value < 0.05, set brant_value = 0; else 1
          if (min_p_value < 0.05) {
            brant_value <- 0
          } else {
            brant_value <- 1
          }
        } else {
          brant_value <- NA
        }
      }, silent = TRUE)
    }
    
    # Proceed based on pheromone_type
    if (pheromone_type == "reliability") {
      # Always compute CFA and pheromone value regardless of Brant test
      # Estimate the CFA model based on the selected vector of items
      mod_cfa <- paste('L =~', paste(selected_vector, collapse = " + "))
      
      # Fit the CFA model
      fit <- tryCatch({
        cfa(model = mod_cfa,
            data = data_input,
            ordered = TRUE,
            missing = "pairwise",
            std.lv = TRUE,
            se = "none",
            parameterization = "theta")
      }, error = function(e) return(NULL))
      
      if (!is.null(fit)) {
        # Calculate reliability (omega)
        omega <- semTools::reliability(fit)
        omega2 <- as.numeric(omega["omega2", "L"])
        
        # Calculate CFI and RMSEA
        CFI <- fitMeasures(fit, fit.measures = "CFI.scaled")
        RMSEA <- fitMeasures(fit, fit.measures = "RMSEA.scaled")
        
        # Calculate phi values
        phi_CFI <- 1 / (1 + exp(75 * (.98 - CFI)))
        phi_RMSEA <- 1 - 1 / (1 + exp(75 * (.02 - RMSEA)))
        phi_omega <- 1 / (1 + exp(10 * (.8 - omega2)))
        
        # Calculate pheromone with normalization
        pheromone <- ((phi_CFI + phi_RMSEA)/2 + 3 * phi_omega) / 4
        
        # Predict the language categories from the ordinal logistic regression model
        predicted <- predict(polr_mod, type = "class")
        
        # Ensure that actual categories and predicted categories are factors with the same levels
        actual_groups <- data_input$Level_reduced
        predicted_groups <- factor(predicted, levels = levels(actual_groups), ordered = TRUE)
        
        suppressWarnings({
          polychoric_corr <- polychor(actual_groups, predicted_groups)
        })
        
        # Calculate phi_polycor
        phi_polycor <- 1 / (1 + exp(30 * (.9 - polychoric_corr)))
      }
      
    } else {
      # For "balanced" and "validity" functions
      if (perform_brant && !is.na(brant_value) && brant_value == 1) {
        # Brant test passes, proceed to compute CFA and other fit measures
        
        # Define the CFA model based on the selected vector of items
        mod_cfa <- paste('L =~', paste(selected_vector, collapse = " + "))
        
        # Fit the CFA model
        fit <- tryCatch({
          cfa(model = mod_cfa,
              data = data_input,
              ordered = TRUE,
              missing = "pairwise",
              std.lv = TRUE,
              se = "none",
              parameterization = "theta")
        }, error = function(e) return(NULL))
        
        if (!is.null(fit)) {
          # Calculate reliability (omega)
          omega <- semTools::reliability(fit)
          omega2 <- as.numeric(omega["omega2", "L"])
          
          # Calculate CFI and RMSEA
          CFI <- fitMeasures(fit, fit.measures = "CFI.scaled")
          RMSEA <- fitMeasures(fit, fit.measures = "RMSEA.scaled")
          
          # Calculate phi values
          phi_CFI <- 1 / (1 + exp(75 * (.98 - CFI)))
          phi_RMSEA <- 1 - 1 / (1 + exp(75 * (.02 - RMSEA)))
          phi_omega <- 1 / (1 + exp(10 * (.8 - omega2)))
          
          # Predict the language categories from the proportional odds model
          predicted <- predict(polr_mod, type = "class")
          
          # Ensure both variables are factors with the same levels
          actual_groups <- data_input$Level_reduced
          predicted_groups <- factor(predicted, levels = levels(actual_groups), ordered = TRUE)
          
          suppressWarnings({
            polychoric_corr <- polychor(actual_groups, predicted_groups)
          })
          
          # Calculate phi_polycor
          phi_polycor <- 1 / (1 + exp(30 * (.9 - polychoric_corr)))
          
          # Calculate pheromone value based on pheromone_type
          if (pheromone_type == "validity") {
            pheromone <- phi_polycor
            # Normalize pheromone value by dividing by 1 (no change needed)
            pheromone <- pheromone / 1
          } else if (pheromone_type == "balanced") {
            pheromone <- ((phi_CFI + phi_RMSEA)/2 + 2 * phi_omega + 2 * phi_polycor) / 5
          }
        }
        
      } else {
        # Brant test failed or not applicable, set pheromone to 0 and other measures to NA
        pheromone <- 0
        phi_CFI <- NA
        phi_RMSEA <- NA
        phi_omega <- NA
        CFI <- NA
        RMSEA <- NA
        omega2 <- NA
        polychoric_corr <- NA
        if (perform_brant) {
          # Only include Brant test results if Brant was performed
          # Otherwise, these remain NA
          # These have already been initialized as NA
        }
      }
    }
    
    # Store and return all calculated metrics in a list
    fit_info <- list(
      select_indicator = selected_vector,
      pheromone = pheromone,
      phi_CFI = phi_CFI,
      phi_RMSEA = phi_RMSEA,
      phi_omega = phi_omega,
      phi_polycor = phi_polycor,
      brant_value = if (perform_brant) brant_value else NA,  # 0, 1, or NA
      CFI = CFI,
      RMSEA = RMSEA,
      omega2 = omega2,
      polychoric_corr = polychoric_corr,
      brant_min_pvalue = if (perform_brant) brant_min_pvalue else NA
    )
    
    return(fit_info)
  }, warning = function(w) return(NULL), error = function(e) return(NULL))
}
