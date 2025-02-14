#' Ant Colony Optimization (ACO) function
#'
#' This function performs Ant Colony Optimization (ACO) to select optimal items based on pheromone values.
#'
#' @param data_input The input dataset.
#' @param evap_rate The pheromone evaporation rate.
#' @param list_of_items A list of items to be considered in the optimization.
#' @param items_per_factor The number of items to select for each factor.
#' @param iterations The number of max iterations.
#' @param num_ants The number of ants (solutions evaluated in parallel) per iteration.
#' @param factors The factors to include in the model.
#' @param summary_all Path to the summary output file for all iterations.
#' @param summary_final Path to the summary output file for the final solution.
#' @param fit_func The fit function used to evaluate solutions.
#' @param parallel Whether to run code in parallel.
#' @param nCores Number of cores to use in parallel processing.
#' @param seed Random seed for reproducibility.
#' @param plot_pheromone Whether to plot pheromone values across iterations.
#' @param cluster_mode Whether to run code in cluster mode for parallel processing.
#' @param pheromone_type The type of pheromone calculation/weighting of optimization criteria to use ("validity", "balanced", or "reliability").
#' @param plot_list List of plot parameters.
#'
#' @return Writes the results to summary files and returns the final optimized solution.
#' @export
#' 
library(parallel)
library(data.table)
library(ggplot2)

antcolony <- function(data_input, evap_rate, list_of_items, items_per_factor, iterations, num_ants, factors, summary_all, summary_final, fit_func, parallel = FALSE, nCores = 1, seed = 1, plot_pheromone = FALSE, cluster_mode = FALSE, pheromone_type = "balanced", plot_list = list(xlim = c(0, iterations * 2), ylim = c(0, 1), ylab = "Pheromone Value", xlab = "Iteration", jitter_width = 0.5, alpha = 0.8, size = 1)) {
  
  best_pheromone <- 0
  best_so_far_pheromone <- 0
  
  # Store the list of items in a single vector
  item_vector <- unlist(list_of_items, use.names = FALSE)
  include <- rep(1, length(item_vector))
  
  best_so_far_solution <- include
  count <- 1
  run <- 1
  cache <- list()
  
  # Initialize an empty data.table to store all solutions
  all_solutions <- data.table()
  
  # Set up parallel processing
  if (parallel) {
    # Determine the cluster type based on the operating system
    if (.Platform$OS.type == "windows") {
      cluster_type <- "PSOCK"
    } else {
      cluster_type <- "FORK"
    }
    
    # Create the cluster
    if (cluster_mode) {
      myCL <- makeCluster(nCores, type = cluster_type)
    } else {
      myCL <- makeCluster(nCores, type = cluster_type, outfile = "cluster_log.txt")
    }
    
    # Set up the cluster environment
    clusterSetRNGStream(cl = myCL, iseed = seed)
    clusterExport(cl = myCL, varlist = ls(envir = environment()), envir = environment())
    clusterEvalQ(cl = myCL, expr = {
      library(psych)
      library(lavaan)
      library(semTools)
      library(MASS)
      library(polycor)
      library(tidyverse)
      library(brant)
    })
  } else {
    set.seed(seed)
  }
  
  # Define headers for the summary file
  title_fit_info <- c(
    item_vector,
    "run",
    "count",
    "ant",
    "pheromone_value",
    "phi_CFI",
    "phi_RMSEA",
    "phi_omega",
    "phi_polycor",
    "brant_value",  # 0 or 1
    "CFI_val",
    "RMSEA_val",
    "omega2",
    "polychoric_corr",
    "brant_min_pvalue",
    paste0("phi_", item_vector)  # Adds phi_Item columns for included variables
  )
  
  # Write the header with append = FALSE
  write.table(t(title_fit_info), file = summary_all, append = FALSE, quote = FALSE, sep = ";", row.names = FALSE, col.names = FALSE)
  
  # Main loop for iterations
  while (count <= iterations) {
    ant <- 0
    selected_vector_list <- vector(mode = "list", num_ants)
    select_indicator_list <- vector(mode = "list", num_ants)
    
    # Loop through each ant in the colony
    while (ant < num_ants) {
      ant <- ant + 1
      selected_items <- list_of_items
      
      # Select items for each factor
      for (fac in 1:length(list_of_items)) {
        positions <- is.element(item_vector, list_of_items[[fac]])
        prob <- include[positions] / sum(include[positions])
        items <- sample(list_of_items[[fac]], size = items_per_factor[fac], replace = FALSE, prob = prob)
        selected_items[[fac]] <- items
      }
      
      selected_vector <- unlist(selected_items, use.names = FALSE)
      selected_vector_list[[ant]] <- selected_vector
      select_indicator <- is.element(item_vector, selected_vector)
      select_indicator_list[[ant]] <- select_indicator
    }
    
    # Evaluate the solutions (in parallel or sequential mode)
    if (parallel) {
      new_solutions <- parLapply(myCL, 1:num_ants, function(ant) {
        key <- paste(sort(selected_vector_list[[ant]]), collapse = "-")
        if (key %in% names(cache)) {
          return(cache[[key]])
        } else {
          fit_info <- fit_func(selected_vector_list[[ant]], data_input, pheromone_type)
          if (!is.null(fit_info)) {
            cache[[key]] <- fit_info
          }
          return(fit_info)
        }
      })
    } else {
      new_solutions <- lapply(1:num_ants, function(ant) {
        key <- paste(sort(selected_vector_list[[ant]]), collapse = "-")
        if (key %in% names(cache)) {
          return(cache[[key]])
        } else {
          fit_info <- fit_func(selected_vector_list[[ant]], data_input, pheromone_type)
          if (!is.null(fit_info)) {
            cache[[key]] <- fit_info
          }
          return(fit_info)
        }
      })
    }
    
    # Process each ant's solution
    ant <- 0
    while (ant < num_ants) {
      ant <- ant + 1
      fit_info <- new_solutions[[ant]]
      if (!is.null(fit_info)) {
        select_indicator <- select_indicator_list[[ant]]
        pheromone <- fit_info$pheromone
        
        # Create and save the results for this ant in a separate row
        result_row <- c(
          select_indicator,
          run,
          count,
          ant,
          pheromone,
          fit_info$phi_CFI,
          fit_info$phi_RMSEA,
          fit_info$phi_omega,
          fit_info$phi_polycor,
          fit_info$brant_value,  # 0 or 1
          fit_info$CFI,
          fit_info$RMSEA,
          fit_info$omega2,
          fit_info$polychoric_corr,
          fit_info$brant_min_pvalue,  
          round(include, digits = 4)  
        )
        write.table(t(result_row), file = summary_all, append = TRUE, quote = FALSE, sep = ";", row.names = FALSE, col.names = FALSE)
        
        # Update the best solution
        if (pheromone >= best_pheromone) {
          best_solution <- select_indicator
          best_pheromone <- pheromone
          best_CFI <- fit_info$CFI
          best_RMSEA <- fit_info$RMSEA
          best_Omega <- fit_info$omega2
          best_polycor <- fit_info$polychoric_corr
          best_brant_min_pvalue <- fit_info$brant_min_pvalue
        }
      }
    }
    
    # Update pheromone levels
    include <- include * evap_rate
    include <- include + best_solution * best_pheromone
    
    # Check for the best solution overall
    if (best_pheromone > best_so_far_pheromone) {
      best_so_far_solution <- best_solution
      best_so_far_pheromone <- best_pheromone
      best_so_far_CFI <- best_CFI
      best_so_far_RMSEA <- best_RMSEA
      best_so_far_Omega <- best_Omega
      best_so_far_polycor <- best_polycor
      best_so_far_brant_min_pvalue <- best_brant_min_pvalue
      count <- 1
    } else {
      count <- count + 1
    }
    
    # Optionally plot pheromone values
    if (plot_pheromone) {
      new_solutions2 <- data.table(pheromone = unlist(lapply(new_solutions, function(x) x$pheromone)))
      new_solutions2$iter <- run  # Use 'run' instead of 'count'
      
      # Aggregate all results
      all_solutions <- rbind(all_solutions, new_solutions2)
      
      suppressWarnings({
        conv_plot <- ggplot() + 
          geom_jitter(data = all_solutions, aes(x = iter, y = pheromone), color = "cornflowerblue", 
                      width = plot_list$jitter_width, alpha = plot_list$alpha, size = plot_list$size) + 
          scale_x_continuous(limits = c(0, max(all_solutions$iter))) +  # Dynamically adjust x-axis limits
          scale_y_continuous(limits = plot_list$ylim) +
          labs(x = plot_list$xlab, y = plot_list$ylab) +
          theme_minimal()
        print(conv_plot)
      })
    }
    
    run <- run + 1
  }
  
  if (parallel) {
    stopCluster(myCL)
  }
  
  # Save the final solution
  title_final_solution <- c(
    "Items",
    "pheromone",
    "Best_CFI",
    "Best_RMSEA",
    "best_Omega",
    "best_polychoric_corr",
    "best_brant_min_pvalue",
    item_vector
  )
  
  # Write the header with append = FALSE
  write.table(t(title_final_solution), file = summary_final, append = FALSE, quote = FALSE, sep = ";", row.names = FALSE, col.names = FALSE)
  
  final_solution <- c(
    sum(items_per_factor),
    best_so_far_pheromone,
    best_so_far_CFI,
    best_so_far_RMSEA,
    best_so_far_Omega,
    best_so_far_polycor,
    best_so_far_brant_min_pvalue,
    best_so_far_solution
  )
  
  # Write the data with append = TRUE
  write.table(t(final_solution), file = summary_final, append = TRUE, quote = FALSE, sep = ";", row.names = FALSE, col.names = FALSE)
}
