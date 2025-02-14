# Ant Colony Optimization (ACO)

The core of the ACO algorithm is the optimization function, which evaluates and weights the various criteria (e.g., model fit, reliability) for each item set considered. The convergence behavior of the optimization is significantly influenced by its hyperparameters: 
* number of ants,
* number of maximum iterations, and
* evaporation rate.

In each iteration, the *number of ants* (i.e., candidate short-scale solutions generated and evaluated) was set to 120, and the maximum number of iterations was set to 90, following previous recommendations (Olaru et al., 2019). After each iteration, the drawing probabilities (i.e., pheromone levels) of the items are updated according to the current best solution. In subsequent iterations, item sets (ants) are selected in a probabilistic process guided by these drawing probabilities. 

The *evaporation rate* controls how quickly the influence of past solutions diminish across iterations, balancing the exploration of new solutions with the exploitation of existing ones (Blum & Roli, 2003). Higher values slow down the evaporation process, encouraging exploitation of known successful paths, whereas lower values equal faster evaporation, encouraging exploration of new paths. In the present case, the evaporation rate was set close to 1, favoring further exploitation of good solutions found in previous iterations. 

The algorithm stops when no better solution is found within the *maximum number of iterations*.

## Perform Ant Colony Optimization (ACO) for item selection

The `ACO.short` function applies an Ant Colony Optimization algorithm to select items based on a prespecified optimization function. 

* data_input:        The input dataset
* list_of_items:     A list of item names, defining the initial item pool
* items_per_factor:  The number of items to select per factor
* factors:           Factors to include in the model (e.g., c('f1', 'f2'))
* iterations:        The number of max iterations across which to run the ACO algorithm.
* num_ants:          The number of ants to evaluate per iteration.
* evap_rate:         The rate at which pheromones evaporate after each iteration.
* summary_all:       File path to save all optimization results.
* summary_final:     File path to save the final best solution.
* plot_pheromone:    TRUE/FALSE to plot pheromone values across iterations.
* cluster_mode:      TRUE/FALSE to suppress status outputs and plots (necessary when running code on a computer cluster).
* parallel:          TRUE/FALSE to enable parallel processing for ants.
* nCores:            Number of CPU cores for parallel processing.
* pheromone_type:    Sets the weights of the individual optimization criteria in the fit function.

## Example

    # Source the ACO.short.R file
    source("ACO.short.R")
    

    # Load the example dataset from the package
    data("EDAF_Synthetic")

    # Prepare the dataset
    dat <- EDAF_Synthetic %>%
    dplyr::arrange(Level_reduced)
    dat$Level_reduced <- factor(dat$Level_reduced, ordered = TRUE)
    # Select columns Item4 to Item120 to create the initial item pool.
    list_items <- list(names(dplyr::select(dat, Item4:Item120)))
    
    # Define the factors of the model (here unidimensional)
    factor <- c("f1")  
    
    # Set the number of items to select per factor
    i_per_f <- c(15)  
    
    # Set the number of iterations
    iter <- 4  

    # Set the number of ants per iteration (can run parallel)
    ants <- 15  

    # Set the pheromone evaporation rate: Adjust the rate at which pheromone levels decrease after each iteration. Higher values mean slower evaporation, encouraging exploitation of known item sets, while lower values mean faster evaporation, encouraging exploration of new item sets.
    evaporation <- 0.99  

    # File name to save all results to a summary file
    summary.all <- "summary_all_test.csv"  
    
    # File name to save the final best solution
    summary.final <- "summary_final_test.csv"

    # Pheromone type sets the weights of the optimization criteria in the fit function (balanced, validity, reliability)
    pheromone_type <- "balanced"
    
    # Call the ACO.short function with the specified parameters
    ACO.short(
      data_input = dat,  
      list_of_items = list_items,  
      items_per_factor = i_per_f,  
      factors = factor,  
      iterations = iter,  
      num_ants = ants,  
      evap_rate = evaporation,  
      summary_all = summary.all,  
      summary_final = summary.final,  
      plot_pheromone = TRUE,  # Plot pheromone values across iterations
      cluster_mode = TRUE,  # Enable output of status and plots
      parallel = TRUE,  # Enable parallel processing
      nCores = parallel::detectCores() - 2,  # Number of CPU cores to use
      pheromone_type = pheromone_type # Sets the weights of the individual optimization criteria in the fit function 
    )

## References
- Blum, C., & Roli, A. (2003). Metaheuristics in combinatorial optimization: Overview and conceptual comparison. *ACM Computing Surveys (CSUR)*, *35*(3), 268–308. https://doi.org/10.1145/937503.937505
- Olaru, G., Schroeders, U., Hartung, J., & Wilhelm, O. (2019). Ant colony optimization and local weighted structural equation modeling. A tutorial on novel item and person sampling procedures for personality research. *European Journal of Personality*, *33*(3), 400–419. https://doi.org/10.1002/per.2195
