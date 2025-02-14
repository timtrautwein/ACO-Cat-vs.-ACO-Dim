#' ACO Wrapper with default parameters
#'
#' A wrapper for Ant Colony Optimization with predefined default parameters.
#'
#' @param data_input The input dataset.
#' @param list_of_items A list of items to sample from.
#' @param items_per_factor The number of items per factor.
#' @param factors The factors to include in the model.
#' @param iterations The number of max iterations across which to run the optimization.
#' @param num_ants The number of ants per iteration.
#' @param evap_rate The pheromone evaporation rate.
#' @param summary_all Path to the summary output file for all iterations.
#' @param summary_final Path to the summary output file for the final solution.
#' @param fit_func The fit function used to evaluate solutions.
#' @param plot_pheromone Whether to plot pheromone values during iterations.
#' @param cluster_mode Whether to run optimization in cluster mode.
#' @param seed The random seed for reproducibility.
#' @param nCores Number of cores for parallel execution.
#' @param parallel Whether to run code in parallel.
#' @param pheromone_type Sets the weights of the individual optimization criteria in the fit function.
#'
#' @return The final solution after optimization.
#' @export
# Load the antcolony function
source("antcolony.R")

ACO.short <- function(data_input,
                      list_of_items,
                      items_per_factor = 15,
                      factors = c("f1"),
                      iterations = 90,
                      num_ants = 120,
                      evap_rate = 0.99,
                      summary_all,
                      summary_final,
                      fit_func = fit_function,
                      plot_pheromone = TRUE,
                      cluster_mode = FALSE,
                      seed = 1,
                      nCores = parallel::detectCores() - 2,
                      parallel = FALSE,
                      pheromone_type = "balanced",
                      plot_list = list(xlim = c(0, iterations * 2),
                                       ylim = c(0, 1), ylab = "Pheromone Value",
                                       xlab = "Iteration", jitter_width = 0.5, alpha = 0.8,
                                       size = 1))
{
  
  antcolony(
    data_input = data_input,
    evap_rate = evap_rate,
    list_of_items = list_of_items,
    items_per_factor = items_per_factor,
    iterations = iterations,
    num_ants = num_ants,
    factors = factors,
    summary_all = summary_all,
    summary_final = summary_final,
    fit_func = fit_func,
    parallel = parallel,
    nCores = nCores,
    seed = seed,
    plot_pheromone = plot_pheromone,
    cluster_mode = cluster_mode,
    pheromone_type = pheromone_type,
    plot_list = plot_list
  )
}
