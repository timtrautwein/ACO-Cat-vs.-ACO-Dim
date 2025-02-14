# ACO_EDAF_Cluster_flex.R
# This is a flexible version of ACO.short for use on a cluster.

# Load necessary libraries
library(parallel)
library(lavaan)
library(tidyverse)
library(MASS)
library(semTools)
library(ggplot2)
library(data.table)

# Load functions
source("fit_function.R")
source("ACO.short.R")  # This is the updated ACO.short function

# Arguments passed via batch
args <- commandArgs(trailingOnly = TRUE)
seed <- as.numeric(args[1])
item_count <- as.numeric(args[2])
output_dir <- args[3]
pheromone_type <- args[4]

# Load your data and prepare list_of_items
data_input <- read.csv("edaf_clean_s_97.csv") %>% #replace this dataset with the synthetic one
  dplyr::arrange(Level_reduced)
data_input$Level_reduced <- factor(data_input$Level_reduced, ordered = TRUE)
# Select columns Item4 to Item120 to create the initial item pool
list_of_items <- list(f1 = names(dplyr::select(data_input, Item4:Item120)))

# Check if data_input and list_of_items were loaded
if (!exists("data_input")) {
  stop("The data 'data_input' was not loaded. Please load your data before running the script.")
}

if (!exists("list_of_items")) {
  stop("The list of items 'list_of_items' was not created. Please create it before running the script.")
}

# Create subdirectory for the pheromone type
pheromone_dir <- file.path(output_dir, pheromone_type)
if (!dir.exists(pheromone_dir)) {
  dir.create(pheromone_dir, recursive = TRUE)
}

# Create filenames for the summaries
summary_all <- file.path(pheromone_dir, paste0("ACO_edaf_clean_reduced_", pheromone_type, "_Seed", seed, "_Items", item_count, "_AllIterations.csv"))
summary_final <- file.path(pheromone_dir, paste0("ACO_edaf_clean_reduced_", pheromone_type, "_Seed", seed, "_Items", item_count, "_FinalSolution.csv"))

# Run ACO.short
ACO.short(
  data_input = data_input,
  list_of_items = list_of_items,
  items_per_factor = item_count,  # corresponds to item_count in a unidimensional model
  factors = c("f1"),  # Adjust according to your dimensional structure
  iterations = 90,
  num_ants = 120,
  evap_rate = 0.99,
  summary_all = summary_all,
  summary_final = summary_final,
  fit_func = fit_function,
  plot_pheromone = FALSE,
  cluster_mode = TRUE,
  seed = seed,
  nCores = parallel::detectCores() - 2,
  parallel = TRUE,
  pheromone_type = pheromone_type
)
