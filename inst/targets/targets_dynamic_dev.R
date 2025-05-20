# Dynamic target naming function
create_dynamic_grid_target <- function(
  grid, 
  dates = NULL, 
  calculation_func, 
  covariate_name = NULL,
  additional_params = list()
) {
  # Generate a unique identifier for the grid
  generate_target_name <- function(grid, covariate_name = NULL, index = 1) {
    # Create a base name with grid information
    base_name <- paste0(
      "pred_calc_", 
      ifelse(!is.null(covariate_name), paste0(covariate_name, "_"), ""),
      sprintf("%03d", index)
    )
    return(base_name)
  }
  
  # Create a wrapper function for dynamic processing
  process_grid_data <- function(grid, dates = NULL) {
    # Prepare arguments for the calculation function
    args <- list(
      locs = grid,
      locs_id = "site_id"  # Adjust as needed
    )
    
    # Add dates if provided
    if (!is.null(dates)) {
      args$date <- dates
    }
    
    # Merge with additional parameters
    args <- c(args, additional_params)
    
    # Call the calculation function with prepared arguments
    tryCatch({
      result <- do.call(calculation_func, args)
      
      # Add metadata to the result if possible
      if (is.data.frame(result) || inherits(result, "sf")) {
        result <- result %>%
          mutate(
            grid_id = grid$grid_id %||% row_number(),
            grid_unique_identifier = generate_target_name(
              grid, 
              covariate_name, 
              grid$grid_id %||% row_number()
            )
          )
      }
      
      return(result)
    }, error = function(e) {
      warning(paste("Error processing grid:", 
                    generate_target_name(grid, covariate_name), 
                    "Error:", e$message))
      return(NULL)
    })
  }
  
  # Generate list of named targets
  lapply(seq_len(nrow(grid)), function(i) {
    tar_target_raw(
      name = as.symbol(generate_target_name(grid, covariate_name, i)),
      command = quote(process_grid_data(grid[i,], dates)),
      format = "parquet"
    )
  })
}



# Dynamic target naming and processing function for grid-based calculations
create_dynamic_grid_target_old <- function(
  grid, 
  dates = NULL, 
  calculation_func, 
  covariate_name = NULL,
  additional_params = list()
) {
  # Generate a unique identifier for the grid
  generate_target_name <- function(grid, covariate_name = NULL) {
    # Create a base name with grid information
    base_name <- paste0(
      "pred_calc_", 
      ifelse(!is.null(covariate_name), paste0(covariate_name, "_"), ""),
      sprintf("%03d", grid$grid_id %||% row_number())
    )
    return(base_name)
  }
  
  # Create a wrapper function for dynamic processing
  process_grid_data <- function(grid, dates = NULL) {
    # Prepare arguments for the calculation function
    args <- list(
      locs = grid,
      locs_id = "site_id"  # Adjust as needed
    )
    
    # Add dates if provided
    if (!is.null(dates)) {
      args$date <- dates
    }
    
    # Merge with additional parameters
    args <- c(args, additional_params)
    
    # Call the calculation function with prepared arguments
    tryCatch({
      result <- do.call(calculation_func, args)
      
      # Add metadata to the result if possible
      if (is.data.frame(result) || inherits(result, "sf")) {
        result <- result %>%
          mutate(
            grid_id = grid$grid_id %||% row_number(),
            grid_unique_identifier = generate_target_name(grid, covariate_name)
          )
      }
      
      return(result)
    }, error = function(e) {
      warning(paste("Error processing grid:", 
                    generate_target_name(grid, covariate_name), 
                    "Error:", e$message))
      return(NULL)
    })
    
  }
}

create_dynamic_target_simple <- function(points, expr, grid_polygon, grid_id = "grid_id", dataset_name, variable_name) {
  # Create a function that generates a dynamic target name
  generate_target_name <- function(grid_polygon, dataset_name = dataset_name, variable_name = variable_name) {
    # Use unique identifier or create a custom naming scheme
    paste0(grid_polygon[[site_id]], 
           "_", dataset_name, "_", variable_name)
  }
  
#   # Process points for a specific grid polygon
#   process_grid_data <- function(points, grid_polygon) {
#     # Filter points within the specific grid polygon
#     filtered_points <- points[st_within(points, grid_polygon, sparse = FALSE), ]
    
#     # If no points in this grid, return NULL
#     if (nrow(filtered_points) == 0) {
#       return(NULL)
#     }
    
#     # Perform spatial analysis on the filtered points
#     processed_data <-
#       filtered_points %>%
#       dplyr::mutate(
#         # Example calculations with grid metadata
#         normalized_value = (value - mean(value)) / sd(value),
#         grid_unique_id = grid_polygon$unique_identifier,
#         grid_region = grid_polygon$region
#       ) %>%
#       dplyr::group_by(grid_unique_id, grid_region) %>%
#       dplyr::summarise(
#         point_count = dplyr::n(),
#         mean_value = mean(value),
#         sd_value = sd(value),
#         total_area = dplyr::first(grid_polygon$grid_area),
#         centroid_x = dplyr::first(grid_polygon$centroid_x),
#         centroid_y = dplyr::first(grid_polygon$centroid_y),
#         .groups = "drop"
#       )
    
#     return(processed_data)
#   }
  
  # Create the target with a dynamic name
  tar_target_raw(
    name = as.symbol(generate_target_name(grid_polygon, dataset_name = dataset_name, variable_name = variable_name)),
    command = quote(expr),
    format = "parquet"
  )
}

# Collect and combine processed grid results

# Collect and combine results with error handling
collect_grid_results <- function(...) {
  # Convert list of lists to a flat list of results
  result_list <- unlist(list(...), recursive = FALSE)
  
  # Filter out NULL results
  result_list <- result_list[!sapply(result_list, is.null)]
  
  # Combine all non-null results
  if (length(result_list) > 0) {
    # Use rbind or a custom combination method depending on result type
    combined_data <- do.call(rbind, result_list)
    return(combined_data)
  } else {
    return(NULL)
  }
}