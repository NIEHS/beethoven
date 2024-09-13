
#' Combine dynamically branched sublists based on common column names
#' @description
#' This function combines the sublists of dynamically branched targets object
#' as a preprocessor for `reduce_merge` function.
#' @keywords prediction
#' @param df_list list(1). List of data frames with common column names.
#' Typically the output of a dynamically branched target.
#' @return a list object, with data frames combined by common column names.
#' @export
reduce_list <- function(df_list) {
  # Create a named list to group data frames by their column names
  grouped_dfs <- list()

  # Iterate over each data frame in the list
  for (df in df_list) {
    # Get the column names of the current data frame
    col_names <- paste(sort(colnames(df)), collapse = ",")

    # Check if this set of column names already exists in the grouped list
    if (!col_names %in% names(grouped_dfs)) {
      grouped_dfs[[col_names]] <- list()
    }

    # Append the current data frame to the list for its column names
    grouped_dfs[[col_names]] <- append(grouped_dfs[[col_names]], list(df))
  }

  # Combine data frames with the same column names
  combined_list <- lapply(grouped_dfs, function(dfs) {
    do.call(rbind, dfs)
  })

  # Assign numeric names to the combined list
  names(combined_list) <- seq_along(combined_list)

  return(combined_list)
}
