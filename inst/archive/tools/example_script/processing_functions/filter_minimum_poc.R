
################################################################################
# Date modified: 2023-09-13
# Script description: this function filters the input data.table or tibble 
#         object using POC (parameter occurrence code) to return data with 
#         the minimum POC value in each site. It assumes the daily datasets
#         per year were concatenated in advance.
# Packages required: pacman, data.table, dplyr, rlang, tidytable
################################################################################

################################################################################
#' filter_minimum_poc: filter monitors with the minimum POC value
#'
#' @param input_df data.frame/tbl_df/data.table
#' @param site_id character(1). Name of site id (not monitor id)
#' @param poc_name character(1). Name of column containing POC values.
#' @author Insang Song
#' @return a data.table object
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' @importFrom data.table data.table
#' @importFrom rlang sym
#' @export
filter_minimum_poc <- function(input_df, site_id, poc_name) {
  #nocov start
  if (!is(input_df, "data.frame")) {
    stop("input_df should be data.frame/tbl_df/data.table.\n")
  }
  if (!is.character(site_id)) {
    stop("site_id should be character.\n")
  }
  if (!is.character(poc_name)) {
    stop("poc_name should be character.\n")
  }

  poc_filtered <- input_df |>
    dplyr::group_by(!!rlang::sym(site_id)) |>
    dplyr::filter(!!rlang::sym(poc_name) == min(!!rlang::sym(poc_name))) |>
    dplyr::ungroup() |>
    data.table::data.table()
  return(poc_filtered)
  #nocov end
}

