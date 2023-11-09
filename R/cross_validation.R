#' Generate spatio-temporal cross-validation index
#' @param covars stdt. See \link[NRTAPModel]{convert_stobj_to_stdt}
#' for details.
#' @param cv_mode character(1). One of 'lolo (leave-one-location-out)',
#' 'loto (leave-one-time-out)',
#' 'lolto (leave-one-location-time-out)',
#' 'lblo (leave-block-location-out)',
#' 'lbto (leave-block-time-out)',
#' 'lblto (leave-block-location-time-out; not implemented)'
#' 'random (full random selection)'
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @param sp_fold integer(1). Number of subfolds for spatial blocks.
#' @param t_fold integer(1). Number of subfolds for temporal blocks.
#' @param blocks integer(2)/sf/SpatVector object.
#' @param block_id character(1). The unique identifier of each block.
#' @details \code{blocks} is NULL as default
#' (i.e., no block structure is considered); then "lb*" cv_mode
#' is not working. If any meaningful object is assigned here,
#' cv_fold will not make sense and be ignored.
#' Definition of unit grid (horizontal, vertical) size,
#' arbitrary shape of blocks (sf/SpatVector case)
#' where users can consider common spatial hierarchies such as
#' states, counties, hydrological unit code areas, etc.
#' \code{lblto} mode will accept \code{sp_fold} and \code{t_fold}.
#' The maximum of results in this case is \code{sp_fold * t_fold}.
#' @return A numeric vector with length of the number of input rows
#' @author Insang Song
#' @import data.table
#' @import sf
#' @import sftime
#' @import terra
#' @export
generate_cv_index <- function(
  covars,
  cv_mode = c("lolo", "loto", "lolto", "random", "lblo", "lbto", "lblto"),
  cv_fold = 5L,
  sp_fold = NULL,
  t_fold = NULL,
  blocks = NULL,
  block_id = NULL
) {
    # param sp_index character(1). Name of the unique spatial identifier
    # param t_index character(2). Default is 'time'.
    # Name of the unique time identifier

  # type check
  if (!any("stdt" %in% class(covars))) {
    stop("Only stdt object is acceptable. Please consider
    using convert_stobj_to_stdt.\n")
  }
  if (is.null(sp_index)) {
    warning("No sp_index is present in the covars.
    We will make 'sp_index' field based on coordinates...\n")
  }
  cv_mode <- match.arg(cv_mode)

  # block check
  if (!((is.numeric(blocks) & length(blocks) != 2) ||
    methods::is(blocks, "sf") ||
    methods::is(blocks, "SpatVector"))) {
    stop("Inputs for blocks argument are invalid.
    Please revisit the help page of this function
    for the details of proper setting of blocks\n")
  }

  if ((!is.null(sp_fold) || !is.null(t_fold)) && cv_mode != "lblto") {
    stop("sp_fold and t_fold values are only applicable to 
    cv_mode == 'lblto'\n")
  }


  index_cv <- switch(
    cv_mode,
    loto = generate_cv_index_loto(),
    lolto = generate_cv_index_lolto(),
    lblo = generate_cv_index_lblo(),
    lbto = generate_cv_index_lbto(),
    lblto = generate_cv_index_lblto(),
    random = generate_cv_index_random()
  )

  return(index_cv)
}


#' @param covars stdt.
#' @param mode One of "spatial" or "spatiotemporal"
#' @description It generates unique spatiotemporal identifier in the
#' input stdt object.
#' Regardless of mode values
#' (should be one of 'spatial' or 'spatiotemporal'),
#' the index column will be named 'sp_index'.
#' @return stdt with a new index named "sp_index"
#' @author Insang Song
#' @export
#' @noRd
generate_spt_index <- function(
  covars,
  mode = c("spatial", "spatiotemporal")
) {
  mode <- match.arg(mode)
  # generate unique sp_index
  covar_dt <- covars$stdt
  covar_dt[["sp_index"]] <-
    paste0(covar_dt[["long"]], "_", covar_dt[["lat"]])

  if (mode == "spatiotemporal") {
    covar_dt[["sp_index"]] <-
      paste0(covar_dt[["sp_index"]], "_", covar_dt[["time"]])
  }

  covars$stdt <- covar_dt
  return(covars)
}


#' @param covars stdt.
#' @param blocks numeric(2)/sf/SpatVector configuration of blocks.
#' @param block_id character(1). The unique identifier of each block.
#' @details "Block" in this function refers to
#' a group of contiguous spatial/temporal entities.
#' Regardless of mode values
#' (should be one of 'spatial' or 'spatiotemporal'),
#' the index column will be named 'sp_index'.
#' \code{block_id} should be set with a proper field name
#' when blocks is a sf or SpatVector object. When \code{cv_fold} is
#' an integer, then the coordinates stored in \code{stdt} object
#' are clustered with k-means. In this case, the result object
#' will include attributes (accessible with \code{attr} function) about
#' "kmeans_centers" (coordinates of centers) and
#' "kmeans_sizes" (size of each cluster)
#' @return stdt
#' @author Insang Song
#' @importFrom methods is
#' @importFrom stats kmeans
#' @import sf
#' @import sftime
#' @import terra
#' @export
#' @noRd
generate_block_sp_index <- function(
  covars,
  cv_fold = NULL,
  blocks,
  block_id = NULL
) {
  detected_class <- class(blocks)[1]

  if (!is.null(cv_fold)) {
    # deterministic k-fold; yes, it is not elegant,
    # but is readily implemented... now
    coords <- unique(covars[, seq(1, 2)])
    km_index <- stats::kmeans(coords, centers = cv_fold,
      iter.max = 100L)
    km_index_num <- km_index$cluster
    covars$stdt[["sp_index"]] <- km_index_num

    attr(covars, "kmeans_centers") <- km_index$centers
    attr(covars, "kmeans_sizes") <- km_index$size
  }


  if (methods::is(blocks, "sf")
    || methods::is(blocks, "sftime")
    || methods::is(blocks, "SpatVector")) {
    if (is.null(block_id)) {
      stop("block_id must be set for this type of argument blocks.\n")
    }
    if (any(duplicated(unlist(blocks[[block_id]])))) {
      stop("block_id has duplicates.
      Please make sure 'blocks' argument consists of unique identifiers.\n")
    }

    # spatial join
    fun_stjoin <- switch(
      detected_class,
      sf = sf::st_join,
      sftime = sf::st_join,
      SpatVector = terra::intersect
    )

    # to be added (generic convert_stdt_*)
    covars_recov <- convert_stdt(covars, class_to = detected_class)
    covars_recov_id <- fun_stjoin(covars_recov, blocks[, block_id])
    covars$stdt[["sp_index"]] <- unlist(covars_recov_id[[block_id]])
  }

  if (is.numeric(blocks)) {
    step_hor <- blocks[1]
    step_ver <- blocks[2]

    covars_recov_id <- data.table::copy(covars$stdt)
    # 
    x_range <-
      cut(unlist(covars_recov_id[["lon"]]), step_hor)
    y_range <-
      cut(unlist(covars_recov_id[["lat"]]), step_ver)
    xy_range <- paste(
      as.character(x_range),
      as.character(y_range),
      sep = "|"
    )
    xy_range_num <- as.numeric(factor(xy_range))
    covars$stdt[["sp_index"]] <- xy_range_num
  }

  return(covars_recov_id)

}

#' Generate spatio-temporal cross-validation index (leave-one-time-out)
#' @param covars stdt. See \link[NRTAPModel]{convert_stobj_to_stdt}
#' for details.
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @author Insang Song
#' @import data.table
#' @export
#' @noRd
generate_cv_index_loto <- function(
  covars
) {
  origin_ts <- covars$stdt$time
  sorted_ts <- sort(unique(origin_ts))
  cv_index <- replace(origin_ts, sorted_ts, seq_along(sorted_ts))
  return(cv_index)
}

#' Generate spatio-temporal cross-validation index (leave-one-location-out)
#' @param covars stdt. See \link[NRTAPModel]{convert_stobj_to_stdt}
#' for details.
#' @author Insang Song
#' @import data.table
#' @export
#' @noRd
generate_cv_index_lolo <- function(
  covars
) {
  covars_sp_index <- generate_spt_index(covars, mode = "spatial")
  sp_index_origin <- unlist(covars_sp_index[["sp_index"]])
  sp_index_unique <- unique(sp_index_origin)
  cv_index <- replace(
    sp_index_unique,
    sp_index_unique,
    seq_along(sp_index_unique))
  return(cv_index)
}


#' Generate spatio-temporal cross-validation index
#' (leave-one-location-time-out)
#' @param covars stdt. See \link[NRTAPModel]{convert_stobj_to_stdt}
#' for details.
#' @import data.table
#' @export
#' @noRd
generate_cv_index_lolto <- function(
  covars
) {
  rows <- nrow(covars$stdt)
  cv_index <- seq(1, rows)
  return(cv_index)
}


#' Generate spatio-temporal cross-validation index
#' (leave-block-location-out)
#' @param covars stdt. See \link[NRTAPModel]{convert_stobj_to_stdt}
#' for details.
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @param blocks integer(2)/sf/SpatVector object.
#' @param block_id character(1). The unique identifier of each block.
#' @import data.table
#' @export
#' @noRd
generate_cv_index_lblo <- function(
  covars,
  cv_fold = NULL,
  blocks = NULL,
  block_id = NULL
) {
  if (is.null(cv_fold) && !is.null(blocks)) {
    stop("Argument cv_fold cannot be NULL unless
    valid argument for blocks is entered.
    Please set a proper number.\n")
  }
  covars_sp_index <- generate_block_sp_index(
    covars, cv_fold = cv_fold, blocks, block_id
  )
  cv_index <- covars_sp_index$stdt$sp_index
  return(cv_index)

}


#' Generate spatio-temporal cross-validation index
#' (leave-block-time-out)
#' @param covars stdt. See \link[NRTAPModel]{convert_stobj_to_stdt}
#' for details.
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @import data.table
#' @export
#' @noRd
generate_cv_index_lbto <- function(
  covars,
  cv_fold = NULL
) {
  if (is.null(cv_fold)) {
    stop("Argument cv_fold cannot be NULL. Please set a proper number.\n")
  }

  origin_ts <- covars$stdt$time
  sorted_ts <- sort(unique(origin_ts))
  length_ts <- length(sorted_ts)

  if (length_ts > cv_fold) {
    stop(sprintf("The total length of time series in the input is
    shorter than cv_fold.\n
    Length of the input time series: %d\n
    cv_fold: %d\n", length_ts, cv_fold))
  }
  unit_split <- ceiling(length_ts / cv_fold)
  fold_index <- ceiling(seq(1, length_ts) / unit_split)
  cv_index <- replace(origin_ts, sorted_ts, fold_index)
  return(cv_index)
}


#' Generate spatio-temporal cross-validation index
#' (leave-block-location-time-out)
#' @param covars stdt. See \link[NRTAPModel]{convert_stobj_to_stdt}
#' for details.
#' @param sp_fold integer(1). Number of subfolds for spatial blocks.
#' @param t_fold integer(1). Number of subfolds for temporal blocks.
#' @param blocks integer(2)/sf/SpatVector object.
#' @param block_id character(1). The unique identifier of each block.
#' @details The maximum of results is \code{sp_fold * t_fold}.
#' @import data.table
#' @export
#' @noRd
generate_cv_index_lblto <- function(
  covars,
  sp_fold,
  t_fold,
  blocks,
  block_id = NULL
) {
  covars_sp_index <- generate_block_sp_index(
    covars, cv_fold = sp_fold, blocks, block_id
  )
  ts_index <- generate_cv_index_lbto(
    covars, cv_fold = t_fold
  )
  spt_index <- sprintf("S%04d-T%04d",
    covars_sp_index$stdt$sp_index, ts_index)
  cv_index <- as.numeric(factor(spt_index))
  return(cv_index)
}


#' Generate spatio-temporal cross-validation index
#' (random)
#' @param covars stdt. See \link[NRTAPModel]{convert_stobj_to_stdt}
#' for details.
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @return An integer vector with unique values of \code{seq(1, cv_fold)}
#' @export
#' @noRd
generate_cv_index_random <- function(
  covars,
  cv_fold = NULL
) {
  if (is.null(cv_fold)) {
    stop("Argument cv_fold cannot be NULL. Please set a proper number.\n")
  }
  rows <- nrow(covars$stdt)
  cv_index <- sample(seq(1, cv_fold), rows, replace = TRUE)
  return(cv_index)
}