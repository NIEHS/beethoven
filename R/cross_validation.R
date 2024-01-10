#' Generate spatio-temporal cross-validation index
#' @param covars stdt. See \code{\link{convert_stobj_to_stdt}}
#' for details.
#' @param cv_mode character(1). One of 'lolo (leave-one-location-out)',
#' 'loto (leave-one-time-out)',
#' 'lolto (leave-one-location-time-out)',
#' 'lblo (leave-block-location-out)',
#' 'lbto (leave-block-time-out)',
#' 'lblto (leave-block-location-time-out)'
#' 'random (full random selection)'
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @param sp_fold integer(1). Number of subfolds for spatial blocks.
#' @param t_fold integer(1). Number of subfolds for temporal blocks.
#' @param blocks integer(2)/sf/SpatVector object.
#' @param block_id character(1). The unique identifier of each block.
#' @details \code{blocks} is NULL as default
#' (i.e., no block structure is considered); then "lb*" cv_mode
#' is not working.
#' \code{blocks} should be one of integer vector (length 2),
#' sf, sftime, or SpatVector object.
#' Please be advised that we cannot provide error messages
#' related to the \code{blocks} type invalidity
#' due to the cyclometric complexity.
#' If any valid object is assigned here,
#' cv_fold will not make sense and be ignored.
#' Definition of unit grid (horizontal, vertical) size,
#' arbitrary shape of blocks (sf/SpatVector case)
#' where users can consider common spatial hierarchies such as
#' states, counties, hydrological unit code areas, etc.
#' \code{lblto} mode will accept \code{sp_fold} and \code{t_fold}.
#' The maximum of results in this case is \code{sp_fold * t_fold}.
#' @returns A numeric vector with length of the number of input rows
#' @author Insang Song
#' @export
generate_cv_index <- function(
    covars,
    cv_mode = c("lolo", "loto", "lolto", "random", "lblo", "lbto", "lblto"),
    cv_fold = 5L,
    sp_fold = NULL,
    t_fold = NULL,
    blocks = NULL,
    block_id = NULL) {
  # type check
  if (!any("stdt" %in% class(covars))) {
    stop("Only stdt object is acceptable. Please consider
    using convert_stobj_to_stdt.\n")
  }
  cv_mode <- match.arg(cv_mode)

  # no block check
  # sensible cv_mode and cv_fold
  if (startsWith(cv_mode, "lb") && is.null(cv_fold)) {
    stop("Inputs for blocks argument are invalid.
    Please revisit the help page of this function
    for the details of proper setting of blocks.\n")
  }

  if (cv_mode != "lblto") {
    if ((!is.null(sp_fold) || !is.null(t_fold))) {
      stop("sp_fold and t_fold values are only applicable to
        cv_mode == 'lblto'\n")
    }
  }

  index_cv <- switch(cv_mode,
    lolo = generate_cv_index_lolo(covars),
    loto = generate_cv_index_loto(covars),
    lolto = generate_cv_index_lolto(covars),
    lblo = generate_cv_index_lblo(covars, cv_fold, blocks, block_id),
    lbto = generate_cv_index_lbto(covars, cv_fold),
    lblto = generate_cv_index_lblto(covars,
      sp_fold = sp_fold, t_fold = t_fold,
      blocks = blocks, block_id = block_id
    ),
    random = generate_cv_index_random(covars, cv_fold)
  )

  return(index_cv)
}


#' Generate unique spatiotemporal identifier from stdt object
#' @param covars stdt.
#' @param mode One of "spatial" or "spatiotemporal"
#' @description It generates unique spatiotemporal identifier in the
#' input stdt object. Regardless of mode values
#' (should be one of 'spatial' or 'spatiotemporal'),
#' the index column will be named 'sp_index'.
#' @returns stdt with a new index named "sp_index"
#' @author Insang Song
#' @export
generate_spt_index <- function(
    covars,
    mode = c("spatial", "spatiotemporal")) {
  mode <- match.arg(mode)
  # generate unique sp_index
  covar_dt <- covars$stdt

  if ("sp_index" %in% colnames(covar_dt)) {
    return(covars)
  }
  covar_dt[["sp_index"]] <-
    paste0(covar_dt[["lon"]], "_", covar_dt[["lat"]])

  if (mode == "spatiotemporal") {
    covar_dt[["sp_index"]] <-
      paste0(covar_dt[["sp_index"]], "_", covar_dt[["time"]])
  }

  covars$stdt <- covar_dt
  return(covars)
}

# nolint start
#' Generate blocked spatial index
#' @param covars stdt.
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @param blocks numeric(2)/sf/SpatVector configuration of blocks.
#' @param block_id character(1). The unique identifier of each block.
#' @details "Block" in this function refers to a group of contiguous spatial/temporal entities.
#' Regardless of mode values (should be one of 'spatial' or 'spatiotemporal'),
#' the index column will be named 'sp_index'. \code{block_id} should be set with a proper field name
#' when blocks is a sf or SpatVector object. When \code{cv_fold} is an integer, then the coordinates stored in \code{stdt} object are clustered with k-means. In this case, the result object will include attributes (accessible with \code{attr} function) about
#' "kmeans_centers" (coordinates of centers) and "kmeans_sizes" (size of each cluster)
#' @return stdt
#' @author Insang Song
#' @importFrom methods is
#' @importFrom stats kmeans
#' @importFrom sf st_join
#' @importFrom terra intersect
#' @importFrom data.table .SD
#' @importFrom data.table copy
#' @export
# nolint end
generate_block_sp_index <- function(
    covars,
    cv_fold = NULL,
    blocks = NULL,
    block_id = NULL) {
  detected_class <- class(blocks)[1]

  if (!is.null(cv_fold)) {
    # deterministic k-fold... may be improved later
    coords <- unique(covars$stdt[, .SD, .SDcols = seq(1, 2)])
    coordsm <- data.table::copy(coords)
    coordsm <- as.matrix(coordsm)

    km_index <- stats::kmeans(
      x = coordsm,
      centers = cv_fold,
      iter.max = 20L
    )

    coords$sp_index <- km_index$cluster
    covars$stdt <- data.table::copy(
      data.table::merge.data.table(
        covars$stdt,
        coords,
        by = c("lon", "lat")
      )
    )

    attr(covars, "kmeans_centers") <- km_index$centers
    attr(covars, "kmeans_sizes") <- km_index$size
  }

  if (inherits(blocks, "sf") ||
        inherits(blocks, "sftime") ||
        inherits(blocks, "SpatVector")) {
    if (is.null(block_id)) {
      stop("block_id must be set for this type of argument blocks.\n")
    }
    if (any(duplicated(unlist(blocks[[block_id]])))) {
      stop("block_id has duplicates.
      Please make sure 'blocks' argument consists of unique identifiers.\n")
    }

    # spatial join
    fun_stjoin <- switch(detected_class,
      sf = sf::st_join,
      sftime = sf::st_join,
      SpatVector = terra::intersect
    )

    covars_recov <- convert_stdt(covars, class_to = detected_class)
    covars_recov_id <- fun_stjoin(covars_recov, blocks[, block_id])
    covars$stdt[["sp_index"]] <- unlist(covars_recov_id[[block_id]])
  }

  if (is.numeric(blocks)) {
    step_lon <- blocks[1]
    step_lat <- blocks[2]

    covars_recov_id <- data.table::copy(covars$stdt)

    vlon <- unlist(covars_recov_id[["lon"]])
    vlat <- unlist(covars_recov_id[["lat"]])
    vlon_cuts <- seq(min(vlon), max(vlon), step_lon)
    vlat_cuts <- seq(min(vlat), max(vlat), step_lat)

    x_range <-
      cut(vlon, vlon_cuts, include.lowest = TRUE)
    y_range <-
      cut(vlat, vlat_cuts, include.lowest = TRUE)
    xy_range <- paste(
      as.character(x_range),
      as.character(y_range),
      sep = "|"
    )
    xy_range_num <- as.numeric(factor(xy_range))
    covars$stdt[["sp_index"]] <- xy_range_num
  }

  return(covars)
}

#' Generate spatio-temporal cross-validation index (leave-one-time-out)
#' @param covars stdt. See \code{\link{convert_stobj_to_stdt}} for details.
#' @author Insang Song
#' @return An integer vector.
#' @export
generate_cv_index_loto <-
  function(
    covars
  ) {
    origin_ts <- covars$stdt$time
    sorted_ts <- sort(unique(origin_ts))
    cv_index <- as.numeric(factor(origin_ts, levels = sorted_ts))
    return(cv_index)
  }

#' Generate spatio-temporal cross-validation index (leave-one-location-out)
#' @param covars stdt. See \code{\link{convert_stobj_to_stdt}} for details.
#' @author Insang Song
#' @return An integer vector.
#' @export
generate_cv_index_lolo <-
  function(
    covars
  ) {
    covars_sp_index <- generate_spt_index(covars, mode = "spatial")
    sp_index_origin <- unlist(covars_sp_index$stdt[["sp_index"]])
    sp_index_unique <- sort(unique(sp_index_origin))
    cv_index <- as.numeric(factor(sp_index_origin, levels = sp_index_unique))
    return(cv_index)
  }


#' Generate spatio-temporal cross-validation index leave-one-location-time-out)
#' @param covars stdt. See \code{\link{convert_stobj_to_stdt}} for details.
#' @author Insang Song
#' @return An integer vector.
#' @export
generate_cv_index_lolto <-
  function(
    covars
  ) {
    rows <- nrow(covars$stdt)
    cv_index <- seq(1, rows)
    return(cv_index)
  }

# nolint start
#' Generate spatio-temporal cross-validation index (leave-block-location-out)
#' @param covars stdt. See \code{\link{convert_stobj_to_stdt}} for details.
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @param blocks integer(2)/sf/SpatVector object.
#' @param block_id character(1). The unique identifier of each block.
#' @author Insang Song
#' @return An integer vector.
#' @export
# nolint end
generate_cv_index_lblo <-
  function(
    covars,
    cv_fold = NULL,
    blocks = NULL,
    block_id = NULL
  ) {
    if (is.null(cv_fold) && is.null(blocks)) {
      stop("Argument cv_fold cannot be NULL unless
      valid argument for blocks is entered.
      Please set a proper number.\n")
    }
    covars_sp_index <- generate_block_sp_index(
      covars,
      cv_fold = cv_fold, blocks, block_id
    )

    cv_index <- covars_sp_index$stdt$sp_index

    # if cv_index is character (when block vector is entered)
    # convert cv_index to factor
    if (is.character(cv_index)) {
      cv_index <- factor(cv_index)
    }
    return(cv_index)
  }

# nolint start
#' Generate spatio-temporal cross-validation index (leave-block-time-out)
#' @param covars stdt. See \code{\link{convert_stobj_to_stdt}} for details.
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @author Insang Song
#' @return An integer vector.
#' @export
# nolint end
generate_cv_index_lbto <- function(
    covars,
    cv_fold = NULL) {
  if (is.null(cv_fold)) {
    stop("Argument cv_fold cannot be NULL. Please set a proper number.\n")
  }

  origin_ts <- covars$stdt$time
  origin_ts_min <- min(origin_ts)
  origin_ts_diff <- as.integer(origin_ts - origin_ts_min)

  # We assume that we use daily data...
  # Since POSIX* is based on seconds, we divide
  # 86400 (1 day) from diff
  if (startsWith(class(origin_ts_min)[1], "POSIX")) {
    origin_ts_diff <- origin_ts_diff / 86400
  }
  origin_ts_diff <- origin_ts_diff + 1
  sorted_ts <- sort(unique(origin_ts))
  length_ts <- length(sorted_ts)

  if (length_ts < cv_fold) {
    stop(sprintf("The total length of time series in the input is
    shorter than cv_fold.\n
    Length of the input time series: %d\n
    cv_fold: %d\n", length_ts, cv_fold))
  }
  unit_split <- ceiling(length_ts / cv_fold)
  cv_index <- ceiling(origin_ts_diff / unit_split)
  return(cv_index)
}

# nolint start
#' Generate spatio-temporal cross-validation index (leave-block-location-time-out)
#' @param covars stdt. See \code{\link{convert_stobj_to_stdt}} for details.
#' @param sp_fold integer(1). Number of subfolds for spatial blocks.
#' @param t_fold integer(1). Number of subfolds for temporal blocks.
#' @param blocks integer(2)/sf/SpatVector object.
#' @param block_id character(1). The unique identifier of each block.
#' @author Insang Song
#' @return An integer vector.
#' @details The maximum of results is \code{sp_fold * t_fold}.
#' @export
# nolint end
generate_cv_index_lblto <- function(
    covars,
    sp_fold,
    t_fold,
    blocks,
    block_id = NULL) {
  covars_sp_index <- generate_block_sp_index(
    covars,
    cv_fold = sp_fold, blocks, block_id
  )
  ts_index <- generate_cv_index_lbto(
    covars,
    cv_fold = t_fold
  )
  spt_index <- sprintf(
    "S%04d-T%04d",
    covars_sp_index$stdt$sp_index, ts_index
  )
  cv_index <- as.numeric(factor(spt_index))
  return(cv_index)
}


#' Generate spatio-temporal cross-validation index (random)
#' @param covars stdt. See \code{\link{convert_stobj_to_stdt}} for details.
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @author Insang Song
#' @return An integer vector with unique values of \code{seq(1, cv_fold)}
#' @export
generate_cv_index_random <- function(
    covars,
    cv_fold = NULL) {
  if (is.null(cv_fold)) {
    stop("Argument cv_fold cannot be NULL. Please set a proper number.\n")
  }
  rows <- nrow(covars$stdt)
  cv_index <- sample(seq(1, cv_fold), rows, replace = TRUE)
  return(cv_index)
}
