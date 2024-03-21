## Load packages ####
basedir <- file.path(getwd(), "..", "..")
rlibdir <- file.path(basedir, "r-libs")
.libPaths(rlibdir)
pkgs <- c("data.table", "tidymodels")
invisible(sapply(pkgs, library, quietly = TRUE, character.only = TRUE))


## exploring interoperability
source("R/manipulate_spacetime_data.R")
source("R/cross_validation.R")
source("input/Rinput/processing_functions/filter_unique_sites.R")

# Data prep
sitesk <- filter_unique_sites(include_time = FALSE, date_start = "2021-01-01", date_end = "2021-06-30")
sitest <- filter_unique_sites(include_time = TRUE, date_start = "2021-01-01", date_end = "2021-06-30")
colnames(sitest)[4] <- "time"
sitestdt <- convert_stobj_to_stdt(sitest)
sitestdt$crs_stdt <- "OGC:CRS84"

sites_cvi_lblo <- generate_cv_index(sitestdt, "lblo", blocks = c(10, 10))
plot(x = sitestdt$stdt$lon, y = sitestdt$stdt$lat, col = as.factor(sites_cvi_lblo))

## Function will be moved to one of main R files

#' Generate manual rset object from spatiotemporal cross-validation indices
#' @param cvindex integer. Output of [`generate_cv_index`].
#' @param data data.frame from [stdt][`convert_stobj_to_stdt`]. Should be the
#' same object as what was used for `covars` argument of [`generate_cv_index`]
#' @param cv_mode character(1). Spatiotemporal cross-validation indexing method.
#' See `cv_mode` description in [`generate_cv_index`].
#' @returns rset object of `rsample` package. A tibble with a list column of
#' training-test data.frames and a column of labels.
#' @author Insang Song
#' @importFrom rsample make_splits
#' @importFrom rsample manual_rset
#' @export
convert_cv_index_rset <- function(cvindex, data, cv_mode) {
  maxcvi <- max(cvindex)
  len_cvi <- seq_len(maxcvi)
  list_cvi <- split(len_cvi, len_cvi)
  list_cvi_rows <-
    lapply(
      list_cvi,
      function(x) {
        list(analysis = which(cvindex != x),
             assessment = which(cvindex == x))
      }
    )
  list_split_dfs <-
    lapply(
      list_cvi_rows,
      function(x) {
        rsample::make_splits(x = x, data = data)
      }
    )
  modename <- sprintf("cvfold_%s_%03d", cv_mode, len_cvi)
  rset_stcv <- rsample::manual_rset(list_split_dfs, modename)
  return(rset_stcv)

}

convert_cv_index_rset(sites_cvi_lblo, sitestdt$stdt, "lblo") |> str()

rsample::manual_rset()
rsample::make_splits()
rsample::get_rsplit()
rsample::rset_reconstruct()
rsample::populate()
rsample::reverse_splits()
rsample::group_vfold_cv()
rsample::get_rsplit()
rsample::new_rset()
rsample::manual_rset()
checkcv <- rsample::vfold_cv(sitestdt$stdt, strata = sites_cvi_lblo)
checkcv0 <- rsample::vfold_cv(sitestdt$stdt, 10)
?tune::fit_resamples

## y variable
pm25 <- readRDS("./tests/testdata/daily_88101_2018-2022.rds")
pm25e <- pm25 |>
  dplyr::mutate(site_id = sprintf("%02d%03d%04d%05d", State.Code, County.Code, Site.Num, Parameter.Code),
    time = as.character(Date.Local)) |>
  dplyr::group_by(site_id, time) |>
  dplyr::filter(POC == min(POC)) |>
  dplyr::summarize(pm2.5 = mean(Arithmetic.Mean, na.rm = TRUE)) |>
  dplyr::ungroup()
pm25sub <- pm25e |>
  dplyr::mutate(time = as.Date(time)) |>
  dplyr::filter(time >= as.Date("2021-01-01") & time <= as.Date("2021-06-30"))

## covariate join
covarsets <-
  list.files(
    file.path("..", "../../..", "group/set/Projects/NRT-AP-Model", "output"),
    "NRTAP_Covars_*.*.rds", full.names = TRUE)

## Code chunk here is based on songi2 user directory
covarsets <-
  lapply(covarsets[-7], readRDS)
names(covarsets[[7]])[4] <- "time"
covarsets[[7]]$time <- as.character(covarsets[[7]]$time)
covarsets_spt <-
  sapply(covarsets, \(x) sum(grepl("(site_id|time)", names(x))) == 2)
# lapply(covarsetssub, names)
dfcovarst <-
  Reduce(function(x, y) merge(x, y, by = c("site_id", "time")),
         covarsets[covarsets_spt])
dfcovarst <- merge(dfcovarst, pm25sub, by = c("site_id", "time"))

dfcovarstdt <- convert_stobj_to_stdt(dfcovarst)
dfcovarstdt$stdt$time <- as.Date(dfcovarstdt$stdt$time)
# saveRDS(dfcovarstdt, "~/stcv_test_data.rds", compress = "xz")
dfcovarstdt <- readRDS("~/stcv_test_data.rds")
dfcovars_lblto <-
  generate_cv_index(dfcovarstdt, "lblto", blocks = c(30, 15), t_fold = 5L)
dfcovarstdt_cv <-
  convert_cv_index_rset(dfcovars_lblto, dfcovarstdt$stdt, "lblto")


## tidymodel specification
xgb_mod <-
  parsnip::boost_tree(learn_rate = tune::tune()) |>
  set_engine("xgboost", eval_metric = list("rmse", "mae")) |>
  set_mode("regression")

pm25mod <- workflow() |>
  add_model(xgb_mod) |>
  add_formula(pm2.5 ~ .) |>
  tune::tune_bayes(resamples = dfcovarstdt_cv, iter = 50) |>
  fit_resamples(dfcovarstdt_cv, yardstick::metric_set(rmse, mae))

terms0 <- names(dfcovarstdt[[1]])
terms0 <- terms0[5:160]
mlp_mod <-
  parsnip::mlp(
    hidden_units = tune::tune(),
    dropout = tune::tune(),
    learn_rate = tune::tune()
  ) |>
  set_engine("brulee") |>
  set_mode("regression")
mlp_workflow <- workflow() |>
  add_model(mlp_mod) |>
  add_formula(reformulate(response = "pm2.5", termlabels = terms0)) |>
  tune::tune_bayes(resamples = dfcovarstdt_cv, iter = 10) |>
  fit_resamples(dfcovarstdt_cv, yardstick::metric_set(rmse, mae))
