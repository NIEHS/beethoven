library(amadeus)

aqs <- amadeus::process_aqs()

# nlcd ####
nlcd19 <- amadeus::process_nlcd("/ddn/gs1/group/set/Projects/NRT-AP-Model/input/nlcd/raw", year = 2019)
nlcd21 <- amadeus::process_nlcd("/ddn/gs1/group/set/Projects/NRT-AP-Model/input/nlcd/raw", year = 2021)



nlcd19e_1h <- calc_nlcd_ratio(aqs, nlcd19, radius = c(100))
nlcd19e_1k <- calc_nlcd_ratio(aqs, nlcd19, radius = c(1000))
nlcd19e_10k <- calc_nlcd_ratio(aqs, nlcd19, radius = c(10000))
nlcd21e_1h <- calc_nlcd_ratio(aqs, nlcd21, radius = c(100))
nlcd21e_1k <- calc_nlcd_ratio(aqs, nlcd21, radius = c(1000))
nlcd21e_10k <- calc_nlcd_ratio(aqs, nlcd21, radius = c(10000))


nlcd19e <- Reduce(
    function(x, y) merge(as.data.frame(x), as.data.frame(y)),
    list(nlcd19e_1h, nlcd19e_1k, nlcd19e_10k)
    )
nlcd21e <- Reduce(
    function(x, y) merge(as.data.frame(x), as.data.frame(y)),
    list(nlcd21e_1h, nlcd21e_1k, nlcd21e_10k)
    )

aqsl <- lapply(
    seq(2018, 2022),
    function(x) {
        aq <- as.data.frame(aqs)
        aq$time <- x
        if (x <= 2020) {
            ne <- nlcd19e
        } else {
            ne <- nlcd21e
        }
        ne$time <- x
        aqne <- merge(aq, ne)
        return(aqne)
    }
)

aqsldf <- data.table::rbindlist(aqsl)
saveRDS(aqsldf, "~/covar_nlcd.rds", compress = "xz")

# tri ####
tris <-
  split(seq(2018, 2022), seq(2018, 2022)) |>
  lapply(function(x) {
    process_tri("~/projects/beethoven/input/tri/", year = x)
  })

trisd <- lapply(tris,
  function(x) {
    calc_tri(locs = aqs, from = x)
  })

trisdy <- mapply(
  function(x, y) {
    x$time <- y
    return(x)
  }, trisd, seq(2018, 2022), SIMPLIFY = FALSE)

# write a function that computes tri covariates

# trisdf <- do.call(rbind, trisd)
trisdf <- data.table::rbindlist(trisdy, use.names = TRUE, fill = TRUE)
names(trisdf) <- sub("STACK_AIR", "EMI_STACK", names(trisdf))
names(trisdf) <- sub("FUGITIVE_AIR", "EMI_FUGTV", names(trisdf))
trisdf <- trisdf |> tidytable::select(site_id, time, contains("EMI_STACK"), contains("EMI_FUGTV"))
saveRDS(trisdf, "~/covar_tri.rds", compress = "xz")

dirset <- read.csv("~/.songi2init")
dirset$val[5]
saveRDS(trisdf, file.path(dirset$val[5], "output", "covar_tri.rds"), compress = "xz")
