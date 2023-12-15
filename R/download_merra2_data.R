################################################################################
# Date created: 2023-10-12
# Packages required: none
################################################################################

################################################################################
#' download_merra2_data: download meteorological and atmospheric data from the
#' Modern-Era Retrospective analysis for Research and Applications, Version 2
#' (MERRA-2) model.
#' @description
#' The `download_merra2_data()` function accesses and downloads various
#' meteorological and atmospheric collections from the [Modern-Era]
#' [Retrospective analysis for Research and Applications, Version 2 (MERRA-2)]
#' (https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/).
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param collection character(1). MERRA-2 data collection file name.
#' @param directory_to_save character(1). Directory to save data.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). `= FALSE` will generate a `.txt` file containing
#' all download commands. By setting `= TRUE` the function will download all of
#' the requested data files.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_merra2_data <- function(
  date_start = "2023-09-01",
  date_end = "2023-09-01",
  collection = NULL,
  directory_to_save = "../../data/covariates/merra2/",
  data_download_acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 3. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 4. check if collection is recognized
  identifiers <- c("inst1_2d_asm_Nx M2I1NXASM 10.5067/3Z173KIE2TPD",
                   "inst1_2d_int_Nx M2I1NXINT 10.5067/G0U6NGQ3BLE0",
                   "inst1_2d_lfo_Nx M2I1NXLFO 10.5067/RCMZA6TL70BG",
                   "inst3_3d_asm_Np M2I3NPASM 10.5067/QBZ6MG944HW0",
                   "inst3_3d_aer_Nv M2I3NVAER 10.5067/LTVB4GPCOTK2",
                   "inst3_3d_asm_Nv M2I3NVASM 10.5067/WWQSXQ8IVFW8",
                   "inst3_3d_chm_Nv M2I3NVCHM 10.5067/HO9OVZWF3KW2",
                   "inst3_3d_gas_Nv M2I3NVGAS 10.5067/96BUID8HGGX5",
                   "inst3_2d_gas_Nx M2I3NXGAS 10.5067/HNGA0EWW0R09",
                   "inst6_3d_ana_Np M2I6NPANA 10.5067/A7S6XP56VZWS",
                   "inst6_3d_ana_Nv M2I6NVANA 10.5067/IUUF4WB9FT4W",
                   "statD_2d_slv_Nx M2SDNXSLV 10.5067/9SC1VNTWGWV3",
                   "tavg1_2d_adg_Nx M2T1NXADG 10.5067/HM00OHQBHKTP",
                   "tavg1_2d_aer_Nx M2T1NXAER 10.5067/KLICLTZ8EM9D",
                   "tavg1_2d_chm_Nx M2T1NXCHM 10.5067/3RQ5YS674DGQ",
                   "tavg1_2d_csp_Nx M2T1NXCSP 10.5067/H0VVAD8F6MX5",
                   "tavg1_2d_flx_Nx M2T1NXFLX 10.5067/7MCPBJ41Y0K6",
                   "tavg1_2d_int_Nx M2T1NXINT 10.5067/Q5GVUVUIVGO7",
                   "tavg1_2d_lfo_Nx M2T1NXLFO 10.5067/L0T5GEG1NYFA",
                   "tavg1_2d_lnd_Nx M2T1NXLND 10.5067/RKPHT8KC1Y1T",
                   "tavg1_2d_ocn_Nx M2T1NXOCN 10.5067/Y67YQ1L3ZZ4R",
                   "tavg1_2d_rad_Nx M2T1NXRAD 10.5067/Q9QMY5PBNV1T",
                   "tavg1_2d_slv_Nx M2T1NXSLV 10.5067/VJAFPLI1CSIV",
                   "tavg3_3d_mst_Ne M2T3NEMST 10.5067/JRUZ3SJ3ZJ72",
                   "tavg3_3d_trb_Ne M2T3NETRB 10.5067/4I7ZI35QRH8K",
                   "tavg3_3d_nav_Ne M2T3NENAV 10.5067/N5WAKNS1UYQN",
                   "tavg3_3d_cld_Np M2T3NPCLD 10.5067/TX10URJSKT53",
                   "tavg3_3d_mst_Np M2T3NPMST 10.5067/0TUFO90Q2PMS",
                   "tavg3_3d_rad_Np M2T3NPRAD 10.5067/3UGE8WQXZAOK",
                   "tavg3_3d_tdt_Np M2T3NPTDT 10.5067/9NCR9DDDOPFI",
                   "tavg3_3d_trb_Np M2T3NPTRB 10.5067/ZRRJPGWL8AVL",
                   "tavg3_3d_udt_Np M2T3NPUDT 10.5067/CWV0G3PPPWFW",
                   "tavg3_3d_odt_Np M2T3NPODT 10.5067/S0LYTK57786Z",
                   "tavg3_3d_qdt_Np M2T3NPQDT 10.5067/A9KWADY78YHQ",
                   "tavg3_3d_asm_Nv M2T3NVASM 10.5067/SUOQESM06LPK",
                   "tavg3_3d_cld_Nv M2T3NVCLD 10.5067/F9353J0FAHIH",
                   "tavg3_3d_mst_Nv M2T3NVMST 10.5067/ZXTJ28TQR1TR",
                   "tavg3_3d_rad_Nv M2T3NVRAD 10.5067/7GFQKO1T43RW",
                   "tavg3_2d_glc_Nx M2T3NXGLC 10.5067/9ETB4TT5J6US")
  identifiers <- lapply(identifiers, strsplit, split = " ")
  identifiers <- lapply(identifiers, function(x) matrix(x[[1]], nrow = 1))
  identifiers <- do.call(rbind, identifiers)
  identifiers_df <- as.data.frame(identifiers)
  colnames(identifiers_df) <- c("collection_id", "estd_name", "DOI")
  if (!(collection %in% identifiers_df$collection_id)) {
    print(identifiers_df)
    stop(paste0("Requested collection is not recognized.\n
    Please refer to the table above to find a proper collection.\n"))
  }
  #### 5. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 6. define year + month sequence
  yearmonth_sequence <- unique(substr(date_sequence, 1, 6))
  #### 7. define ESDT name and DOI
  identifiers_df_requested <- subset(identifiers_df,
                                     subset =
                                       identifiers_df$collection_id ==
                                       collection)
  esdt_name <- identifiers_df_requested[, 2]
  cat(paste0("Collection: ",
             collection,
             " | ESDT Name: ",
             esdt_name,
             " | DOI: ",
             identifiers_df_requested[, 3],
             "\n"))
  #### 8. define URL base
  #### NOTE: sorted and defined manually according to
  ####       https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/ &
  ####       https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/
  esdt_name_4 <- c("M2I1NXASM", "M2I1NXINT", "M2I1NXLFO", "M2I3NXGAS",
                   "M2SDNXSLV", "M2T1NXADG", "M2T1NXAER", "M2T1NXCHM",
                   "M2T1NXCSP", "M2T1NXFLX", "M2T1NXINT", "M2T1NXLFO",
                   "M2T1NXLND", "M2T1NXOCN", "M2T1NXRAD", "M2T1NXSLV",
                   "M2T3NXGLC")
  esdt_name_5 <- c("M2I3NPASM", "M2I3NVAER", "M2I3NVASM", "M2I3NVCHM",
                   "M2I3NVGAS", "M2I6NPANA", "M2I6NVANA", "M2T3NEMST",
                   "M2T3NENAV", "M2T3NETRB", "M2T3NPCLD", "M2T3NPMST",
                   "M2T3NPODT", "M2T3NPQDT", "M2T3NPRAD", "M2T3NPTDT",
                   "M2T3NPTRB", "M2T3NPUDT", "M2T3NVASM", "M2T3NVCLD",
                   "M2T3NVMST", "M2T3NVRAD")
  if (esdt_name %in% esdt_name_4) {
    base <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/"
  } else if (esdt_name %in% esdt_name_5) {
    base <- "https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/"
  }
  #### 9. identify download URLs
  list_urls <- NULL
  for (y in seq_along(yearmonth_sequence)) {
    year <- substr(yearmonth_sequence[y], 1, 4)
    month <- substr(yearmonth_sequence[y], 5, 6)
    list_urls_month <- system(paste0("wget -q -nH -nd ",
                                     "\"",
                                     base,
                                     esdt_name,
                                     ".5.12.4/",
                                     year,
                                     "/",
                                     month,
                                     "/\"",
                                     " -O - | grep .nc4 | awk -F'\"' ",
                                     "'{print $4}'"),
                              intern = TRUE)
    list_urls <- c(list_urls, list_urls_month)
  }
  #### 10. match list_urls to date sequence
  list_urls_date_sequence <- list_urls[substr(list_urls, 28, 35) %in%
                                         date_sequence]
  #### 11. separate data and metadata
  list_urls_data <- list_urls_date_sequence[grep("*.xml",
                                                 list_urls_date_sequence,
                                                 invert = TRUE)]
  list_urls_metadata <- list_urls_date_sequence[grep("*.xml",
                                                     list_urls_date_sequence,
                                                     invert = FALSE)]
  #### 12. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(directory_to_save,
                         collection,
                         "_",
                         date_start,
                         "_",
                         date_end,
                         "_wget_commands.txt")
  download_sink(commands_txt)
  #### 13. concatenate and print download commands to "..._wget_commands.txt"
  for (l in seq_along(date_sequence)) {
    year <- as.character(substr(date_sequence[l], 1, 4))
    month <- as.character(substr(date_sequence[l], 5, 6))
    download_url <- paste0(base,
                           esdt_name,
                           ".5.12.4/",
                           year,
                           "/",
                           month,
                           "/",
                           list_urls_data[l])
    download_folder <- paste0(directory_to_save,
                              collection)
    download_command <- paste0("wget ",
                               download_url,
                               " -P ",
                               download_folder,
                               "\n")
    cat(download_command)
    download_url_metadata <- paste0(base,
                                    esdt_name,
                                    ".5.12.4/",
                                    year,
                                    "/",
                                    month,
                                    "/",
                                    list_urls_metadata[l])
    download_folder_metadata <- paste0(directory_to_save,
                                       collection,
                                       "/metadata/")
    download_command_metadata <- paste0("wget ",
                                        download_url_metadata,
                                        " -P ",
                                        download_folder_metadata,
                                        "\n")
    cat(download_command_metadata)
  }
  #### 14. finish "..._wget_commands.txt"
  sink()
  #### 15. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 16. download data
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
  #### 17. Remove command file
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)

}

