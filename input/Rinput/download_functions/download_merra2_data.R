################################################################################
# Date created: 2023-10-12
# Packages required: none
################################################################################

################################################################################
#' download_merra2_data:
#' @description
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param collection character(1).
#' @param directory_to_save character(1). Directory to save data.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @author Mitchell Manware
#' @return NULL;
#' @export
download_merra2_data <- function(
  date_start = "2023-09-01",
  date_end = "2023-09-01",
  collection = NULL,
  directory_to_save = "./input/merra2/raw/",
  data_download_acknowledgement = FALSE
) {
  #### 1. directory setup
  chars_dir_save <- nchar(directory_to_save)
  if (substr(directory_to_save, chars_dir_save, chars_dir_save) != "/") {
    directory_to_save <- paste0(directory_to_save, "/", sep = "")
  }
  if (dir.exists(directory_to_save) == FALSE) {
    dir.create(directory_to_save)
  }
  #### 2. check for data download acknowledgement
  if (data_download_acknowledgement == FALSE) {
    cat(paste0("Data download acknowledgement is set to FALSE. ",
               "Please acknowledge that the data downloaded using this ",
               "function may be very large and use lots of machine storage ",
               "and memory.\n"))
    stop()
  }
  #### 2. check for collection
  if (is.null(collection) == TRUE) {
    cat(paste0("Please select a MERRA2 collection.\n"))
    stop()
  }
  #### 3. check if collection is recognized
  identifiers <- paste0("inst1_2d_asm_Nx M2I1NXASM 10.5067/3Z173KIE2TPD,",
                        "inst1_2d_int_Nx M2I1NXINT 10.5067/G0U6NGQ3BLE0,",
                        "inst1_2d_lfo_Nx M2I1NXLFO 10.5067/RCMZA6TL70BG,",
                        "inst3_3d_asm_Np M2I3NPASM 10.5067/QBZ6MG944HW0,",
                        "inst3_3d_aer_Nv M2I3NVAER 10.5067/LTVB4GPCOTK2,",
                        "inst3_3d_asm_Nv M2I3NVASM 10.5067/WWQSXQ8IVFW8,",
                        "inst3_3d_chm_Nv M2I3NVCHM 10.5067/HO9OVZWF3KW2,",
                        "inst3_3d_gas_Nv M2I3NVGAS 10.5067/96BUID8HGGX5,",
                        "inst3_2d_gas_Nx M2I3NXGAS 10.5067/HNGA0EWW0R09,",
                        "inst6_3d_ana_Np M2I6NPANA 10.5067/A7S6XP56VZWS,",
                        "inst6_3d_ana_Nv M2I6NVANA 10.5067/IUUF4WB9FT4W,",
                        "statD_2d_slv_Nx M2SDNXSLV 10.5067/9SC1VNTWGWV3,",
                        "tavg1_2d_adg_Nx M2T1NXADG 10.5067/HM00OHQBHKTP,",
                        "tavg1_2d_aer_Nx M2T1NXAER 10.5067/KLICLTZ8EM9D,",
                        "tavg1_2d_chm_Nx M2T1NXCHM 10.5067/3RQ5YS674DGQ,",
                        "tavg1_2d_csp_Nx M2T1NXCSP 10.5067/H0VVAD8F6MX5,",
                        "tavg1_2d_flx_Nx M2T1NXFLX 10.5067/7MCPBJ41Y0K6,",
                        "tavg1_2d_int_Nx M2T1NXINT 10.5067/Q5GVUVUIVGO7,",
                        "tavg1_2d_lfo_Nx M2T1NXLFO 10.5067/L0T5GEG1NYFA,",
                        "tavg1_2d_lnd_Nx M2T1NXLND 10.5067/RKPHT8KC1Y1T,",
                        "tavg1_2d_ocn_Nx M2T1NXOCN 10.5067/Y67YQ1L3ZZ4R,",
                        "tavg1_2d_rad_Nx M2T1NXRAD 10.5067/Q9QMY5PBNV1T,",
                        "tavg1_2d_slv_Nx M2T1NXSLV 10.5067/VJAFPLI1CSIV,",
                        "tavg3_3d_mst_Ne M2T3NEMST 10.5067/JRUZ3SJ3ZJ72,",
                        "tavg3_3d_trb_Ne M2T3NETRB 10.5067/4I7ZI35QRH8K,",
                        "tavg3_3d_nav_Ne M2T3NENAV 10.5067/N5WAKNS1UYQN,",
                        "tavg3_3d_cld_Np M2T3NPCLD 10.5067/TX10URJSKT53,",
                        "tavg3_3d_mst_Np M2T3NPMST 10.5067/0TUFO90Q2PMS,",
                        "tavg3_3d_rad_Np M2T3NPRAD 10.5067/3UGE8WQXZAOK,",
                        "tavg3_3d_tdt_Np M2T3NPTDT 10.5067/9NCR9DDDOPFI,",
                        "tavg3_3d_trb_Np M2T3NPTRB 10.5067/ZRRJPGWL8AVL,",
                        "tavg3_3d_udt_Np M2T3NPUDT 10.5067/CWV0G3PPPWFW,",
                        "tavg3_3d_odt_Np M2T3NPODT 10.5067/S0LYTK57786Z,",
                        "tavg3_3d_qdt_Np M2T3NPQDT 10.5067/A9KWADY78YHQ,",
                        "tavg3_3d_asm_Nv M2T3NVASM 10.5067/SUOQESM06LPK,",
                        "tavg3_3d_cld_Nv M2T3NVCLD 10.5067/F9353J0FAHIH,",
                        "tavg3_3d_mst_Nv M2T3NVMST 10.5067/ZXTJ28TQR1TR,",
                        "tavg3_3d_rad_Nv M2T3NVRAD 10.5067/7GFQKO1T43RW,",
                        "tavg3_2d_glc_Nx M2T3NXGLC 10.5067/9ETB4TT5J6US")
  identifiers <- as.vector(strsplit(identifiers, ",")[[1]])
  identifiers_df <- data.frame(NULL)
  for (i in seq_along(identifiers)){
    new <- as.vector(strsplit(identifiers[i], " ")[[1]])
    identifiers_df <- rbind(identifiers_df, new)
  }
  colnames(identifiers_df) <- c("collection_id", "estd_name", "DOI")
  if (!(collection %in% identifiers_df$collection_id)) {
    cat(paste0("Requested collection is not recognized.\n"))
    stop()
  }
  #### 4. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 5. define ESDT name and DOI
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
  #### 6. define URL base
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
  #### 7. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(directory_to_save,
                         collection,
                         "_wget_commands.txt")
  sink(commands_txt)
  #### 8. concatenate and print download commands to "..._wget_commands.txt"
  for (d in seq_along(date_sequence)) {
    date <- date_sequence[d]
    year <- as.character(substr(date, 1, 4))
    month <- as.character(substr(date, 5, 6))
    year_num <- as.numeric(year)
    #### 8.1 define Stream and Version number
    #### NOTE: sorted and defined manually according
    ####       MERRA2 File Specification
    ####       https://gmao.gsfc.nasa.gov/pubs/docs/Bosilovich785.pdf
    if (year_num > 2010) {
      hundreds <- "400"
    } else if (year_num <= 2010 && year_num > 2001) {
      hundreds <- "300"
    } else if (year_num <= 2001 && year_num > 1991) {
      hundreds <- "200"
    } else if (year_num <= 1991) {
      hundreds <- "100"
    }
    download_url <- paste0(base,
                           esdt_name,
                           ".5.12.4/",
                           year,
                           "/",
                           month,
                           "/MERRA2_",
                           hundreds,
                           ".",
                           collection,
                           ".",
                           date,
                           ".nc4")
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
                                    "/MERRA2_",
                                    hundreds,
                                    ".",
                                    collection,
                                    ".",
                                    date,
                                    ".nc4.xml")
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
  #### 9. finish "..._wget_commands.txt"
  sink()
  #### 10. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 11. download data
  system(command = system_command)
  #### 12. remove "..._wget_commands.txt"
  file.remove(commands_txt)
}
