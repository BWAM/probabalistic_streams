

#' grab_data
#'
#' @param path this is the specified path to your data
#'
#' @return 1 data frame for sites table (sites), and 3 lists for other data;
#' Chem_raw_list for chemistry parameters, field_raw_list for habitat, insitu, sample_info and
#' user perception; and macro_raw_list for metrics, method and raw bugs.
#' @export
#'
#' @examples grab_data() will grab from the L drive

grab_data <- function(path = "L:/DOW/BWAM Share/SMAS/data/cleaned_files") {

  db_path <- file.path(path)

  sites_path <- file.path(db_path,
                          "Final_Sites_ITS")

  cat("(1/4) Grabbing master sites file.  \n")
  # Get the file paths for the filenames with the prefix "MASTER" and
  # extension CSV.
  sites_csv_list <- list.files(path = sites_path,
                               pattern = "Master(.+?)csv",
                               full.names = TRUE)
  # Identify the appropriate name for each file path.
  sites_csv_names <- ifelse(
    test = grepl("Master_S_Site", sites_csv_list),
    yes = "sites",
    no = "ERROR"
  )
  # Assign easy to reference names to filepaths.
  names(sites_csv_list) <- sites_csv_names
  # Reading in sites data -------------------------------------------------
  ## Loop through CSV list, import data, store in a list.
  sites_raw_list <- lapply(sites_csv_list, function(file_i) {
    # Import data
    read.csv(
      file_i,
      na.strings = c("", "NA"),
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
  })
  sites <- sites_raw_list$sites
  rm(sites_raw_list)


  ## ----get-chemistry-and-pcode-data---------------------------------------------------------------------
  chem_path <- file.path(db_path,
                         "Final_Chemistry_ITS")
  cat("(2/4) Grabbing chemistry files. \n")
  # Get the file paths for the filenames with the prefix "MASTER" and
  # extension CSV.
  chem_csv_list <- list.files(path = chem_path,
                              pattern = "MASTER(.+?)csv",
                              full.names = TRUE)
  # Identify the appropriate name for each file path.
  chem_csv_names <- dplyr::case_when(
    grepl("RESULT", chem_csv_list) ~ "result",
    grepl("SAMPLE", chem_csv_list) ~ "sample",
    grepl("PARAMETER", chem_csv_list) ~ "pcode",
    grepl("QUALIFIER_LAB", chem_csv_list) ~ "lab_qualifiers",
    grepl("VALIDATOR", chem_csv_list) ~ "qualifier_validator"
    TRUE ~ "ERROR"
  )
  # Assign easy to reference names to filepaths.
  names(chem_csv_list) <- chem_csv_names
  # Reading in macro data -------------------------------------------------
  # Loop through CSV list, import data, store in a list.
  chem_raw_list <- lapply(chem_csv_list, function(file_i) {
    # Import data
    read.csv(file_i,
             na.strings = c("", "NA"),
             stringsAsFactors = FALSE)
  })
  # Join chem Data ----------------------------------------------------------

  chem_raw_list$chem_all <-
    merge(
      chem_raw_list$result,
      chem_raw_list$sample,
      by.x = c("CHR_SYS_SAMPLE_CDE", "CHR_SAMPLE_DEL_GRP"),
      by.y = c("CHS_SYS_SAMPLE_CDE", "CHS_SAMPLE_DEL_GRP")
    )

  chem_raw_list$chem_all <- chem_raw_list$chem_all %>%
    subset(startsWith(CHS_DEC_SAMPLE_TYPE_CDE, "N")) %>%
    subset(CHS_SAMPLE_SOURCE == "Field") %>%
    subset(CHR_RESULT_TYPE_CDE %in% "TRG")

  #change both to numeric
  chem_raw_list$pcode$pcode.num <-
    as.numeric(chem_raw_list$pcode$CHEM_PARAMETER_PCODE)

  #merge pcode and chemistry
  chem_raw_list$chem_final <-
    merge(
      chem_raw_list$chem_all,
      chem_raw_list$pcode,
      by.x = "CHR_PCODE",
      by.y = "pcode.num",
      all.x = TRUE
    ) %>%
    #filter out lab pH, lab temperature, and lab specific conductance
    dplyr::filter(!(CHR_PCODE %in% c(110, 136, 139, 143, 145))) %>%
    dplyr::filter(CHR_VALIDATOR_QUAL != "R")

  ## ----get-event-table----------------------------------------------------------------------------------
  field_path <- file.path(db_path,
                          "Final_SBU_Field_ITS")
  cat("(3/4) Grabbing field files. \n")

  # Get the file paths for the filenames with the prefix "MASTER" and
  # extension CSV.
  field_csv_list <- list.files(path = field_path,
                               pattern = "(.+?)csv",
                               full.names = TRUE)
  # Identify the appropriate name for each file path.
  field_csv_names <- dplyr::case_when(
    grepl("User_Perception", field_csv_list) ~ "userp",
    grepl("Habitat", field_csv_list) ~ "habitat",
    grepl("IN_SITU", field_csv_list) ~ "insitu",
    grepl("Sample_Event", field_csv_list) ~ "sample_info",
    grepl("Macro_field", field_csv_list) ~ "macro_field",
    TRUE ~ "ERROR"
  )
  # Assign easy to reference names to filepaths.
  names(field_csv_list) <- field_csv_names
  # Reading in macro data -------------------------------------------------
  ## Loop through CSV list, import data, store in a list.
  field_raw_list <- lapply(field_csv_list, function(file_i) {
    # Import data
    read.csv(
      file_i,
      na.strings = c("", "NA"),
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
  })

  # merge insitu and pcode
  field_raw_list$insitu$pcode.num <-
    as.numeric(field_raw_list$insitu$ISWC_CHEM_PARAMETER_PCODE_VALID)
  field_raw_list$insitu <-
    merge(field_raw_list$insitu,
          chem_raw_list$pcode,
          by = "pcode.num",
          all.x = TRUE)

  ## ----get-macro-data-----------------------------------------------------------------------------------
  # read in data that have the "master" tag
  cat("(4/4) Grabbing bug files.")
  macro_path <- file.path(db_path,
                          "Final_Macro_ITS")
  # Get the file paths for the filenames with the prefix "MASTER" and
  # extension CSV.
  macro_csv_list <- list.files(path = macro_path,
                               pattern = "MASTER(.+?)csv",
                               full.names = TRUE)
  # Identify the appropriate name for each file path.
  macro_csv_names <- dplyr::case_when(
    grepl("METRICS", macro_csv_list) ~ "metrics",
    grepl("SPECIES_SAMP_INF", macro_csv_list) ~ "bug_method",
    grepl("SPECIES_DATA_HISTORY", macro_csv_list) ~ "raw_bugs",
    TRUE ~ "ERROR"
  )
  # Assign easy to reference names to filepaths.
  names(macro_csv_list) <- macro_csv_names
  # Reading in macro data -------------------------------------------------
  ## Loop through CSV list, import data, store in a list.
  macro_raw_list <- lapply(macro_csv_list, function(file_i) {
    # Import data
    read.csv(file_i,
             na.strings = c("", "NA"),
             stringsAsFactors = FALSE)
  })

  # Join Macro Data ----------------------------------------------------------
  macro_raw_list$metrics_all <- merge(
    x = macro_raw_list$metrics,
    y = macro_raw_list$bug_method,
    by.x = "MMDH_LINKED_ID_VALIDATOR",
    by.y = "MSSIH_LINKED_ID_VALIDATOR"
  )

  macro_raw_list$bugs_raw_all <- merge(
    x = macro_raw_list$raw_bugs,
    y = macro_raw_list$bug_method,
    by.x = "MSDH_LINKED_ID_VALIDATOR",
    by.y = "MSSIH_LINKED_ID_VALIDATOR"
  )

  cat("Updating date columns for standard formats. \n")
  ## ----change-date-formats------------------------------------------------------------------------------
  # change date on the final list objects to use
  field_raw_list$insitu$ISWC_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(field_raw_list$insitu$ISWC_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
  macro_raw_list$metrics_all$MSSIH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(macro_raw_list$metrics_all$MSSIH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
  chem_raw_list$chem_final$CHS_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(chem_raw_list$chem_final$CHS_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
  field_raw_list$sample_info$SEIH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(field_raw_list$sample_info$SEIH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
  field_raw_list$userp$UPFDH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(field_raw_list$userp$UPFDH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
  macro_raw_list$bug_method$MSSIH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(macro_raw_list$bug_method$MSSIH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
  field_raw_list$habitat$HFDH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(field_raw_list$habitat$HFDH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")

  .GlobalEnv$field_raw_list <- field_raw_list
  .GlobalEnv$metrics <- macro_raw_list$metrics
  .GlobalEnv$chem<-chem_raw_list$chem_final
  .GlobalEnv$pcode <- chem_raw_list$pcode
  .GlobalEnv$sites <- sites

  cat("DONE! \n")
}

