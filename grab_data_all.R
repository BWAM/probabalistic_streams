

#' grab_data_all
#'
#' @param path this is the specified path to your data
#' @param full this is an argument to return the full dataset (TRUE) or as shortened dataset
#' with just the most relevant dataframes.
#'
#' @return 1 data frame for sites table (sites), and 3 lists for other data;
#' Chem_raw_list for chemistry parameters, field_raw_list for habitat, insitu, sample_info and
#' user perception; and macro_raw_list for metrics, method and raw bugs.
#' @export
#'
#' @examples grab_data_all() will grab from the L drive

grab_data_all <- function(path = "L:/DOW/BWAM Share/SMAS/data/cleaned_files",
                      full=TRUE) {
  source("grab_sites_function.R")
  source("grab_chem_function.R")
  source("grab_bugs_function.R")
  source("grab_field_function.R")
  
  cat("(1/4 - master) Running master sites grab.  \n")
  grab_sites()
  cat("(2/4 - master) Running chemistry file grab.  \n")
  grab_chem()
  cat("(3/4 - master) Running bug file grab.  \n")
  grab_bugs()
  cat("(4/4 - master) Running field file grab.  \n")
 grab_field()
  
  cat("DONE! \n")
}

