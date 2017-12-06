#' Read economic indicator file type
#'
#' Read economic indicator file type
#'
#' Read economic indicator file type
#'
#' @param filename name of file
#' @return list of two tibbles: with data and meta information
#' @export
#' @examples
#' filename <- "~/Documents/inflation_platon/data/Data/Economic Indicator_Russia Consumer Prices, Core CPI, Total, Index, DecPY=100_30 Nov 2017.xlsx"
#' # df_list <- read_economic_indicator(filename)
read_economic_indicator <- function(filename) {
  df <- readxl::read_excel(filename, skip = 8)
  if (!colnames(df)[1] == "Period") {
    warning("First column name is not equal to 'Period'. Maybe read_excel(file_name, skip = 8) failed :(")
  }
  meta_info <- readxl::read_excel(filename, range = "B2:B4", col_names = FALSE)
  meta_df <- tibble::as_tibble(t(meta_info))
  colnames(meta_df) <- c("indicator", "download_date", "unit")
  meta_df$first_name <- colnames(df)[2]
  meta_df$n_series <- ncol(df) - 1
  meta_df$n_obs <- nrow(df)
  if (meta_df$n_obs > 1) {
    meta_df$diff_days <- as.numeric(df$Period[1] - df$Period[2])
  } else {
    meta_df$diff_days <- NA
  }
  meta_df$filename <- filename
  return(list(df, meta_df))
}



#' Read many economic indicator files
#'
#' Read many economic indicator files
#'
#' Read many economic indicator files
#'
#' @param filenames vector of file names
#' @return list of: all_df (list of all tibbles) and all_meta (tibble with meta information)
#' @export
#' @examples
#' filenames <- "~/Documents/inflation_platon/data/Data/Economic Indicator_Russia Consumer Prices, Core CPI, Total, Index, DecPY=100_30 Nov 2017.xlsx"
#' # df_list <- read_economic_indicator(filenames)
read_economic_indicator_files <- function(filenames) {
  all_df <- NULL
  all_meta <- NULL

  for (filename in filenames) {
    message("Processing file:")
    message(filename)
    message("-----------------------")

    two_df <- read_economic_indicator(filename)
    all_df <- append(all_df, list(two_df[[1]]))
    all_meta <- dplyr::bind_rows(all_meta, two_df[[2]])
  }

  all_meta <- dplyr::mutate(all_meta,
      freq = ifelse(diff_days %in% 1:2, 365,
               ifelse(diff_days %in% 5:7, 52,
                 ifelse(diff_days %in% 27:31, 12,
                   ifelse(diff_days %in% 88:93, 4, NA)))))

  return(list(all_df, all_meta))
}



#' Extract and join all data frames with given frequency
#'
#' Extract and join all data frames with given frequency
#'
#' Extract and join all data frames with given frequency
#'
#' @param all_data all_data provided by `read_economic_indicator_files`
#' @param freq frequency to extract
#' @return joined tibble of all series
#' @export
#' @examples
#' filenames <- "~/Documents/inflation_platon/data/Data/Economic Indicator_Russia Consumer Prices, Core CPI, Total, Index, DecPY=100_30 Nov 2017.xlsx"
#' # all_data <- read_economic_indicator_files(filenames)
#' # df_monthly <- extract_by_frequency(all_data, freq = 12)
extract_by_frequency <- function(all_data, freq = 12) {
  all_meta <- all_data[[2]]
  tibble_index <- all_meta$freq == freq
  tibble_index[is.na(tibble_index)] <- FALSE
  df_with_freq <- all_data[[1]][tibble_index]

  joined_df <- df_with_freq[[1]]
  for (df_no in 2:length(df_with_freq)) {
    joined_df <- dplyr::full_join(joined_df, df_with_freq[[df_no]], by = "Period")
  }
  return(joined_df)
}


