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

#' Extract cell with special text from vector
#'
#' Extract cell with special text from vector
#'
#' Extract cell with special text from vector
#'
#' @param first_col character vector
#' @param property_text character
#' @return extracted value
#' @export
#' @examples
#' first_col <- c("Sex: male", "Interval: A", "Group: B")
#' extract_property(first_col, "Group: ")
extract_property <- function(first_col, property_text = "Interval: ") {
  property_cell <- stringr::str_subset(first_col, property_text)
  n_cells <- length(property_cell)
  if (n_cells == 1) {
    property <- stringr::str_match(property_cell, paste0(property_text, "([a-zA-Z0-9 ]+)"))[, 2]
  } else if (n_cells == 0) {
    property <- NA
  } else {
    property <- stringr::str_match(property_cell[1], paste0(property_text, "([a-zA-Z0-9 ]+)"))[, 2]
    warning("More than one cell with required text found.")
  }
}



#' Read price history file type
#'
#' Read price history file type
#'
#' Read price history file type
#'
#' @param filename name of file
#' @return list of two tibbles: with data and meta information
#' @export
#' @examples
#' filename <- "~/Documents/inflation_platon/data/Data2/Price History [29].xlsx"
#' # two_df <- read_price_history(filename)
read_price_history <- function(filename) {
  first_col_df <- readxl::read_excel(filename, range = "A1:A50", col_names = FALSE)
  first_col <- first_col_df$X__1

  n_skip_lines <- stringr::str_which(first_col, "Exchange Date") - 1

  df <- readxl::read_excel(filename, skip = n_skip_lines)

  meta_df <- tibble::tibble(name_long = first_col[1],
                    name_short = first_col[3])

  meta_df$interval <- extract_property(first_col, "Interval: ")
  meta_df$period <- extract_property(first_col, "Period: ")
  meta_df$conversion <- extract_property(first_col, "Conversion: ")

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
#' # two_df <- read_economic_indicator(filenames)
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


