% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_economic_indicator_files}
\alias{read_economic_indicator_files}
\title{Read many economic indicator files}
\usage{
read_economic_indicator_files(filenames)
}
\arguments{
\item{filenames}{vector of file names}
}
\value{
list of: all_df (list of all tibbles) and all_meta (tibble with meta information)
}
\description{
Read many economic indicator files
}
\details{
Read many economic indicator files
}
\examples{
filenames <- "~/Documents/inflation_platon/data/Data/Economic Indicator_Russia Consumer Prices, Core CPI, Total, Index, DecPY=100_30 Nov 2017.xlsx"
# two_df <- read_economic_indicator(filenames)
}
