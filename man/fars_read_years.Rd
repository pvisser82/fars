% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Read the month and year data from the CSV file}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A list or vector containing the years to search}
}
\value{
Returns a list of data frames containing MONTH and year columns based on the "accident_<year>.csv.bz2 files
Returns NULL if the year does not exist
}
\description{
This function uses a vector or list of years and returns a list of data frames containing the month and years for each
accident in the corresponding CSV file. It will display a warning and return NULL if the year does not exist
}
\examples{
\dontrun{
fars_read_years(c(2014, 2015, 2016))
fars_read_years(2014:2016)
fars_read_years(list(2014, 2015, 2016))
}

}
