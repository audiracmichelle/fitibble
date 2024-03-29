% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaries.R
\name{prep_daily_data}
\alias{prep_daily_data}
\title{prep_daily_data}
\usage{
prep_daily_data(.data, nonvalid = F, sedentary_level = c(sedentary = "0"))
}
\arguments{
\item{.data}{a fitibble.}

\item{nonvalid}{determines if nonvalid wear should be cropped instead of valid wear.}

\item{sedentary_level}{named vector of length one indicating the intensity value and name of the PA intensity level that represents the sedentary behavior.}
}
\value{
a tibble containing daily statistics of valid wear for each date and subject \code{id} in \code{.data}.
}
\description{
prep_daily_data
}
\details{
Prepares daily data derived from valid wear in a fitibble.
}
